--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Request;

package body CBE.Cache
with Spark_Mode
is

	package body Cache_Item
	with Spark_Mode
	is

		--
		-- Unused_Object
		--
		function Unused_Object
		return Cache_Item_Type
		is (
			Pba   => 0,
			Ts    => 0,
			State => Unused);

		--
		-- Initialize_Object
		--
		procedure Initialize_Object(
			Obj : out Cache_Item_Type;
			Pba : Physical_Block_Address_Type;
			Ts  : Timestamp)
		is
		begin
			State(Obj, Sta => Used);
			Obj.Pba := Pba;
			Obj.Ts  := Ts;
		end Initialize_Object;

		---------------
		-- Accessors --
		---------------

		function Unused (Obj : Cache_Item_Type) return Boolean is (Obj.State = Unused);
		function Used   (Obj : Cache_Item_Type) return Boolean is (Obj.State = Used);

		function Pba (Obj : Cache_Item_Type) return Physical_Block_Address_Type is (Obj.Pba);
		function Ts  (Obj : Cache_Item_Type) return Timestamp is (Obj.Ts);

		procedure State(
			Obj : in out Cache_Item_Type;
			Sta :        State_Type)
		is
		begin
			Obj.State := Sta;
		end State;

		procedure Set_Ts(
			Obj : in out Cache_Item_Type;
			Ts  :        Timestamp)
		is
		begin
			Obj.Ts := Ts;
		end Set_Ts;
	end Cache_Item;


	package body Job_Item
	with Spark_Mode
	is

		--
		-- Initialize_Object
		--
		procedure Initialize_Object(
			Obj : out Job_Item_Type;
			Pba : Physical_Block_Address_Type)
		is
		begin
			State(Obj, Sta => Pending);
			Obj.Success := False;
			Obj.Pba := Pba;
		end Initialize_Object;

		--
		-- Unused_Object
		--
		function Unused_Object
		return Job_Item_Type
		is (
			Pba     => 0,
			State   => Unused,
			Success => False);

		--
		-- Already_Pending
		--
		function Already_Pending(
			Obj : Job_Item_Type;
			Pba : Physical_Block_Address_Type)
		return Boolean
		is
		begin
			if Obj.State = Pending and Obj.Pba = Pba then
				return True;
			end if;
			return False;
		end Already_Pending;

		---------------
		-- Accessors --
		---------------

		function Unused      (Obj : Job_Item_Type) return Boolean is (Obj.State = Unused);
		function Pending     (Obj : Job_Item_Type) return Boolean is (Obj.State = Pending);
		function In_Progress (Obj : Job_Item_Type) return Boolean is (Obj.State = In_Progress);
		function Complete    (Obj : Job_Item_Type) return Boolean is (Obj.State = Complete);

		function Pba     (Obj : Job_Item_Type) return Physical_Block_Address_Type is (Obj.Pba);
		function Success (Obj : Job_Item_Type) return Boolean is (Obj.Success);

		procedure State(
			Obj : in out Job_Item_Type;
			Sta :        State_Type)
		is
		begin
			Obj.State := Sta;
		end State;

		procedure Set_Unused(Obj : in out Job_Item_Type)
		is
		begin
			Obj.State := Unused;
		end Set_Unused;

		procedure Set_Success(
			Obj : in out Job_Item_Type;
			Suc :        Boolean)
		is
		begin
			Obj.Success := Suc;
		end Set_Success;
	end Job_Item;


	--
	-- Get_Cache_Slot
	--
	function Get_Cache_Slot(Obj : Object_Type)
	return Cache_Index_Type
	is
		Cache_Id : Cache_Index_Type := Cache_Index_Type'Last;
		Min_Used : Timestamp := Timestamp'Last;
	begin
		Unused_Slot: for Cache_Item_Id in Obj.Cache_Items'Range loop
			if Cache_Item.Unused(Obj.Cache_Items(Cache_Item_Id)) then
				return Cache_Item_Id;
			end if;
		end loop Unused_Slot;

		Evict_Slot: for Cache_Item_Id in Obj.Cache_Items'Range loop
			if Cache_Item.Used(Obj.Cache_Items(Cache_Item_Id)) then
				if Min_Used > Cache_Item.Ts(Obj.Cache_Items(Cache_Item_Id)) then
					Cache_Id := Cache_Item_Id;
					Min_Used := Cache_Item.Ts(Obj.Cache_Items(Cache_Item_Id));
				end if;
			end if;
		end loop Evict_Slot;

		return Cache_Id;
	end Get_Cache_Slot;

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(Obj : out Object_Type)
	is
	begin
		for Item_Id in Obj.Cache_Items'Range loop
			Obj.Cache_Items(Item_Id) := Cache_Item.Unused_Object;
		end loop;

		for Item_Id in Obj.Job_Items'Range loop
			Obj.Job_Items(Item_Id) := Job_Item.Unused_Object;
		end loop;

		Obj.Active_Jobs := 0;
	end Initialize_Object;

	--
	-- Data_Available
	--
	function Data_Available(
		Obj : Object_Type;
		Pba : Physical_Block_Address_Type)
	return Boolean
	is
	begin
		Item_Loop: for Item of Obj.Cache_Items loop
			if Cache_Item.Used(Item) and
			   Cache_Item.Pba(Item) = Pba then
				return True;
			end if;
		end loop Item_Loop;

		return False;
	end Data_Available;

	--
	-- Data_Index
	--
	function Data_Index(
		Obj : in out Object_Type;
		Pba :        Physical_Block_Address_Type;
		Ts  :        Timestamp)
	return Cache_Index_Type
	is
	begin
		for Cache_Item_Id in Obj.Cache_Items'Range loop
			if Cache_Item.Used(Obj.Cache_Items(Cache_Item_Id)) and
			   Cache_Item.Pba(Obj.Cache_Items(Cache_Item_Id)) = Pba then
				Cache_Item.Set_Ts(Obj.Cache_Items(Cache_Item_Id), Ts);
				return Cache_Item_Id; -- XXX convert
			end if;
		end loop;

		return Cache_Index_Type'Last; -- XXX make proper INVALID
	end Data_Index;

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(
		Obj : Object_Type;
		Pba : Physical_Block_Address_Type)
	return Boolean
	is
	begin
		-- XXX replace cast, i.e. change Active_Jobs to Cache_Index_Type
		if Obj.Active_Jobs >= Standard.Natural(Cache_Job_Index_Type'Last) then
			return False;
		end if;

		for Item of Obj.Job_Items loop
			if Job_Item.Already_Pending(Item, Pba) then
				return False;
			end if;
		end loop;

		return True;
	end Request_Acceptable;

	--
	-- Submit_Request
	--
	procedure Submit_Request(
		Obj : in out Object_Type;
		Pba :        Physical_Block_Address_Type)
	is
	begin
		--
		-- There is already a Job pending, we do not need to do the work
		-- twice.
		--
		Already_Pending: for Item of Obj.Job_Items loop
			if Job_Item.Already_Pending(Item, Pba) then
				return;
			end if;
		end loop Already_Pending;

		--
		-- The data is already available, do not create a new Job.
		-- (XXX It stands to reason is this check is necessary.)
		--
		if Data_Available(Obj, Pba) then
			return;
		end if;

		New_Job: for Job_Item_Id in Obj.Job_Items'Range loop
			if Job_Item.Unused(Obj.Job_Items(Job_Item_Id)) then
				Job_Item.Initialize_Object(Obj.Job_Items(Job_Item_Id), Pba);
				Job_Item.State(Obj.Job_Items(Job_Item_Id), Job_Item.Pending);
				Obj.Active_Jobs := Obj.Active_Jobs + 1;
				return;
			end if;
		end loop New_Job;
	end Submit_Request;

	--
	-- Fill_Cache
	--
	procedure Fill_Cache(
		Obj      : in out Object_Type;
		Data     : in out Cache_Data_Type;
		Job_Data : in     Cache_Job_Data_Type;
		Time     :        Timestamp)
	is
		Cache_Id : Cache_Index_Type;
	begin
		Obj.Execute_Progress := False;

		Complete_Job: for Job_Item_Id in Obj.Job_Items'Range loop
			if Job_Item.Complete(Obj.Job_Items(Job_Item_Id)) and
			   Job_Item.Success(Obj.Job_Items(Job_Item_Id)) then
				Cache_Id := Get_Cache_Slot(Obj);
				Cache_Item.Initialize_Object(Obj.Cache_Items(Cache_Id),
				                             Job_Item.Pba(Obj.Job_Items(Job_Item_Id)),
				                             Time);

				Data.Items(Cache_Id) := Job_Data.Items(Job_Item_Id); -- XXX will it work?

				Job_Item.Set_Unused(Obj.Job_Items(Job_Item_Id));

				Obj.Execute_Progress := True;
				Obj.Active_Jobs := Obj.Active_Jobs - 1;
			end if;
		end loop Complete_Job;
	end Fill_Cache;

	--
	-- Execute_Progress
	--
	function Execute_Progress(Obj : Object_Type) return Boolean is (Obj.Execute_Progress);

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Object_Type)
	return Primitive.Object_Type
	is
	begin
		Peek_Primitive: for Job_Item_Id in Obj.Job_Items'Range loop
			if Job_Item.Pending(Obj.Job_Items(Job_Item_Id)) then

				return Primitive.Valid_Object(
					Op => Request.Read,
					Succ => Request.Success_Type(False),
					Tg => 16#20#,
					Blk_Nr => Request.Block_Number_Type(Job_Item.Pba(Obj.Job_Items(Job_Item_Id))),
					Idx => 0);
			end if;
		end loop Peek_Primitive;

		return Primitive.Invalid_Object;
	end Peek_Generated_Primitive;

	--
	-- Peek_Generated_Data_Index
	--
	function Peek_Generated_Data_Index(
		Obj  : Object_Type;
		Prim : Primitive.Object_Type)
	return Cache_Index_Type
	is
	begin
		Pending_Data: for Job_Item_Id in Obj.Job_Items'Range loop
			if Job_Item.Pending(Obj.Job_Items(Job_Item_Id)) and
			   Job_Item.Pba(Obj.Job_Items(Job_Item_Id)) = Physical_Block_Address_Type(Primitive.Block_Number(Prim)) then
				return Cache_Index_Type(Job_Item_Id);
			end if;
		end loop Pending_Data;

		return Cache_Index_Type'Last;
	end Peek_Generated_Data_Index;

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Object_Type;
		Prim :        Primitive.Object_Type)
	is
	begin
		Drop_Primitive: for Job_Item_Id in Obj.Job_Items'Range loop
			if Job_Item.Pending(Obj.Job_Items(Job_Item_Id)) and
			   Job_Item.Pba(Obj.Job_Items(Job_Item_Id)) = Physical_Block_Address_Type(Primitive.Block_Number(Prim)) then
				Job_Item.State(Obj.Job_Items(Job_Item_Id), Job_Item.In_Progress);
				return;
			end if;
		end loop Drop_Primitive;
	end Drop_Generated_Primitive;

	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Completed_Primitive(
		Obj  : in out Object_Type;
		Prim :        Primitive.Object_Type)
	is
	begin
		Complete_Primitive: for Job_Item_Id in Obj.Job_Items'Range loop
			if Job_Item.In_Progress(Obj.Job_Items(Job_Item_Id)) and
			   Job_Item.Pba(Obj.Job_Items(Job_Item_Id)) = Physical_Block_Address_Type(Primitive.Block_Number(Prim)) then
				Job_Item.State(Obj.Job_Items(Job_Item_Id), Job_Item.Complete);
				Job_Item.Set_Success(Obj.Job_Items(Job_Item_Id), Boolean(Primitive.Success(Prim)));
				return;
			end if;
		end loop Complete_Primitive;
	end Mark_Completed_Primitive;
end CBE.Cache;
