--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX.CXX_Cache
with Spark_Mode
is

	--
	-- Object_Size
	--
	function Object_Size (Obj : Cache.Object_Type)
	return CXX_Object_Size_Type is (Obj'Size / 8);

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(Obj : out Cache.Object_Type)
	is
	begin
		Cache.Initialize_Object(Obj);
	end Initialize_Object;

	--
	-- Data_Available
	--
	function Data_Available(
		Obj : Cache.Object_Type;
		Pba : CXX_Physical_Block_Address_Type)
	return CXX_Bool_Type
	is
	begin
		return Boolean_To_CXX(Cache.Data_Available(Obj, Physical_Block_Address_Type(Pba)));
	end Data_Available;

	--
	-- Data_Index
	--
	function Data_Index(
		Obj : in out Cache.Object_Type;
		Pba :        CXX_Physical_Block_Address_Type;
		Ts  :        Timestamp_Type)
	return CXX_Index_Type
	is
		Cache_Idx : constant Cache.Cache_Index_Type := Cache.Data_Index(Obj, Physical_Block_Address_Type(Pba), Ts);
	begin
		return Index_Type_To_CXX(Index_Type(Cache_Idx));
	end Data_Index;

	--
	-- Invalidate
	--
	procedure Invalidate(
		Obj : in out Cache.Object_Type;
		Pba :        CXX_Physical_Block_Address_Type)
	is
	begin
		Cache.Invalidate(Obj, Physical_Block_Address_Type(Pba));
	end Invalidate;

	--
	-- Primitive_Acceptable
	--
	function Request_Acceptable(
		Obj : Cache.Object_Type;
		Pba : CXX_Physical_Block_Address_Type)
	return CXX_Bool_Type
	is
	begin
		return Boolean_To_CXX(Cache.Request_Acceptable(Obj, Physical_Block_Address_Type(Pba)));
	end Request_Acceptable;

	--
	-- Submit_Primitive
	--
	procedure Submit_Request(
		Obj : in out Cache.Object_Type;
		Pba :        CXX_Physical_Block_Address_Type)
	is
	begin
		Cache.Submit_Request(Obj, Physical_Block_Address_Type(Pba));
	end Submit_Request;

	--
	-- Fill_Cache
	--
	procedure Fill_Cache(
		Obj      : in out Cache.Object_Type;
		Data     : in out Cache.Cache_Data_Type;
		Job_Data : in     Cache.Cache_Job_Data_Type;
		Time     :        Timestamp_Type)
	is
	begin
		Cache.Fill_Cache(Obj, Data, Job_Data, Time);
	end Fill_Cache;

	--
	-- Execute_Progress
	--
	function Execute_Progress(Obj : Cache.Object_Type)
	return CXX_Bool_Type
	is
	begin
		return (Boolean_To_CXX(Cache.Execute_Progress(Obj)));
	end Execute_Progress;

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Cache.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return (CXX_Primitive.From_Spark(Cache.Peek_Generated_Primitive(Obj)));
	end Peek_Generated_Primitive;

	--
	-- Peek_Generated_Data_Index
	--
	function Peek_Generated_Data_Index(
		Obj  : Cache.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Index_Type
	is
		Idx : constant Cache.Cache_Index_Type := Cache.Peek_Generated_Data_Index(Obj, CXX_Primitive.To_Spark(Prim));
	begin
		return Index_Type_To_CXX(Index_Type(Idx));
	end Peek_Generated_Data_Index;

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Cache.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Cache.Drop_Generated_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Drop_Generated_Primitive;

	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Completed_Primitive(
		Obj  : in out Cache.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Cache.Mark_Completed_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Mark_Completed_Primitive;

end CBE.CXX.CXX_Cache;
