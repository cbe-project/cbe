--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body External.Crypto
with Spark_Mode
is
	--
	-- Initialize_Object
	--
	-- FIXME will not be used anymore when the library module is in spark
	--
	procedure Initialize_Object(
		Obj : out Object_Type)
	is
	begin
		Obj := Initialized_Object;
	end Initialize_Object;


	--
	-- Initialized_Object
	--
	function Initialized_Object
	return Object_Type
	is (
		Items            => (others => Item.Invalid_Object),
		Keys             => (others => (Invalid_Key)),
		Cipher_Data => (others => (others => 0)),
		Plain_Data => (others => (others => 0)),
		Execute_Progress => False);


	--
	-- Set_Key
	--
	procedure Set_Key (
		Obj      : in out Object_Type;
		Slot     :        Key_Slot_Type;
		Key_Id   :        Key_Id_Type;
		Key_Data :        Key_Data_Type;
		Result   :    out Boolean)
	is
	begin
		Keys_Loop: for K in Obj.Keys'Range loop
			if Key_Slot_Type (K) = Slot then
				Obj.Keys (K) := (
					Data  => Key_Data,
					Id    => Key_Id,
					Valid => True);
				Result := True;
				return;
			end if;
		end loop Keys_Loop;
		-- XXX no free key slot found
		Result := False;
	end Set_Key;


	--
	-- Execute
	--
	procedure Execute (
		Obj      : in out Object_Type;
		Progress :    out Boolean)
	is
	begin
		Progress := False;

		if Item.Pending (Obj.Items (1)) then
			Aes_Cbc_4k.Encrypt (
				Key_Data (Obj.Keys (1)),
				Aes_Cbc_4k.Block_Number_Type (
					CBE.Request.Block_Number (Item.Request (Obj.Items (1)))),
					Obj.Plain_Data (1),
					Obj.Cipher_Data (1));
			Item.State (Obj.Items (1), Item.Complete);
			Item.Request_Success (Obj.Items (1), True);
			Progress := True;
		end if;

		if Item.Pending (Obj.Items (2)) then
			Aes_Cbc_4k.Decrypt (
				Key_Data (Obj.Keys (1)),
				Aes_Cbc_4k.Block_Number_Type (
					CBE.Request.Block_Number (Item.Request (Obj.Items (2)))),
					Obj.Cipher_Data (2),
					Obj.Plain_Data (2));
			Item.State (Obj.Items (2), Item.Complete);
			Item.Request_Success (Obj.Items (2), True);
			Progress := True;
		end if;
	end Execute;


	--
	-- Encryption_Request_acceptable
	--
	function Encryption_Request_acceptable (Obj : Object_Type)
	return Boolean
	is
	begin
		return Item.Invalid (Obj.Items (1));
	end Encryption_Request_acceptable;


	--
	-- Submit_Encryption_Request
	--
	procedure Submit_Encryption_Request (
		Obj  : in out Object_Type;
		Req  :        CBE.Request.Object_Type;
		Data :        Plain_Data_Type)
	is
	begin
		Obj.Items (1) := Item.Initialized_Object (
			S => Item.Pending,
			R => Req);
		Obj.Cipher_Data (1) := (others => 0);
		Obj.Plain_Data (1) := Data;
	end Submit_Encryption_Request;


	--
	-- Peek_Completed_Encryption_Request
	--
	function Peek_Completed_Encryption_Request (Obj : Object_Type)
	return CBE.Request.Object_Type
	is
	begin
		if Item.Complete (Obj.Items (1)) then
			return Item.Request (Obj.Items (1));
		else
			return CBE.Request.Invalid_Object;
		end if;
	end Peek_Completed_Encryption_Request;


	--
	-- Supply_Cipher_Data
	--
	procedure Supply_Cipher_Data (
		Obj  : in out Object_Type;
		Req  :        CBE.Request.Object_Type;
		Data :    out Cipher_Data_Type;
		Res  :    out Boolean)
	is
	begin
		Res := False;

		if not CBE.Request.Equal (Req, Item.Request (Obj.Items (1))) then
			return;
		end if;

		if Item.Complete (Obj.Items (1)) then
			Data := Obj.Cipher_Data (1);
			Item.State (Obj.Items (1), Item.Invalid);
			Res := True;
		end if;
	end Supply_Cipher_Data;


	--
	-- Decryption_Request_Acceptable
	--
	function Decryption_Request_Acceptable (Obj : Object_Type)
	return Boolean
	is
	begin
		return Item.Invalid (Obj.Items (2));
	end Decryption_Request_Acceptable;


	--
	-- Submit_Decryption_Request
	--
	procedure Submit_Decryption_Request (
		Obj  : in out Object_Type;
		Req  :        CBE.Request.Object_Type;
		Data :        Cipher_Data_Type)
	is
	begin
		Obj.Items (2) := Item.Initialized_Object (
			S => Item.Pending,
			R => Req);
		Obj.Cipher_Data (2) := Data;
		Obj.Plain_Data (2) := (others => 0);
	end Submit_Decryption_Request;


	--
	-- Peek_Completed_Decryption_Request
	--
	function Peek_Completed_Decryption_Request (Obj : Object_Type)
	return CBE.Request.Object_Type
	is
	begin
		if Item.Complete (Obj.Items (2)) then
			return Item.Request (Obj.Items (2));
		else
			return CBE.Request.Invalid_Object;
		end if;
	end Peek_Completed_Decryption_Request;


	--
	-- Supply_Plain_Data
	--
	procedure Supply_Plain_Data (
		Obj  : in out Object_Type;
		Req  :        CBE.Request.Object_Type;
		Data :    out Plain_Data_Type;
		Res  :    out Boolean)
	is
	begin
		Res := False;
		if not CBE.Request.Equal (Req, Item.Request (Obj.Items (2))) then
			return;
		end if;

		if Item.Complete (Obj.Items (2)) then
			Data := Obj.Plain_Data (2);
			Item.State (Obj.Items (2), Item.Invalid);
			Res := True;
		end if;
	end Supply_Plain_Data;


	--
	-- Item
	--
	package body Item
	with Spark_Mode
	is
		--
		-- Invalid_Object
		--
		function Invalid_Object
		return Item_Type
		is (
			State       => Invalid,
			Req         => CBE.Request.Invalid_Object);

		--
		-- Initialized_Object
		--
		function Initialized_Object (
			S : State_Type;
			R : CBE.Request.Object_Type)
		return Item_Type
		is (
			State       => S,
			Req         => R);

		--
		-- Write accessors
		--
		procedure State (Obj : in out Item_Type; S : State_Type)
		is
		begin
			Obj.State := S;
		end State;

		procedure Request_Success (Obj : in out Item_Type; S : CBE.Request.Success_Type)
		is
		begin
			CBE.Request.Success (Obj.Req, S);
		end Request_Success;

		--
		-- Read accessors
		--
		function Invalid  (Obj : Item_Type) return Boolean is (Obj.State = Invalid);
		function Pending  (Obj : Item_Type) return Boolean is (Obj.State = Pending);
		function Complete (Obj : Item_Type) return Boolean is (Obj.State = Complete);

		function Request     (Obj : Item_Type) return CBE.Request.Object_Type is (Obj.Req);

	end Item;

end External.Crypto;
