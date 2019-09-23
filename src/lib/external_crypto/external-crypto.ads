--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Request;
with Aes_Cbc_4k;

package External.Crypto
with Spark_Mode
is
	--  Disable for now because of libsparkcrypto
	--  pragma Pure;

	subtype Key_Data_Type    is Aes_Cbc_4k.Key_Type;
	subtype Plain_Data_Type  is Aes_Cbc_4k.Plaintext_Type;
	subtype Cipher_Data_Type is Aes_Cbc_4k.Ciphertext_Type;

	type Object_Type is private;

	type Key_Id_Type   is mod 2**32;
	type Key_Slot_Type is mod 2**32;

	type Key_Type is record
		Data  : Key_Data_Type;
		Id    : Key_Id_Type;
		Valid : Boolean;
	end record;

	function Invalid_Key
	return Key_Type
	is (
		Data  => (others => 0),
		Id    => 0,
		Valid => False);

	function Key_Invalid (Key : Key_Type)
	return Boolean is (Key.Valid = False);

	function Key_Data (Key : Key_Type)
	return Key_Data_Type is (Key.Data);

	--
	-- Initialize_Object
	--
	-- FIXME will not be used anymore when the library module is in spark
	--
	procedure Initialize_Object(
		Obj : out Object_Type);

	--
	-- Initialized_Object
	--
	function Initialized_Object
	return Object_Type;

	--
	-- Set_Key
	--
	procedure Set_Key (
		Obj      : in out Object_Type;
		Slot     :        Key_Slot_Type;
		Key_Id   :        Key_Id_Type;
		Key_Data :        Key_Data_Type;
		Result   :    out Boolean);

	--
	-- Execute
	--
	procedure Execute (
		Obj      : in out Object_Type;
		Progress :    out Boolean);

	--
	-- Encryption_Request_acceptable
	--
	function Encryption_Request_acceptable (Obj : Object_Type) return Boolean;

	--
	-- Submit_Encryption_Request
	--
	procedure Submit_Encryption_Request (
		Obj  : in out Object_Type;
		Req  :        CBE.Request.Object_Type;
		Data :        Plain_Data_Type)
	with
		Pre => (Encryption_Request_acceptable (Obj) and CBE.Request.Valid (Req));

	--
	-- Peek_Completed_Encryption_Request
	--
	function Peek_Completed_Encryption_Request (Obj : Object_Type)
	return CBE.Request.Object_Type;

	--
	-- Supply_Cipher_Data
	--
	procedure Supply_Cipher_Data (
		Obj  : in out Object_Type;
		Req  :        CBE.Request.Object_Type;
		Data :    out Cipher_Data_Type;
		Res  :    out Boolean);

	--
	-- Decryption_Request_Acceptable
	--
	function Decryption_Request_Acceptable (Obj : Object_Type) return Boolean;

	--
	-- Submit_Decryption_Request
	--
	procedure Submit_Decryption_Request (
		Obj  : in out Object_Type;
		Req  :        CBE.Request.Object_Type;
		Data :        Cipher_Data_Type);

	--
	-- Peek_completed_decryption_request()
	--
	function Peek_completed_decryption_request (Obj : Object_Type)
	return CBE.Request.Object_Type;

	--
	-- Supply_Plain_Data
	--
	procedure Supply_Plain_Data (
		Obj  : in out Object_Type;
		Req  :        CBE.Request.Object_Type;
		Data :    out Plain_Data_Type;
		Res  :    out Boolean);

private

	--
	-- Item
	--
	package Item
	with Spark_Mode
	is
		type State_Type is (Invalid, Pending, Complete);
		type Item_Type  is private;

		--
		-- Invalid_Object
		--
		function Invalid_Object
		return Item_Type;

		function Initialized_Object (
			S : Item.State_Type;
			R : CBE.Request.Object_Type)
		return Item_Type;

		--
		-- Write accessors
		--
		procedure State (Obj : in out Item_Type; S : State_Type);

		procedure Request_Success (
			Obj : in out Item_Type;
			S   :        CBE.Request.Success_Type);

		--
		-- Read accessors
		--
		function Invalid  (Obj : Item_Type) return Boolean;
		function Pending  (Obj : Item_Type) return Boolean;
		function Complete (Obj : Item_Type) return Boolean;

		function Request     (Obj : Item_Type) return CBE.Request.Object_Type;


	private

		--
		-- Item_Type
		--
		type Item_Type is record
			State       : State_Type;
			Req         : CBE.Request.Object_Type;
		end record;

	end Item;

	-- 1 for encryption and 2 for decryption
	type Items_Type is array (1..2) of Item.Item_Type;
	type Cipher_Data_Array_Type is array (1..2) of Cipher_Data_Type;
	type Plain_Data_Array_Type is array (1..2) of Plain_Data_Type;

	type Key_Array_Type is array (1..2) of Key_Type;

	--
	-- Object_Type
	--
	type Object_Type is record
		Items            : Items_Type;
		Keys             : Key_Array_Type;
		Cipher_Data      : Cipher_Data_Array_Type;
		Plain_Data       : Plain_Data_Array_Type;
		Execute_Progress : Boolean;
	end record;

end External.Crypto;
