--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with Aes_Cbc_4k;

package CBE.Crypto
with Spark_Mode
is
	--  Disable for now because of libsparkcrypto
	--  pragma Pure;

	subtype Key_Type         is Aes_Cbc_4k.Key_Type;
	subtype Plain_Data_Type  is Aes_Cbc_4k.Plaintext_Type;
	subtype Cipher_Data_Type is Aes_Cbc_4k.Ciphertext_Type;

	type Object_Type is private;

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(
		Obj : out Object_Type;
		Key :     Key_Type)
	with
		Post => (Primitive_Acceptable(Obj));

	--
	-- Primitive_Acceptable
	--
	function Primitive_Acceptable(Obj : Object_Type)
	return Boolean;

	--
	-- Submit_Primitive
	--
	procedure Submit_Primitive(
		Obj        : in out Object_Type;
		Prim       :        Primitive.Object_Type;
		Plain_Data :        Plain_Data_Type)
	with
		Pre => (Primitive_Acceptable(Obj) and Primitive.Valid(Prim));

	--
	-- Execute
	--
	procedure Execute(Obj : in out Object_Type);

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Object_Type)
	return Primitive.Object_Type;

	--
	-- Peek_Generated_Cipher_Data
	--
	function Peek_Generated_Cipher_Data(
		Obj  : Object_Type;
		Prim : Primitive.Object_Type)
	return Cipher_Data_Type;

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Object_Type;
		Prim :        Primitive.Object_Type)
	with
		Pre => (Primitive.Valid(Prim));

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive(Obj : Object_Type)
	return Primitive.Object_Type;

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive(
		Obj  : in out Object_Type;
		Prim :        Primitive.Object_Type)
	with
		Pre => (Primitive.Valid(Prim));

	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Completed_Primitive(
		Obj  : in out Object_Type;
		Prim :        Primitive.Object_Type);


	---------------
	-- Accessors --
	---------------

	function Execute_Progress(Obj : Object_Type) return Boolean;

private

	--
	-- Item
	--
	package Item
	with Spark_Mode
	is
		type State_Type is (Invalid, Submitted, Pending, In_Progress, Complete);
		type Item_Type  is private;
		--
		-- Execute
		--
		procedure Execute(
			Obj : in out Item_Type;
			Key :        Key_Type);

		--
		-- Mark_Completed_Primitive
		--
		procedure Mark_Completed_Primitive(
			Obj : in out Item_Type;
			Prm :        Primitive.Object_Type;
			Key :        Key_Type);

		--
		-- Invalid_Object
		--
		function Invalid_Object
		return Item_Type;

		--
		-- Submitted_Object
		--
		function Submitted_Object(
			Prm       : Primitive.Object_Type;
			Plain_Dat : Plain_Data_Type)
		return Item_Type;


		--------------------
		-- Read Accessors --
		--------------------

		function Invalid     (Obj : Item_Type) return Boolean;
		function Pending     (Obj : Item_Type) return Boolean;
		function Submitted   (Obj : Item_Type) return Boolean;
		function In_Progress (Obj : Item_Type) return Boolean;
		function Complete    (Obj : Item_Type) return Boolean;
		function Prim        (Obj : Item_Type) return Primitive.Object_Type;
		function Plain_Data  (Obj : Item_Type) return Plain_Data_Type;
		function Cipher_Data (Obj : Item_Type) return Cipher_Data_Type;


		---------------------
		-- Write Accessors --
		---------------------

		procedure State(Obj : in out Item_Type; Sta : State_Type) with Pre => not Invalid(Obj);

	private

		--
		-- Item_Type
		--
		type Item_Type is record
			State       : State_Type;
			Prim        : Primitive.Object_Type;
			Plain_Data  : Plain_Data_Type;
			Cipher_Data : Cipher_Data_Type;
		end record;

	end Item;

	type Items_Type is array (1..1) of Item.Item_Type;

	--
	-- Object_Type
	--
	type Object_Type is record
		Key              : Key_Type;
		Items            : Items_Type;
		Execute_Progress : Boolean;
	end record;

end CBE.Crypto;
