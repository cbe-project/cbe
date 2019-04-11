--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX.CXX_Crypto
with Spark_Mode
is
	procedure print_size(Sz : CXX_Size_Type)
	with
		Import,
		Convention    => C,
		External_Name => "print_size";

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(
		Obj       : out Crypto.Object_Type;
		Key       :     Crypto.Key_Type;
		Obj_Size  :     CXX_Size_Type;
		Data_Size :     CXX_Size_Type;
		Prim_Size :     CXX_Size_Type)
	is
		Plain_Data  : constant Crypto.Plain_Data_Type    := (others => 0);
		Cipher_Data : constant Crypto.Cipher_Data_TYpe   := (others => 0);
		Prim        : constant CXX_Primitive.Object_Type := (0,0,0,0,0);
	begin
		if not Correct_Object_Size(Obj'Size, Obj_Size) then
			print_size(Obj'Size);
			raise Program_Error;
		elsif not Correct_Object_Size(Plain_Data'Size, Data_Size) then
			print_size(Plain_Data'Size);
			raise Program_Error;
		elsif not Correct_Object_Size(Cipher_Data'Size, Data_Size) then
			print_size(Cipher_Data'Size);
			raise Program_Error;
		elsif not Correct_Object_Size(Prim'Size, Prim_Size) then
			print_size(Prim'Size);
			raise Program_Error;
		end if;

		Crypto.Initialize_Object(Obj, Key);
	end Initialize_Object;

	--
	-- Primitive_Acceptable
	--
	function Primitive_Acceptable(Obj : Crypto.Object_Type)
	return CXX_Bool_Type
	is (Boolean_To_CXX(Crypto.Primitive_Acceptable(Obj)));

	--
	-- Submit_Primitive
	--
	procedure Submit_Primitive(
		Obj         : in out Crypto.Object_Type;
		Prim        :        CXX_Primitive.Object_Type;
		Plain_Data  :        Crypto.Plain_Data_Type;
		Cipher_Data :        Crypto.Cipher_Data_Type)
	is
	begin
		Crypto.Submit_Primitive(Obj, CXX_Primitive.To_Spark(Prim), Plain_Data, Cipher_Data);
	end Submit_Primitive;

	--
	-- Execute
	--
	procedure Execute(Obj : in out Crypto.Object_Type)
	is
	begin
		Crypto.Execute(Obj);
	end Execute;

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Crypto.Object_Type)
	return CXX_Primitive.Object_Type
	is (CXX_Primitive.From_Spark(Crypto.Peek_Generated_Primitive(Obj)));

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Crypto.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Crypto.Drop_Generated_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Drop_Generated_Primitive;

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive(Obj : Crypto.Object_Type)
	return CXX_Primitive.Object_Type
	is (CXX_Primitive.From_Spark(Crypto.Peek_Completed_Primitive(Obj)));

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive(
		Obj  : in out Crypto.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Crypto.Drop_Completed_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Drop_Completed_Primitive;

	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Completed_Primitive(
		Obj  : in out Crypto.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Crypto.Mark_Completed_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Mark_Completed_Primitive;


	---------------
	-- Accessors --
	---------------

	function Execute_Progress(Obj : Crypto.Object_Type)
	return CXX_Bool_Type
	is (Boolean_To_CXX(Crypto.Execute_Progress(Obj)));

end CBE.CXX.CXX_Crypto;
