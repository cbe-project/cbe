--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX.CXX_Block_IO
with Spark_Mode
is
	function Object_Size (Obj : Block_IO.Object_Type)
	return CXX_Object_Size_Type
	is (Obj'Size / 8);

	procedure Initialize_Object(Obj : out Block_IO.Object_Type)
	is
	begin
		Block_IO.Initialize_Object(Obj);
	end Initialize_Object;

	function Primitive_Acceptable(Obj : Block_IO.Object_Type) return CXX_Bool_Type
	is (CXX_Bool_From_SPARK(Block_IO.Primitive_Acceptable(Obj)));

	procedure Submit_Primitive(
		Obj     : in out Block_IO.Object_Type;
		Tag     :        CBE.Tag_Type;
		Prim    :        CXX_Primitive.Object_Type;
		IO_Data : in out Block_IO.Data_Type;
		Data    : in     Block_Data_Type)
	is
	begin
		Block_IO.Submit_Primitive(Obj, Tag, CXX_Primitive.To_SPARK(Prim),
		                          IO_Data, Data);
	end Submit_Primitive;

	function Peek_Completed_Primitive (Obj : Block_IO.Object_Type)
	return CXX_Primitive.Object_Type
	is (CXX_Primitive.From_SPARK(Block_IO.Peek_Completed_Primitive(Obj)));

	function Peek_Completed_Data_Index (Obj : Block_IO.Object_Type)
	return CXX_Data_Index_Type
	is (CXX_Data_Index_Type(Block_IO.Peek_Completed_Data_Index(Obj)));

	function Peek_Completed_Tag (
		Obj  : Block_IO.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CBE.Tag_Type
	is (Block_IO.Peek_Completed_Tag(Obj, CXX_Primitive.To_SPARK(Prim)));

	procedure Drop_Completed_Primitive (
		Obj  : in out Block_IO.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Block_IO.Drop_Completed_Primitive(Obj, CXX_Primitive.To_SPARK(Prim));
	end Drop_Completed_Primitive;

	function Peek_Generated_Primitive (Obj : Block_IO.Object_Type)
	return CXX_Primitive.Object_Type
	is (CXX_Primitive.From_SPARK(Block_IO.Peek_Generated_Primitive(Obj)));

	function Peek_Generated_Data_Index (
		Obj  : Block_IO.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Data_Index_Type
	is (CXX_Data_Index_Type(Block_IO.Peek_Generated_Data_Index(Obj, CXX_Primitive.To_SPARK(Prim))));

	procedure Drop_Generated_Primitive (
		Obj  : in out Block_IO.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Block_IO.Drop_Generated_Primitive(Obj, CXX_Primitive.To_SPARK(Prim));
	end Drop_Generated_Primitive;

	procedure Mark_Generated_Primitive_Complete (
		Obj  : in out Block_IO.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Block_IO.Mark_Generated_Primitive_Complete(Obj, CXX_Primitive.To_SPARK(Prim));
	end Mark_Generated_Primitive_Complete;

end CBE.CXX.CXX_Block_IO;
