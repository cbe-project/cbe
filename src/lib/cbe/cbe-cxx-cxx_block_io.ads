--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Block_IO;
with CBE.CXX.CXX_Primitive;

package CBE.CXX.CXX_Block_IO
with Spark_Mode
is
	pragma Pure;

	type CXX_Data_Index_Type is mod 2**32;

	function Object_Size (Obj : Block_IO.Object_Type)
	return CXX_Object_Size_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11object_sizeERKNS0_8Block_ioE";

	procedure Initialize_Object(Obj : out Block_IO.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_ioC1Ev";

	function Primitive_Acceptable(Obj : Block_IO.Object_Type) return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module8Block_io20primitive_acceptableEv";

	procedure Submit_Primitive(
		Obj     : in out Block_IO.Object_Type;
		Tag     :        CBE.Tag_Type;
		Prim    :        CXX_Primitive.Object_Type;
		IO_Data : in out Block_IO.Data_Type;
		Data    : in     Block_Data_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io16submit_primitiveENS_3TagERKNS_9PrimitiveERNS0_7Io_dataERNS_10Block_dataEb";

	function Peek_Completed_Primitive (Obj : Block_IO.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io24peek_completed_primitiveEv";

	function Peek_Completed_Data_Index (Obj : Block_IO.Object_Type)
	return CXX_Data_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io25peek_completed_data_indexERKNS_9PrimitiveE";

	function Peek_Completed_Tag (
		Obj  : Block_IO.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CBE.Tag_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io18peek_completed_tagERKNS_9PrimitiveE";

	procedure Drop_Completed_Primitive (
		Obj  : in out Block_IO.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io24drop_completed_primitiveERKNS_9PrimitiveE";

	function Peek_Generated_Primitive (Obj : Block_IO.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io24peek_generated_primitiveEv";

	function Peek_Generated_Data_Index (
		Obj  : Block_IO.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Data_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io25peek_generated_data_indexERKNS_9PrimitiveE";

	procedure Drop_Generated_Primitive (
		Obj  : in out Block_IO.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io24drop_generated_primitiveERKNS_9PrimitiveE";

	procedure Mark_Generated_Primitive_Complete (
		Obj  : in out Block_IO.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Block_io33mark_generated_primitive_completeERKNS_9PrimitiveE";

end CBE.CXX.CXX_Block_IO;
