--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.CXX;
with CBE.CXX.CXX_Primitive;
with CBE.Cache_Flusher;

package CBE.CXX.CXX_Cache_Flusher
with Spark_Mode
is
	pragma Pure;

	--
	-- Object_Size
	--
	function Object_Size (Obj : Cache_Flusher.Object_Type) return CXX_Object_Size_Type
		with Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11object_sizeERKNS0_13Cache_flusherE";

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(Obj : out Cache_Flusher.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module13Cache_flusherC1Ev";

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(Obj : Cache_Flusher.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module13Cache_flusher18request_acceptableEv";

	--
	-- Submit_Primitive
	--
	procedure Submit_Request(
		Obj : in out Cache_Flusher.Object_Type;
		Pba :        CXX_Physical_Block_Address_Type;
		Idx :        CXX_Index_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module13Cache_flusher14submit_requestEyNS0_11Cache_IndexE";

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive(Obj : Cache_Flusher.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module13Cache_flusher24peek_completed_primitiveEv";

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive(
		Obj  : in out Cache_Flusher.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module13Cache_flusher24drop_completed_primitiveERKNS_9PrimitiveE";

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Cache_Flusher.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module13Cache_flusher24peek_generated_primitiveEv";

	--
	-- Peek_Generated_Data_Index
	--
	function Peek_Generated_Data_Index(
		Obj  : Cache_Flusher.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module13Cache_flusher25peek_generated_data_indexERKNS_9PrimitiveE";

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Cache_Flusher.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module13Cache_flusher24drop_generated_primitiveERKNS_9PrimitiveE";

	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Generated_Primitive_Complete(
		Obj  : in out Cache_Flusher.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module13Cache_flusher33mark_generated_primitive_completeERKNS_9PrimitiveE";

end CBE.CXX.CXX_Cache_Flusher;
