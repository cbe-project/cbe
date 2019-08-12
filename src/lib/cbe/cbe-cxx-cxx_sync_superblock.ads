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
with CBE.Sync_Superblock;

package CBE.CXX.CXX_Sync_Superblock
with Spark_Mode
is
	pragma Pure;

	--
	-- Object_Size
	--
	function Object_Size (Obj : Sync_Superblock.Object_Type) return CXX_Object_Size_Type
		with Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11object_sizeERKNS0_15Sync_superblockE";

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(Obj : out Sync_Superblock.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module15Sync_superblockC1Ev";

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(Obj : Sync_Superblock.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module15Sync_superblock18request_acceptableEv";

	--
	-- Submit_Primitive
	--
	procedure Submit_Request(
		Obj : in out Sync_Superblock.Object_Type;
		Idx :        CXX_Superblock_Index_Type;
		Gen :        CXX_Generation_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module15Sync_superblock14submit_requestEyy";

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive(Obj : Sync_Superblock.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module15Sync_superblock24peek_completed_primitiveEv";

	--
	-- Peek_Completed_Generation
	--
	function Peek_Completed_Generation(
		Obj  : Sync_Superblock.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Generation_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module15Sync_superblock25peek_completed_generationERKNS_9PrimitiveE";

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive(
		Obj  : in out Sync_Superblock.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module15Sync_superblock24drop_completed_primitiveERKNS_9PrimitiveE";

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Sync_Superblock.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module15Sync_superblock24peek_generated_primitiveEv";

	--
	-- Peek_Generated_Data_Index
	--
	function Peek_Generated_Index(
		Obj  : Sync_Superblock.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Superblock_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module15Sync_superblock17peek_generated_idERKNS_9PrimitiveE";

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Sync_Superblock.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module15Sync_superblock24drop_generated_primitiveERKNS_9PrimitiveE";

	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Generated_Primitive_Complete(
		Obj  : in out Sync_Superblock.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module15Sync_superblock33mark_generated_primitive_completeERKNS_9PrimitiveE";

end CBE.CXX.CXX_Sync_Superblock;
