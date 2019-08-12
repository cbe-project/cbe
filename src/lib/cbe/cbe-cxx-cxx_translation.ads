--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.CXX.CXX_Primitive;
with CBE.CXX.CXX_Request;
with CBE.Translation;

package CBE.CXX.CXX_Translation
with Spark_Mode
is
	pragma Pure;

	--
	-- Object_Size
	--
	function Object_Size (Obj : Translation.Object_Type)
	return CXX_Object_Size_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11object_sizeERKNS0_11TranslationE";

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(
		Obj     : out Translation.Object_Type;
		Helpr   :     CXX_Tree_Helper_Type;
		Free_Tr :     CXX_Bool_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11TranslationC2ERNS_11Tree_helperEb";

	--
	-- Height
	--
	function Height (Obj : Translation.Object_Type)
	return CXX_Tree_Level_Type
	with
		Export,
		Convention    => C,
		External_Name => "a";

	--
	-- Index
	--
	function Index (
		Obj   : Translation.Object_Type;
		VBA   : CXX_Virtual_Block_Address_Type;
		Level : CXX_Tree_Level_Type)
	return CXX_Tree_Child_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "b";

	--
	-- Acceptable
	--
	function Acceptable (Obj : Translation.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module11Translation10acceptableEv";

	--
	-- Suspend
	--
	procedure Suspend(Obj : in out Translation.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation7suspendEv";

	--
	-- Resume
	--
	procedure Resume(Obj : in out Translation.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation6resumeEv";

	--
	-- Submit_Primitive
	--
	procedure Submit_Primitive (
		Obj       : in out Translation.Object_Type;
		Root_PBA  :        CXX_Physical_Block_Address_Type;
		Root_Gen  :        CXX_Generation_Type;
		Root_Hash :        Hash_Type;
		Prim      :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation16submit_primitiveEyyRKNS_4HashERKNS_9PrimitiveE";

	--
	-- Execute
	--
	procedure Execute (
		Obj        : in out Translation.Object_Type;
		Trans_Data :        Translation_Data_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation7executeERNS0_16Translation_DataE";

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive (Obj : Translation.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation24peek_completed_primitiveEv";

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive (
		Obj  : in out Translation.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation24drop_completed_primitiveERKNS_9PrimitiveE";

	--
	-- Get_Virtual_Block_Address
	--
	function Get_Virtual_Block_Address (
		Obj  : Translation.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Request.Block_Number_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation25get_virtual_block_addressERKNS_9PrimitiveE";

	--
	-- Can_Get_Type_1_Info
	--
	function Can_Get_Type_1_Info (
		Obj   : Translation.Object_Type;
		Prim  : CXX_Primitive.Object_Type;
		Infos : Address_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation19can_get_type_1_infoERKNS_9PrimitiveEPNS_16Type_1_node_infoE";

	--
	-- Get_Type_1_Info
	--
	procedure Get_Type_1_Info (
		Obj   :        Translation.Object_Type;
		Infos : in out CXX_Type_1_Node_Infos_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation15get_type_1_infoEPNS_16Type_1_node_infoE";

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive (Obj : Translation.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module11Translation24peek_generated_primitiveEv";

	--
	-- Discard_Generated_Primitive
	--
	procedure Discard_Generated_Primitive (
		Obj  : in out Translation.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation27discard_generated_primitiveERKNS_9PrimitiveE";

	--
	-- Mark_Generated_Primitive_Complete
	--
	procedure Mark_Generated_Primitive_Complete (
		Obj        : in out Translation.Object_Type;
		Prim       :        CXX_Primitive.Object_Type;
		Data       :        Block_Data_Type;
		Trans_Data : in out Translation_Data_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11Translation33mark_generated_primitive_completeERKNS_9PrimitiveERKNS_10Block_dataERNS0_16Translation_DataE";

	---------------
	-- Accessors --
	---------------

	function Execute_Progress(Obj : Translation.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module11Translation16execute_progressEv";

	function Max_Levels(Obj : Translation.Object_Type)
	return CXX_Tree_Level_Type
	with
		Export,
		Convention    => C,
		External_Name => "FIXME";

end CBE.CXX.CXX_Translation;
