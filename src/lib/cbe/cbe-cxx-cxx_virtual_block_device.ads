
pragma Ada_2012;

with CBE.Virtual_Block_Device;
with CBE.Cache;
with CBE.CXX.CXX_Request;
with CBE.CXX.CXX_Primitive;

package CBE.CXX.CXX_Virtual_Block_Device
with Spark_Mode
is
	pragma Pure;

	--
	-- Object_Size
	--
	function Object_Size (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Object_Size_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe11object_sizeERKNS_20Virtual_block_deviceE";

	--
	-- Initialize_Object
	--
	procedure Initialize_Object (
		Obj    : out Virtual_Block_Device.Object_Type;
		Height :     CXX_Tree_Level_Type;
		Degree :     CXX_Tree_Degree_Type;
		Leafs  :     CXX_Tree_Number_Of_Leafs_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_deviceC2Ejjy";

	--
	-- Trans_Inhibit_Translation
	--
	procedure Trans_Inhibit_Translation (
		Obj : in out Virtual_Block_Device.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device25trans_inhibit_translationEv";

	--
	-- Trans_Resume_Translation
	--
	procedure Trans_Resume_Translation (
		Obj : in out Virtual_Block_Device.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device24trans_resume_translationEv";

	--
	-- Trans_Get_Virtual_Block_Address
	--
	function Trans_Get_Virtual_Block_Address (
		Obj  : Virtual_Block_Device.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Request.Block_Number_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device31trans_get_virtual_block_addressERKNS_9PrimitiveE";

	--
	-- Trans_Can_Get_Type_1_Info
	--
	function Trans_Can_Get_Type_1_Info (
		Obj   : Virtual_Block_Device.Object_Type;
		Prim  : CXX_Primitive.Object_Type;
		Infos : Address_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device25trans_can_get_type_1_infoERKNS_9PrimitiveEPNS_16Type_1_node_infoE";

	--
	-- Trans_Get_Type_1_Info
	--
	procedure Trans_Get_Type_1_Info (
		Obj   :        Virtual_Block_Device.Object_Type;
		Infos : in out CXX_Type_1_Node_Infos_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device21trans_get_type_1_infoEPNS_16Type_1_node_infoE";

	--
	-- Tree_Height
	--
	function Tree_Height (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Tree_Level_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe20Virtual_block_device11tree_heightEv";

	--
	-- Index_For_Level
	--
	function Index_For_Level (
		Obj   : Virtual_Block_Device.Object_Type;
		VBA   : CXX_Virtual_Block_Address_Type;
		Level : CXX_Tree_Level_Type)
	return CXX_Tree_Child_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe20Virtual_block_device15index_for_levelEyj";

	--
	-- Get_Tree_Helper
	--
	function Get_Tree_Helper (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Tree_Helper_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe20Virtual_block_device11tree_helperEv";

	--
	-- Primitive_Acceptable
	--
	function Primitive_Acceptable (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe20Virtual_block_device20primitive_acceptableEv";

	--
	-- Submit_Primitive
	--
	procedure Submit_Primitive (
		Obj  : in out Virtual_Block_Device.Object_Type;
		PBA  :        CXX_Physical_Block_Address_Type;
		Gen  :        CXX_Generation_Type;
		Hash :        Hash_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device16submit_primitiveEyyRKNS_4HashERKNS_9PrimitiveE";

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device24peek_completed_primitiveEv";

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive (
		Obj : in out Virtual_Block_Device.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device24drop_completed_primitiveERKNS_9PrimitiveE";

	--
	-- Execute
	--
	procedure Execute (
		Obj        : in out Virtual_Block_Device.Object_Type;
		Trans_Data : in out Translation_Data_Type;
		Cach       : in out Cache.Object_Type;
		Cach_Data  :        Cache.Cache_Data_Type;
		Timestamp  :        CXX_Timestamp_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe20Virtual_block_device7executeERNS_6Module16Translation_DataERNS1_5CacheERNS1_10Cache_DataEy";

	---------------
	-- Accessors --
	---------------

	function Execute_Progress(Obj : Virtual_Block_Device.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe20Virtual_block_device16execute_progressEv";

end CBE.CXX.CXX_Virtual_Block_Device;
