--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.CXX.CXX_Primitive;
with CBE.Free_Tree;
with CBE.Cache;

package CBE.CXX.CXX_Free_Tree
with Spark_Mode
is
	pragma Pure;

	type CXX_Write_Back_Data_Type is record
		Prim        : CXX_Primitive.Object_Type;
		Gen         : Generation_Type;
		VBA         : Virtual_Block_Address_Type;
		Tree_Height : Tree_Level_Type;
		New_PBAs    : Free_Tree.WB_Data_New_PBAs_Type;
		Old_PBAs    : CXX_Type_1_Node_Infos_Type;
		Finished    : CXX_Bool_Type;
	end record;


	function CXX_Write_Back_Data_From_Spark (Obj : Free_Tree.Write_Back_Data_Type)
	return CXX_Write_Back_Data_Type;


	function Object_Size (Obj : Free_Tree.Object_Type)
	return CXX_Object_Size_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe11object_sizeERKNS_9Free_treeE";


	procedure Initialize_Object (
		Obj     : out Free_Tree.Object_Type;
		Rt_PBA  :     Physical_Block_Address_Type;
		Rt_Gen  :     Generation_Type;
		Rt_Hash :     Hash_Type;
		Hght    :     Tree_Level_Type;
		Degr    :     Tree_Degree_Type;
		Lfs     :     Tree_Number_Of_Leafs_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe9Free_treeC2EyyRKNS_4HashEjjy";

	procedure Retry_Allocation (Obj : in out Free_Tree.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe9Free_tree16retry_allocationEv";


	function Request_Acceptable (Obj : Free_Tree.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe9Free_tree18request_acceptableEv";

	procedure Submit_Request (
		Obj             : in out Free_Tree.Object_Type;
		Curr_Gen        :        Generation_Type;
		Nr_of_Blks      :        Number_Of_Blocks_Type;
		New_PBAs        :        Free_Tree.WB_Data_New_PBAs_Type;
		Old_PBAs        :        CXX_Type_1_Node_Infos_Type;
		Tree_Height     :        Tree_Level_Type;
		Fr_PBAs         :        Free_Tree.Free_PBAs_Type;
		Req_Prim        :        CXX_Primitive.Object_Type;
		VBA             :        Virtual_Block_Address_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe9Free_tree14submit_requestEyjPKyPKNS_16Type_1_node_infoEjS2_RKNS_9PrimitiveEy";


	procedure Execute (
		Obj              : in out Free_Tree.Object_Type;
		Active_Snaps     :        Snapshots_Type;
		Last_Secured_Gen :        Generation_Type;
		Trans_Data       : in out Translation_Data_Type;
		Cach             : in out Cache.Object_Type;
		Cach_Data        : in out Cache.Cache_Data_Type;
		Query_Data       :        Query_Data_Type;
		Timestamp        :        CXX_Timestamp_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe9Free_tree7executeEPKNS_8SnapshotEyRNS_6Module16Translation_DataERNS4_5CacheERNS4_10Cache_DataERNS_10Query_dataERNS_4TimeE";


	function Execute_Progress(Obj : Free_Tree.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe9Free_tree16execute_progressEv";


	function Peek_Generated_Primitive (Obj : Free_Tree.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe9Free_tree24peek_generated_primitiveEv";


	function Peek_Generated_Data_Index (
		Obj  : Free_Tree.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe9Free_tree25peek_generated_data_indexERKNS_9PrimitiveE";


	procedure Drop_Generated_Primitive (
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe9Free_tree24drop_generated_primitiveERKNS_9PrimitiveE";


	procedure Mark_Generated_Primitive_Complete(
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe9Free_tree33mark_generated_primitive_completeERKNS_9PrimitiveE";


	function Peek_Completed_Primitive (Obj : Free_Tree.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe9Free_tree24peek_completed_primitiveEv";


	function Peek_Completed_WB_Data (
		Obj  : Free_Tree.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Write_Back_Data_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe9Free_tree22peek_completed_wb_dataERKNS_9PrimitiveE";


	procedure Drop_Completed_Primitive (
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe9Free_tree24drop_completed_primitiveERKNS_9PrimitiveE";

end CBE.CXX.CXX_Free_Tree;
