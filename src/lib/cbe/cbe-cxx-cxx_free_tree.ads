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


	procedure Initialize_Object (
		Obj     : out Free_Tree.Object_Type;
		Rt_PBA  :     Physical_Block_Address_Type;
		Rt_Gen  :     Generation_Type;
		Rt_Hash :     Hash_Type;
		Hght    :     Tree_Level_Type;
		Degr    :     Tree_Degree_Type;
		Lfs     :     Tree_Number_Of_Leafs_Type);


	procedure Retry_Allocation (Obj : in out Free_Tree.Object_Type);


	function Request_Acceptable (Obj : Free_Tree.Object_Type)
	return CXX_Bool_Type;

	procedure Submit_Request (
		Obj             : in out Free_Tree.Object_Type;
		Curr_Gen        :        Generation_Type;
		Nr_of_Blks      :        Number_Of_Blocks_Type;
		New_PBAs        :        Free_Tree.WB_Data_New_PBAs_Type;
		Old_PBAs        :        CXX_Type_1_Node_Infos_Type;
		Tree_Height     :        Tree_Level_Type;
		Fr_PBAs         :        Free_Tree.Free_PBAs_Type;
		Req_Prim        :        CXX_Primitive.Object_Type;
		VBA             :        Virtual_Block_Address_Type);


	procedure Execute (
		Obj              : in out Free_Tree.Object_Type;
		Active_Snaps     :        Snapshots_Type;
		Last_Secured_Gen :        Generation_Type;
		Trans_Data       : in out Translation_Data_Type;
		Cach             : in out Cache.Object_Type;
		Cach_Data        : in out Cache.Cache_Data_Type;
		Query_Data       :        Query_Data_Type;
		Timestamp        :        CXX_Timestamp_Type);


	function Peek_Generated_Primitive (Obj : Free_Tree.Object_Type)
	return CXX_Primitive.Object_Type;


	function Peek_Generated_Data_Index (
		Obj  : Free_Tree.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Index_Type;


	procedure Drop_Generated_Primitive (
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type);


	procedure Mark_Generated_Primitive_Complete(
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type);


	function Peek_Completed_Primitive (Obj : Free_Tree.Object_Type)
	return CXX_Primitive.Object_Type;


	function Peek_Completed_WB_Data (
		Obj  : Free_Tree.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Write_Back_Data_Type;


	procedure Drop_Completed_Primitive (
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type);

end CBE.CXX.CXX_Free_Tree;
