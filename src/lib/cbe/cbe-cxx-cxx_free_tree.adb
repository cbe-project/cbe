--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX.CXX_Free_Tree
with Spark_Mode
is
	function Object_Size (Obj : Free_Tree.Object_Type)
	return CXX_Object_Size_Type
	is (Obj'Size / 8);

	function CXX_Write_Back_Data_From_Spark (Obj : Free_Tree.Write_Back_Data_Type)
	return CXX_Write_Back_Data_Type
	is (
		Prim        => CXX_Primitive.From_Spark (Obj.Prim),
		Gen         => Obj.Gen,
		VBA         => Obj.VBA,
		Tree_Height => Obj.Tree_Height ,
        New_PBAs    => Obj.New_PBAs    ,
        Old_PBAs    => CXX_Type_1_Node_Infos_From_SPARK (Obj.Old_PBAs),
        Finished    => CXX_Bool_From_SPARK (Obj.Finished));


	procedure Initialize_Object (
		Obj     : out Free_Tree.Object_Type;
		Rt_PBA  :     Physical_Block_Address_Type;
		Rt_Gen  :     Generation_Type;
		Rt_Hash :     Hash_Type;
		Hght    :     Tree_Level_Type;
		Degr    :     Tree_Degree_Type;
		Lfs     :     Tree_Number_Of_Leafs_Type)
	is
	begin
		Free_Tree.Initialize_Object (
			Obj     ,
            Rt_PBA  ,
            Rt_Gen  ,
            Rt_Hash ,
            Hght    ,
            Degr    ,
            Lfs     );
	end Initialize_Object;


	procedure Retry_Allocation (Obj : in out Free_Tree.Object_Type)
	is
	begin
		Free_Tree.Retry_Allocation (Obj);
	end Retry_Allocation;


	function Request_Acceptable (Obj : Free_Tree.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_Spark (Free_Tree.Request_Acceptable (Obj)));

	procedure Submit_Request (
		Obj             : in out Free_Tree.Object_Type;
		Curr_Gen        :        Generation_Type;
		Nr_of_Blks      :        Number_Of_Blocks_Type;
		New_PBAs        :        Write_Back.New_PBAs_Type;
		Old_PBAs        :        CXX_Type_1_Node_Infos_Type;
		Tree_Height     :        Tree_Level_Type;
		Fr_PBAs         :        Free_Tree.Free_PBAs_Type;
		Req_Prim        :        CXX_Primitive.Object_Type;
		VBA             :        Virtual_Block_Address_Type)
	is
	begin
		Free_Tree.Submit_Request (
			Obj             ,
            Curr_Gen        ,
            Nr_of_Blks      ,
            New_PBAs        ,
            CXX_Type_1_Node_Infos_To_SPARK (Old_PBAs),
            Tree_Height     ,
            Fr_PBAs         ,
            CXX_Primitive.To_Spark (Req_Prim)        ,
            VBA             );
	end Submit_Request;


	procedure Execute (
		Obj              : in out Free_Tree.Object_Type;
		Active_Snaps     :        Snapshots_Type;
		Last_Secured_Gen :        Generation_Type;
		Trans_Data       : in out Translation_Data_Type;
		Cach             : in out Cache.Object_Type;
		Cach_Data        : in out Cache.Cache_Data_Type;
		Query_Data       :        Query_Data_Type;
		Timestamp        :        CXX_Timestamp_Type)
	is
	begin
		Free_Tree.Execute (
			Obj              ,
            Active_Snaps     ,
            Last_Secured_Gen ,
            Trans_Data       ,
            Cach             ,
            Cach_Data        ,
            Query_Data       ,
            Timestamp_Type (Timestamp)        );
	end Execute;


	function Execute_Progress(Obj : Free_Tree.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK (Free_Tree.Execute_Progress (Obj)));


	function Peek_Generated_Primitive (Obj : Free_Tree.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return CXX_Primitive.From_Spark (Free_Tree.Peek_Generated_Primitive (Obj));

	end Peek_Generated_Primitive;


	function Peek_Generated_Data_Index (
		Obj  : Free_Tree.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Index_Type
	is
	begin
		return Free_Tree.Peek_Generated_Data_Index (Obj, CXX_Primitive.To_Spark (Prim));

	end Peek_Generated_Data_Index;


	procedure Drop_Generated_Primitive (
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Free_Tree.Drop_Generated_Primitive (Obj, CXX_Primitive.To_Spark (Prim));

	end Drop_Generated_Primitive;


	procedure Mark_Generated_Primitive_Complete(
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Free_Tree.Mark_Generated_Primitive_Complete (Obj, CXX_Primitive.To_Spark (Prim));

	end Mark_Generated_Primitive_Complete;


	function Peek_Completed_Primitive (Obj : Free_Tree.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return CXX_Primitive.From_Spark (Free_Tree.Peek_Completed_Primitive (Obj));

	end Peek_Completed_Primitive;


	function Peek_Completed_WB_Data (
		Obj  : Free_Tree.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Write_Back_Data_Type
	is
	begin
		return CXX_Write_Back_Data_From_Spark (
			Free_Tree.Peek_Completed_WB_Data (Obj, CXX_Primitive.To_Spark (Prim)));

	end Peek_Completed_WB_Data;


	procedure Drop_Completed_Primitive (
		Obj  : in out Free_Tree.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Free_Tree.Drop_Completed_Primitive (Obj, CXX_Primitive.To_Spark (Prim));
	end Drop_Completed_Primitive;

end CBE.CXX.CXX_Free_Tree;
