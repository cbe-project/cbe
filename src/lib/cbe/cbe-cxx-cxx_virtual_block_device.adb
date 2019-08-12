--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX.CXX_Virtual_Block_Device
with Spark_Mode
is
	--
	-- Object_Size
	--
	function Object_Size (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Object_Size_Type
	is (Obj'Size / 8);

	--
	-- Initialize_Object
	--
	procedure Initialize_Object (
		Obj    : out Virtual_Block_Device.Object_Type;
		Height :     CXX_Tree_Level_Type;
		Degree :     CXX_Tree_Degree_Type;
		Leafs  :     CXX_Tree_Number_Of_Leafs_Type)
	is
	begin
		Virtual_Block_Device.Initialize_Object (
			Obj,
			Tree_Level_Type (Height),
			Tree_Degree_Type (Degree),
			Tree_Number_Of_Leafs_Type (Leafs));
	end Initialize_Object;

	--
	-- Trans_Inhibit_Translation
	--
	procedure Trans_Inhibit_Translation (
		Obj : in out Virtual_Block_Device.Object_Type)
	is
	begin
		Virtual_Block_Device.Trans_Inhibit_Translation (Obj);
	end Trans_Inhibit_Translation;

	--
	-- Trans_Resume_Translation
	--
	procedure Trans_Resume_Translation (
		Obj : in out Virtual_Block_Device.Object_Type)
	is
	begin
		Virtual_Block_Device.Trans_Resume_Translation (Obj);
	end Trans_Resume_Translation;

	--
	-- Trans_Get_Virtual_Block_Address
	--
	function Trans_Get_Virtual_Block_Address (
		Obj  : Virtual_Block_Device.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Request.Block_Number_Type
	is (
		CXX_Request.Block_Number_Type (
			Virtual_Block_Device.Trans_Get_Virtual_Block_Address (
				Obj, CXX_Primitive.To_SPARK (Prim))));

	--
	-- Trans_Can_Get_Type_1_Info
	--
	function Trans_Can_Get_Type_1_Info (
		Obj   : Virtual_Block_Device.Object_Type;
		Prim  : CXX_Primitive.Object_Type;
		Infos : Address_Type)
	return CXX_Bool_Type
	is (
		CXX_Bool_From_SPARK (
			Virtual_Block_Device.Trans_Can_Get_Type_1_Info (
				Obj, CXX_Primitive.To_SPARK (Prim), Infos)));

	--
	-- Trans_Get_Type_1_Info
	--
	procedure Trans_Get_Type_1_Info (
		Obj   :        Virtual_Block_Device.Object_Type;
		Infos : in out CXX_Type_1_Node_Infos_Type)
	is
		SPARK_Infos : Type_1_Node_Infos_Type :=
			CXX_Type_1_Node_Infos_To_SPARK (Infos);
	begin
		Virtual_Block_Device.Trans_Get_Type_1_Info (Obj, SPARK_Infos);
		Infos := CXX_Type_1_Node_Infos_From_SPARK (SPARK_Infos);
	end Trans_Get_Type_1_Info;

	--
	-- Tree_Height
	--
	function Tree_Height (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Tree_Level_Type
	is (
		CXX_Tree_Level_Type (
			Virtual_Block_Device.Tree_Height (Obj)));

	--
	-- Index_For_Level
	--
	function Index_For_Level (
		Obj   : Virtual_Block_Device.Object_Type;
		VBA   : CXX_Virtual_Block_Address_Type;
		Level : CXX_Tree_Level_Type)
	return CXX_Tree_Child_Index_Type
	is (
		CXX_Tree_Child_Index_Type (
			Virtual_Block_Device.Index_For_Level (
				Obj,
				Virtual_Block_Address_Type (VBA),
				Tree_Level_Type (Level))));

	--
	-- Get_Tree_Helper
	--
	function Get_Tree_Helper (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Tree_Helper_Type
	is (
		CXX_Tree_Helper_From_SPARK (
			Virtual_Block_Device.Get_Tree_Helper (Obj)));

	--
	-- Primitive_Acceptable
	--
	function Primitive_Acceptable (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Bool_Type
	is (
		CXX_Bool_From_SPARK (
			Virtual_Block_Device.Primitive_Acceptable (Obj)));

	--
	-- Submit_Primitive
	--
	procedure Submit_Primitive (
		Obj  : in out Virtual_Block_Device.Object_Type;
		PBA  :        CXX_Physical_Block_Address_Type;
		Gen  :        CXX_Generation_Type;
		Hash :        Hash_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Virtual_Block_Device.Submit_Primitive (
			Obj,
			Physical_Block_Address_Type (PBA),
			Generation_Type (Gen),
			Hash,
			CXX_Primitive.To_SPARK (Prim));
	end Submit_Primitive;

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive (Obj : Virtual_Block_Device.Object_Type)
	return CXX_Primitive.Object_Type
	is (
		CXX_Primitive.From_SPARK (
			Virtual_Block_Device.Peek_Completed_Primitive (Obj)));

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive (
		Obj : in out Virtual_Block_Device.Object_Type)
	is
	begin
		Virtual_Block_Device.Drop_Completed_Primitive (Obj);
	end Drop_Completed_Primitive;

	--
	-- Execute
	--
	procedure Execute (
		Obj        : in out Virtual_Block_Device.Object_Type;
		Trans_Data : in out Translation_Data_Type;
		Cach       : in out Cache.Object_Type;
		Cach_Data  :        Cache.Cache_Data_Type;
		Timestamp  :        CXX_Timestamp_Type)
	is
	begin
		Virtual_Block_Device.Execute (
			Obj, Trans_Data, Cach, Cach_Data, Timestamp_Type (Timestamp));
	end Execute;

	---------------
	-- Accessors --
	---------------

	function Execute_Progress(Obj : Virtual_Block_Device.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK (Virtual_Block_Device.Execute_Progress (Obj)));

end CBE.CXX.CXX_Virtual_Block_Device;
