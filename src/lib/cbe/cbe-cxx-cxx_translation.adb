--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.Request;

package body CBE.CXX.CXX_Translation
with Spark_Mode
is
	--
	-- Object_Size
	--
	function Object_Size (Obj : Translation.Object_Type)
	return CXX_Object_Size_Type
	is (Obj'Size / 8);

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(
		Obj     : out Translation.Object_Type;
		Helpr   :     CXX_Tree_Helper_Type;
		Free_Tr :     CXX_Bool_Type)
	is
	begin
		Translation.Initialize_Object (
			Obj,
			CXX_Tree_Helper_To_SPARK (Helpr),
			CXX_Bool_To_SPARK (Free_Tr));
	end Initialize_Object;

	--
	-- Height
	--
	function Height (Obj : Translation.Object_Type)
	return CXX_Tree_Level_Type
	is (
		CXX_Tree_Level_Type (Translation.Height (Obj)));

	--
	-- Index
	--
	function Index (
		Obj   : Translation.Object_Type;
		VBA   : CXX_Virtual_Block_Address_Type;
		Level : CXX_Tree_Level_Type)
	return CXX_Tree_Child_Index_Type
	is (
		CXX_Tree_Child_Index_Type (
			Translation.Index (
				Obj,
				Virtual_Block_Address_Type (VBA),
				Tree_Level_Type (Level))));

	--
	-- Acceptable
	--
	function Acceptable (Obj : Translation.Object_Type)
	return CXX_Bool_Type
	is (
		CXX_Bool_From_SPARK (Translation.Acceptable (Obj)));

	--
	-- Suspend
	--
	procedure Suspend(Obj : in out Translation.Object_Type)
	is
	begin
		Translation.Suspend (Obj);
	end Suspend;

	--
	-- Resume
	--
	procedure Resume(Obj : in out Translation.Object_Type)
	is
	begin
		Translation.Resume (Obj);
	end Resume;

	--
	-- Submit_Primitive
	--
	procedure Submit_Primitive (
		Obj       : in out Translation.Object_Type;
		Root_PBA  :        CXX_Physical_Block_Address_Type;
		Root_Gen  :        CXX_Generation_Type;
		Root_Hash :        Hash_Type;
		Prim      :        CXX_Primitive.Object_Type)
	is
	begin
		if Primitive.Valid (Translation.Current (Obj)) then
			raise Program_Error;
		end if;

		Translation.Submit_Primitive (
			Obj,
			Physical_Block_Address_Type (Root_PBA),
			Generation_Type (Root_Gen),
			Root_Hash,
			CXX_Primitive.To_SPARK (Prim));
	end Submit_Primitive;

	--
	-- Execute
	--
	procedure Execute (
		Obj        : in out Translation.Object_Type;
		Trans_Data :        Translation_Data_Type)
	is
	begin
		Translation.Execute (Obj, Trans_Data);
	end Execute;

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive (Obj : Translation.Object_Type)
	return CXX_Primitive.Object_Type
	is (
		CXX_Primitive.From_Spark (
			Translation.Peek_Completed_Primitive (Obj)));

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive (
		Obj  : in out Translation.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
		SPARK_Prim : constant Primitive.Object_Type :=
			CXX_Primitive.To_SPARK (Prim);
	begin
		if
			Request."/="(
				Primitive.Block_Number (SPARK_Prim),
				Request.Block_Number_Type (Translation.Data_PBA (Obj)))
		then
			-- Print_String("Trans.DrpComplPrim: invalid primitive: "
			-- Print_Primitive(Prim);
			-- Print_Line_Break;
			raise Program_Error;
		end if;

		Translation.Drop_Completed_Primitive (Obj);
	end Drop_Completed_Primitive;

	--
	-- Get_Virtual_Block_Address
	--
	function Get_Virtual_Block_Address (
		Obj  : Translation.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Request.Block_Number_Type
	is
		SPARK_Prim : constant Primitive.Object_Type :=
			CXX_Primitive.To_SPARK (Prim);
	begin
		if
			Request."/=" (
				Primitive.Block_Number (SPARK_Prim),
				Request.Block_Number_Type (Translation.Data_PBA (Obj)))
		then
			raise Program_Error;
		end if;

		return
			CXX_Request.Block_Number_Type (
				Translation.Get_Virtual_Block_Address (Obj));
	end Get_Virtual_Block_Address;

	--
	-- Can_Get_Type_1_Info
	--
	function Can_Get_Type_1_Info (
		Obj   : Translation.Object_Type;
		Prim  : CXX_Primitive.Object_Type;
		Infos : Address_Type)
	return CXX_Bool_Type
	is (
		CXX_Bool_From_SPARK (
			Translation.Can_Get_Type_1_Info (
				Obj, CXX_Primitive.To_SPARK (Prim), Infos)));

	--
	-- Get_Type_1_Info
	--
	procedure Get_Type_1_Info (
		Obj   :        Translation.Object_Type;
		Infos : in out CXX_Type_1_Node_Infos_Type)
	is
		SPARK_Infos : Type_1_Node_Infos_Type :=
			CXX_Type_1_Node_Infos_To_SPARK (Infos);
	begin
		Translation.Get_Type_1_Info (Obj, SPARK_Infos);
		Infos := CXX_Type_1_Node_Infos_From_SPARK (SPARK_Infos);
	end Get_Type_1_Info;

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive (Obj : Translation.Object_Type)
	return CXX_Primitive.Object_Type
	is (
		CXX_Primitive.From_SPARK (
			Translation.Peek_Generated_Primitive (Obj)));

	--
	-- Discard_Generated_Primitive
	--
	procedure Discard_Generated_Primitive (
		Obj  : in out Translation.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
		SPARK_Prim : constant Primitive.Object_Type :=
			CXX_Primitive.To_SPARK (Prim);
	begin
		if
			Request."/="(
				Primitive.Block_Number (SPARK_Prim),
				Request.Block_Number_Type (Translation.Next_PBA (Obj)))
		then
			-- Print_String("Trans.DiscGenPrim: invalid primitive: "
			-- Print_Primitive(Prim);
			-- Print_Line_Break;
			raise Program_Error;
		end if;

		Translation.Discard_Generated_Primitive (Obj);
	end Discard_Generated_Primitive;

	--
	-- Mark_Generated_Primitive_Complete
	--
	procedure Mark_Generated_Primitive_Complete (
		Obj        : in out Translation.Object_Type;
		Prim       :        CXX_Primitive.Object_Type;
		Data       :        Block_Data_Type;
		Trans_Data : in out Translation_Data_Type)
	is
		SPARK_Prim : constant Primitive.Object_Type :=
			CXX_Primitive.To_SPARK (Prim);
	begin
		if
			Request."/="(
				Primitive.Block_Number (SPARK_Prim),
				Request.Block_Number_Type (Translation.Next_PBA (Obj)))
		then
			-- Print_String("Trans.MarkGenPrimCompl: invalid primitive: "
			-- Print_Primitive(Prim);
			-- Print_Line_Break;
			raise Program_Error;
		end if;

		Translation.Mark_Generated_Primitive_Complete (
			Obj, Data, Trans_Data);
	end Mark_Generated_Primitive_Complete;

	---------------
	-- Accessors --
	---------------

	function Execute_Progress (Obj : Translation.Object_Type)
	return CXX_Bool_Type
	is (
		CXX_Bool_From_SPARK (
			Translation.Execute_Progress (Obj)));

	function Max_Levels (Obj : Translation.Object_Type)
	return CXX_Tree_Level_Type
	is (
		CXX_Tree_Level_Type (
			Translation.Max_Levels (Obj)));

end CBE.CXX.CXX_Translation;
