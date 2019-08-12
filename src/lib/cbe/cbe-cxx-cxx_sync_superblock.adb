--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX.CXX_Sync_Superblock
with Spark_Mode
is

	--
	-- Object_Size
	--
	function Object_Size (Obj : Sync_Superblock.Object_Type)
	return CXX_Object_Size_Type is (Obj'Size / 8);

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(Obj : out Sync_Superblock.Object_Type)
	is
	begin
		Sync_Superblock.Initialize_Object(Obj);
	end Initialize_Object;

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(Obj : Sync_Superblock.Object_Type)
	return CXX_Bool_Type
	is
	begin
		return Boolean_To_CXX(Sync_Superblock.Request_Acceptable(Obj));
	end Request_Acceptable;

	--
	-- Submit_Primitive
	--
	procedure Submit_Request(
		Obj : in out Sync_Superblock.Object_Type;
		Idx :        CXX_Superblock_Index_Type;
		Gen :        CXX_Generation_Type)
	is
	begin
		Sync_Superblock.Submit_Request(Obj, Superblock_Index_Type(Idx), Generation_Type(Gen));
	end Submit_Request;

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive(Obj : Sync_Superblock.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return (CXX_Primitive.From_Spark(Sync_Superblock.Peek_Completed_Primitive(Obj)));
	end Peek_Completed_Primitive;

	--
	-- Peek_Completed_Generation
	--
	function Peek_Completed_Generation(
		Obj  : Sync_Superblock.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Generation_Type
	is
	begin
		return Generation_Type_To_CXX(Sync_Superblock.Peek_Completed_Generation(Obj, CXX_Primitive.To_Spark(Prim)));
	end Peek_Completed_Generation;

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive(
		Obj  : in out Sync_Superblock.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Sync_Superblock.Drop_Completed_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Drop_Completed_Primitive;

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Sync_Superblock.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return (CXX_Primitive.From_Spark(Sync_Superblock.Peek_Generated_Primitive(Obj)));
	end Peek_Generated_Primitive;

	--
	-- Peek_Generated_Index
	--
	function Peek_Generated_Index(
		Obj  : Sync_Superblock.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Superblock_Index_Type
	is
		Idx : constant Superblock_Index_Type := Sync_Superblock.Peek_Generated_Index(Obj, CXX_Primitive.To_Spark(Prim));
	begin
		return Superblock_Index_Type_To_CXX(Idx);
	end Peek_Generated_Index;

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Sync_Superblock.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Sync_Superblock.Drop_Generated_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Drop_Generated_Primitive;

	--
	-- Mark_Generated_Primitive_Complete
	--
	procedure Mark_Generated_Primitive_Complete(
		Obj  : in out Sync_Superblock.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Sync_Superblock.Mark_Generated_Primitive_Complete(Obj, CXX_Primitive.To_Spark(Prim));
	end Mark_Generated_Primitive_Complete;

end CBE.CXX.CXX_Sync_Superblock;
