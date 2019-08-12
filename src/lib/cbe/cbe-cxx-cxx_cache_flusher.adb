--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Cache;

package body CBE.CXX.CXX_Cache_Flusher
with Spark_Mode
is

	--
	-- Object_Size
	--
	function Object_Size (Obj : Cache_Flusher.Object_Type)
	return CXX_Object_Size_Type is (Obj'Size / 8);

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(Obj : out Cache_Flusher.Object_Type)
	is
	begin
		Cache_Flusher.Initialize_Object(Obj);
	end Initialize_Object;

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(Obj : Cache_Flusher.Object_Type)
	return CXX_Bool_Type
	is
	begin
		return CXX_Bool_From_SPARK(Cache_Flusher.Request_Acceptable(Obj));
	end Request_Acceptable;

	--
	-- Submit_Primitive
	--
	procedure Submit_Request(
		Obj : in out Cache_Flusher.Object_Type;
		Pba :        CXX_Physical_Block_Address_Type;
		Idx :        CXX_Index_Type)
	is
	begin
		Cache_Flusher.Submit_Request(Obj, Physical_Block_Address_Type(Pba), Cache.Cache_Index_Type(Idx));
	end Submit_Request;

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive(Obj : Cache_Flusher.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return (CXX_Primitive.From_Spark(Cache_Flusher.Peek_Completed_Primitive(Obj)));
	end Peek_Completed_Primitive;

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive(
		Obj  : in out Cache_Flusher.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Cache_Flusher.Drop_Completed_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Drop_Completed_Primitive;

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Cache_Flusher.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return (CXX_Primitive.From_Spark(Cache_Flusher.Peek_Generated_Primitive(Obj)));
	end Peek_Generated_Primitive;

	--
	-- Peek_Generated_Data_Index
	--
	function Peek_Generated_Data_Index(
		Obj  : Cache_Flusher.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Index_Type
	is
		Idx : constant Cache.Cache_Index_Type := Cache_Flusher.Peek_Generated_Data_Index(Obj, CXX_Primitive.To_Spark(Prim));
	begin
		return CXX_Index_From_SPARK(Index_Type(Idx));
	end Peek_Generated_Data_Index;

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Cache_Flusher.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Cache_Flusher.Drop_Generated_Primitive(Obj, CXX_Primitive.To_Spark(Prim));
	end Drop_Generated_Primitive;

	--
	-- Mark_Generated_Primitive_Complete
	--
	procedure Mark_Generated_Primitive_Complete(
		Obj  : in out Cache_Flusher.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Cache_Flusher.Mark_Generated_Primitive_Complete(Obj, CXX_Primitive.To_Spark(Prim));
	end Mark_Generated_Primitive_Complete;

end CBE.CXX.CXX_Cache_Flusher;
