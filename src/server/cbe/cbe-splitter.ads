--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Request;
with CBE.Primitive;

package CBE.Splitter
with Spark_Mode
is
	pragma Pure;

	type Object_Type is private;

	--
	-- Curr_Primitive
	--
	function Curr_Primitive(Obj : Object_Type)
	return Primitive.Object_Type
	with
		Pre => (Request.Valid(Curr_Request(Obj)));

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(Obj : out Object_Type)
	with
		Post => (Request_Acceptable(Obj));

	--
	-- Number_Of_Primitives
	--
	function Number_Of_Primitives(Req : Request.Object_Type)
	return Number_Of_Primitives_Type
	with
		Pre => (Request.Valid(Req));

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(Obj : Object_Type)
	return Boolean;

	--
	-- Submit_Request
	--
	procedure Submit_Request(
		Obj : in out Object_Type;
		Req :        Request.Object_Type)
	with
		Pre  => (Request_Acceptable(Obj) and Request.Valid(Req)),
		Post => (not Request_Acceptable(Obj));

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Splitter.Object_Type)
	return Primitive.Object_Type;

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Object_Type;
		Prim :        Primitive.Object_Type)
	with
		Pre => (Primitive.Valid(Prim) and Primitive."="(Prim, Curr_Primitive(Obj)));


	---------------
	-- Accessors --
	---------------

	function Curr_Request(Obj : Object_Type) return Request.Object_Type;

private

	--
	-- Object_Type
	--
	type Object_Type is record
		Curr_Req    : Request.Object_Type;
		Curr_Blk_Nr : Request.Block_Number_Type;
		Curr_Idx    : Primitive.Index_Type;
		Nr_Of_Prims : Number_Of_Primitives_Type;
	end record;

end CBE.Splitter;
