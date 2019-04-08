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

package body CBE.CXX.CXX_Splitter
with Spark_Mode => Off
is
	--
	-- Initialize_Object
	--
	procedure Initialize_Object(
		Obj       : in out Splitter.Object_Type;
		Obj_Size  : in     CXX_Size_Type;
		Req_Size  : in     CXX_Size_Type;
		Prim_Size : in     CXX_Size_Type)
	is
		Req  : constant CXX_Request.Object_Type   := (0,0,0,0,0,0);
		Prim : constant CXX_Primitive.Object_Type := (0,0,0,0,0);
	begin
		if not Correct_Object_Size(Obj'Size, Obj_Size) then
			raise Program_Error;
		elsif not Correct_Object_Size(Req'Size, Req_Size) then
			raise Program_Error;
		elsif not Correct_Object_Size(Prim'Size, Prim_Size) then
			raise Program_Error;
		end if;

		Splitter.Initialize_Object(Obj);

	end Initialize_Object;


	--
	-- Request_Acceptable
	--
	function Request_Acceptable(Obj : in Splitter.Object_Type)
	return CXX_Bool_Type
	is (Boolean_To_CXX(Splitter.request_acceptable(Obj)));


	--
	-- Number_Of_Primitives
	--
	function Number_Of_Primitives(Req : in CXX_Request.Object_Type)
	return CXX_Number_Of_Primitives_Type
	is Spark_Req : constant Request.Object_Type := CXX_Request.To_Spark(Req);
	begin
		case Request.Valid(Spark_Req) is
		when False => raise Program_Error;
		when True  => null;
		end case;

		return CXX_Number_Of_Primitives_Type(
			Splitter.Number_Of_Primitives(Spark_Req));

	end Number_Of_Primitives;


	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Splitter.Object_Type)
	return CXX_Primitive.Object_Type
	is (CXX_Primitive.From_Spark(Splitter.Peek_Generated_Primitive(Obj)));


	--
	-- Submit_Request
	--
	procedure Submit_Request(
		Obj : out Splitter.Object_Type;
		Req :     CXX_Request.Object_Type)
	is
	begin
		Splitter.Submit_Request(Obj, CXX_Request.To_Spark(Req));
	end Submit_Request;


	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Splitter.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
		Spark_Prim : constant Primitive.Object_Type := CXX_Primitive.To_Spark(Prim);
	begin
		if not Primitive.Valid(Spark_Prim) then
			raise Program_Error;
		end if;
		if Primitive."/="(Splitter.Curr_Primitive(Obj), Spark_Prim) then
			raise Program_Error;
		end if;

		Splitter.Drop_Generated_Primitive(Obj, Spark_Prim);
	end Drop_Generated_Primitive;

end CBE.CXX.CXX_Splitter;
