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

package body CBE.CXX.CXX_Pool
with Spark_Mode
is
	--
	-- Object_Size
	--
	function Object_Size (Obj : Pool.Object_Type)
	return CXX_Object_Size_Type is (Obj'Size / 8);

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(
		Obj       :    out Pool.Object_Type;
		Req_Size  : in     CXX_Size_Type;
		Prim_Size : in     CXX_Size_Type)
	is
		Req  : constant CXX_Request.Object_Type   := (0,0,0,0,0,0);
		Prim : constant CXX_Primitive.Object_Type := (0,0,0,0,0);
	begin
		if not Correct_Object_Size(Req'Size, Req_Size) then
			raise Program_Error;
		elsif not Correct_Object_Size(Prim'Size, Prim_Size) then
			raise Program_Error;
		end if;
		Pool.Initialize_Object(Obj);
	end Initialize_Object;

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(Obj : Pool.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK(Pool.request_acceptable(Obj)));

	--
	-- Submit_Request
	--
	procedure Submit_Request(
		Obj         : in out Pool.Object_Type;
		Req         :        CXX_Request.Object_Type;
		Nr_Of_Prims :        CXX_Number_Of_Primitives_Type)
	is
	begin
		Pool.Submit_Request(Obj, CXX_Request.To_Spark(Req), Number_Of_Primitives_Type(Nr_Of_Prims));
	end Submit_Request;

	--
	-- Peek_Pending_Request
	--
	function Peek_Pending_Request(Obj : Pool.Object_Type)
	return CXX_Request.Object_Type
	is (CXX_Request.From_Spark(Pool.Peek_Pending_Request(Obj)));


	--
	-- Drop_Pending_Request
	--
	procedure Drop_Pending_Request(
		Obj : in out Pool.Object_Type;
		Req :        CXX_Request.Object_Type)
	is
		Spark_Req : constant Request.Object_Type := CXX_Request.To_Spark(Req);
	begin
		if not Request.Valid(Spark_Req) then
			raise Program_Error;
		end if;
		Pool.Drop_Pending_Request(Obj, Spark_Req);
	end Drop_Pending_Request;


	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Completed_Primitive(
		Obj  : in out Pool.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
		Spark_Prim : constant Primitive.Object_Type := CXX_Primitive.To_Spark(Prim);
	begin
		if not Primitive.Valid(Spark_Prim) then
			raise Program_Error;
		end if;
		Pool.Mark_Completed_Primitive(Obj, Spark_Prim);
	end Mark_Completed_Primitive;

	--
	-- Peek_Completed_Request
	--
	function Peek_Completed_Request(Obj : Pool.Object_Type)
	return CXX_Request.Object_Type
	is (CXX_Request.From_Spark(Pool.Peek_Completed_Request(Obj)));

	--
	-- Drop_Completed_Request
	--
	procedure Drop_Completed_Request(
		Obj : in out Pool.Object_Type;
		Req :        CXX_Request.Object_Type)
	is
		Spark_Req : constant Request.Object_Type := CXX_Request.To_Spark(Req);
	begin
		if not Request.Valid(Spark_Req) then
			raise Program_Error;
		end if;
		Pool.Drop_Completed_Request(Obj, Spark_Req);
	end Drop_Completed_Request;


	--
	-- Request_For_Tag
	--
	function Request_For_Tag(
		Obj : Pool.Object_Type;
		Tag : CXX_Request.Tag_Type)
	return CXX_Request.Object_Type
	is (CXX_Request.From_Spark(Pool.Request_For_Tag(Obj, Request.Tag_Type(Tag))));

end CBE.CXX.CXX_Pool;
