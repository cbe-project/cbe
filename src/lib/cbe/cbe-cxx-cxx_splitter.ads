--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Splitter;
with CBE.CXX.CXX_Request;
with CBE.CXX.CXX_Primitive;

package CBE.CXX.CXX_Splitter
with Spark_Mode
is
--	pragma Pure;


	-------------------
	-- C++ Interface --
	-------------------

	--
	-- Object_Size
	--
	function Object_Size (Obj : Splitter.Object_Type) return CXX_Object_Size_Type
		with Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11object_sizeERKNS0_8SplitterE";

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(
		Obj       : out Splitter.Object_Type;
		Req_Size  :     CXX_Size_Type;
		Prim_Size :     CXX_Size_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8SplitterC1Emm";

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(Obj : Splitter.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module8Splitter18request_acceptableEv";

	--
	-- Number_Of_Primitives
	--
	function Number_Of_Primitives(Req : CXX_Request.Object_Type)
	return CXX_Number_Of_Primitives_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Splitter20number_of_primitivesERKNS_7RequestE";

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Splitter.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module8Splitter24peek_generated_primitiveEv";

	--
	-- Submit_Request
	--
	procedure Submit_Request(
		Obj : out Splitter.Object_Type;
		Req :     CXX_Request.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Splitter14submit_requestERKNS_7RequestE";

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Splitter.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module8Splitter24drop_generated_primitiveERKNS_9PrimitiveE";

end CBE.CXX.CXX_Splitter;
