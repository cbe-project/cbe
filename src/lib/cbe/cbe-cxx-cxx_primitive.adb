--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Request;

package body CBE.CXX.CXX_Primitive
with Spark_Mode
is
	--
	-- From_Spark
	--
	function From_Spark(Obj : Primitive.Object_Type)
	return Object_Type
	is (
		case Primitive.Valid(Obj) is
		when True => (
			Operation    => CXX_Request.Op_From_Spark(Primitive.Operation(Obj)),
			Success      => CXX_Request.Success_From_Spark(Primitive.Success(Obj)),
			Tag          => Uint32_Type(Primitive.Tag(Obj)),
			Block_Number => Uint64_Type(Primitive.Block_Number(Obj)),
			Index        => Uint64_Type(Primitive.Index(Obj))),
		when False => (
			Operation    => 0,
			Success      => 0,
			Tag          => 0,
			Block_Number => 0,
			Index        => 0));

	--
	-- Valid_To_Spark
	--
	function Valid_To_Spark(
		Obj : in Object_Type;
		Op  : in Request.Operation_Type)
	return Primitive.Object_Type
	is (
		Primitive.Valid_Object(
			Op,
			CXX_Request.Success_To_Spark(Obj.Success),
			Request.Tag_Type(Obj.Tag),
			Request.Block_Number_Type(Obj.Block_Number),
			Primitive.Index_Type(Obj.Index)));


	--
	-- To_Spark
	--
	function To_Spark(
		Obj : in Object_Type)
	return Primitive.Object_Type
	is (
		case Obj.Operation is
		when 0 => Primitive.Invalid_Object,
		when 1 => Valid_To_Spark(Obj, Request.Read),
		when 2 => Valid_To_Spark(Obj, Request.Write),
		when 3 => Valid_To_Spark(Obj, Request.Sync));

end CBE.CXX.CXX_Primitive;
