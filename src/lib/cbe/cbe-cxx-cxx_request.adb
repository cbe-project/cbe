--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX.CXX_Request
with Spark_Mode
is
	--
	-- Success_From_Spark
	--
	function Success_From_Spark(Success : in Request.Success_Type)
	return Success_Type
	is (
		case Success is
		when False => 0,
		when True  => 1);


	--
	-- Op_From_Spark
	--
	function Op_From_Spark(Op : in Request.Operation_Type)
	return Operation_Type
	is (
		case Op is
		when Request.Read  => 1,
		when Request.Write => 2,
		when Request.Sync  => 3);


	--
	-- From_Spark
	--
	function From_Spark(Obj : in Request.Object_Type)
	return Object_Type
	is (
		case Request.Valid(Obj) is
		when True => (
			Operation    => Op_From_Spark(Request.Operation(Obj)),
			Success      => Success_From_Spark(Request.Success(Obj)),
			Block_Number => CXX_Block_Number_Type(Request.Block_Number(Obj)),
			Offset       => Uint64_Type(Request.Offset(Obj)),
			Count        => Uint32_Type(Request.Count(Obj)),
			Tag          => CXX_Tag_Type(Request.Tag(Obj))),
		when False => (
			Operation    => 0,
			Success      => 0,
			Block_Number => 0,
			Offset       => 0,
			Count        => 0,
			Tag          => 0));


	--
	-- Success_To_Spark
	--
	function Success_To_Spark(Success : in Success_Type)
	return Request.Success_Type
	is (
		case Success is
		when 0 => False,
		when 1 => True);


	--
	-- Valid_To_Spark
	--
	function Valid_To_Spark(
		Obj : in Object_Type;
		Op  : in Request.Operation_Type)
	return Request.Object_Type
	is (
		Request.Valid_Object (
			Op,
			Success_To_Spark(Obj.Success),
			Block_Number_Type(Obj.Block_Number),
			Request.Offset_Type(Obj.Offset),
			Request.Count_Type(Obj.Count),
			Tag_Type (Obj.Tag)));


	--
	-- To_Spark
	--
	function To_Spark(Obj : in Object_Type)
	return Request.Object_Type
	is (
		case Obj.Operation is
		when 0 => Request.Invalid_Object,
		when 1 => Valid_To_Spark(Obj, Request.Read),
		when 2 => Valid_To_Spark(Obj, Request.Write),
		when 3 => Valid_To_Spark(Obj, Request.Sync));

end CBE.CXX.CXX_Request;
