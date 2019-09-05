--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package CBE.Request
with Spark_Mode
is
	pragma Pure;

	type    Operation_Type    is (Read, Write, Sync);
	subtype Success_Type      is Boolean;
	type    Block_Number_Type is mod 2**64;
	type    Offset_Type       is mod 2**64;
	type    Count_Type        is mod 2**32;
	type    Tag_Type          is mod 2**32;
	type    Object_Type       is private;

	--
	-- Invalid_Object
	--
	function Invalid_Object
	return Object_Type
	with Post => (not Valid(Invalid_Object'Result));

	--
	-- Valid_Object
	--
	function Valid_Object(
		Op     : Operation_Type;
		Succ   : Success_Type;
		Blk_Nr : Block_Number_Type;
		Off    : Offset_Type;
		Cnt    : Count_Type;
		Tg     : Tag_Type)
	return Object_Type
	with
		Post => (
			Valid(Valid_Object'Result) and then (
				Operation   (Valid_Object'Result) = Op     and
				Success     (Valid_Object'Result) = Succ   and
				Block_Number(Valid_Object'Result) = Blk_Nr and
				Offset      (Valid_Object'Result) = Off    and
				Count       (Valid_Object'Result) = Cnt    and
				Tag         (Valid_Object'Result) = Tg));


	---------------
	-- Accessors --
	---------------

	function Valid(Obj : Object_Type)
	return Boolean;

	function Operation(Obj : Object_Type)
	return Operation_Type
	with Pre => (Valid(Obj));

	function Success(Obj : Object_Type)
	return Success_Type
	with Pre => (Valid(Obj));

	function Block_Number(Obj : Object_Type)
	return Block_Number_Type
	with Pre => (Valid(Obj));

	function Offset(Obj : Object_Type)
	return Offset_Type
	with Pre => (Valid(Obj));

	function Count(Obj : Object_Type)
	return Count_Type
	with Pre => (Valid(Obj));

	function Tag(Obj : Object_Type)
	return Tag_Type
	with Pre => (Valid(Obj));

	procedure Success(
		Obj  : in out Object_Type;
		Succ :        Success_Type)
	with Pre => (Valid(Obj));

private

	--
	-- Object_Type
	--
	type Object_Type is record
		Valid        : Boolean;
		Operation    : Operation_Type;
		Success      : Success_Type;
		Block_Number : Block_Number_Type;
		Offset       : Offset_Type;
		Count        : Count_Type;
		Tag          : Tag_Type;
	end record;

end CBE.Request;
