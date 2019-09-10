--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.Primitive
with Spark_Mode
is
	--
	-- Copy_Valid_Object_Change_Tag
	--
	function Copy_Valid_Object_Change_Tag (
		Obj : Object_Type;
		Tag : Tag_Type)
	return Object_Type
	is (
		Valid        => True,
		Operation    => Obj.Operation,
		Success      => Obj.Success,
		Tag          => Request.Tag_Type (Tag),
		Block_Number => Obj.Block_Number,
		Index        => Obj.Index);

	--
	-- Invalid_Object
	--
	function Invalid_Object
	return Object_Type
	is (
		Valid        => False,
		Operation    => Request.Read,
		Success      => False,
		Tag          => Request.Tag_Type (Tag_Invalid),
		Block_Number => 0,
		Index        => 0);

	--
	-- Valid_Object
	--
	function Valid_Object(
		Op     : Request.Operation_Type;
		Succ   : Request.Success_Type;
		Tg     : Request.Tag_Type;
		Blk_Nr : Request.Block_Number_Type;
		Idx    : Index_Type)
	return Object_Type
	is (
		Valid        => True,
		Operation    => Op,
		Success      => Succ,
		Tag          => Tg,
		Block_Number => Blk_Nr,
		Index        => Idx);

	--
	-- Equal
	--
	function Equal(
		Obj_1 : Object_Type;
		Obj_2 : Object_Type)
	return Boolean
	is (
		Obj_1.Block_Number = Obj_2.Block_Number and
		Obj_1.Index        = Obj_2.Index        and
		Request."=" (Obj_1.Tag, Obj_2.Tag)      and
		Obj_1.Operation    = Obj_2.Operation);


	--------------------
	-- Read Accessors --
	--------------------

	function Valid       (Obj : Object_Type) return Boolean                   is (Obj.Valid);
	function Operation   (Obj : Object_Type) return Request.Operation_Type    is (Obj.Operation);
	function Success     (Obj : Object_Type) return Request.Success_Type      is (Obj.Success);
	function Tag         (Obj : Object_Type) return Request.Tag_Type          is (Obj.Tag);
	function Block_Number(Obj : Object_Type) return Request.Block_Number_Type is (Obj.Block_Number);
	function Index       (Obj : Object_Type) return Index_Type                is (Obj.Index);


	---------------------
	-- Write Accessors --
	---------------------

	procedure Success     (Obj : in out Object_Type; Value : Request.Success_Type)      is begin Obj.Success      := Value; end Success;
	procedure Block_Number(Obj : in out Object_Type; Value : Request.Block_Number_Type) is begin Obj.Block_Number := Value; end Block_Number;
	procedure Operation   (Obj : in out Object_Type; Value : Request.Operation_Type)    is begin Obj.Operation    := Value; end Operation;


	function To_String (Obj : Object_Type)
	return String
	is
	begin
		if not Obj.Valid then
			return "Invalid Primitive";
		end if;

		return "Primitive(Op=" & Obj.Operation'Image &
			", Tag="           & Cbe.To_String(U64(Obj.Tag)) &
			", Success="       & Cbe.To_String(Obj.Success) &
			", Block_Number="  & Cbe.To_String(U64(Obj.Block_Number)) &
			", Index="         & Cbe.To_String(U64(Obj.Index)) &
			")";

	end To_String;

end CBE.Primitive;
