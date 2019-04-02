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
	-- Invalid_Object
	--
	function Invalid_Object
	return Object_Type
	is (
		Valid        => False,
		Operation    => Request.Read,
		Success      => Request.False,
		Tag          => 0,
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


	---------------
	-- Accessors --
	---------------

	function Valid       (Obj : Object_Type) return Boolean                   is (Obj.Valid);
	function Operation   (Obj : Object_Type) return Request.Operation_Type    is (Obj.Operation);
	function Success     (Obj : Object_Type) return Request.Success_Type      is (Obj.Success);
	function Tag         (Obj : Object_Type) return Request.Tag_Type          is (Obj.Tag);
	function Block_Number(Obj : Object_Type) return Request.Block_Number_Type is (Obj.Block_Number);
	function Index       (Obj : Object_Type) return Index_Type                is (Obj.Index);

end CBE.Primitive;
