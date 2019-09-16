--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX with Spark_Mode is

	--
	-- CXX_Bool_From_SPARK
	--
	function CXX_Bool_From_SPARK(Value : Boolean)
	return CXX_Bool_Type
	is (
		case Value is
		when False => 0,
		when True  => 1);

	--
	-- CXX_Bool_To_SPARK
	--
	function CXX_Bool_To_SPARK(Value : CXX_Bool_Type)
	return Boolean
	is (
		case Value is
		when 0 => False,
		when 1 => True);

	--
	-- CXX_Index_From_SPARK
	--
	function CXX_Index_From_SPARK(Value : Index_Type)
	return CXX_Index_Type
	is (CXX_Index_Type(Value));

	--
	-- CXX_Superblock_Index_From_SPARK
	--
	function CXX_Superblock_Index_From_SPARK(Value : Superblock_Index_Type)
	return CXX_Superblock_Index_Type
	is (CXX_Superblock_Index_Type(Value));

	--
	-- Bits_To_Bytes
	--
	function Number_Of_Bits_To_Bytes(Bits : Number_Of_Bits_Type)
	return Number_Of_Bytes_Type
	is (
		if Bits mod Byte_Type'Size = 0
		then Number_Of_Bytes_Type(Bits / Byte_Type'Size)
		else Number_Of_Bytes_Type(Bits / Byte_Type'Size) + 1);

end CBE.CXX;
