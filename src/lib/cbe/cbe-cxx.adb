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
	-- Boolean_To_CXX
	--
	function Boolean_To_CXX(Boolean_Value : in Boolean) return CXX_Bool_Type is (
		case Boolean_Value is
		when False => 0,
		when True  => 1);

	--
	-- Index_Type_To_CXX
	--
	function Index_Type_To_CXX(Index_Value : in Index_Type)
	return CXX_Index_Type
	is
	begin
		return (CXX_Index_Type(Index_Value));
	end Index_Type_To_CXX;

	--
	-- Bits_To_Bytes
	--
	function Number_Of_Bits_To_Bytes(Bits : in Number_Of_Bits_Type)
	return Number_Of_Bytes_Type is
		(if Bits mod Byte_Type'Size = 0
			then Number_Of_Bytes_Type(Bits / Byte_Type'Size)
			else Number_Of_Bytes_Type(Bits / Byte_Type'Size) + 1);

end CBE.CXX;
