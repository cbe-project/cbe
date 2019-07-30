--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package CBE.CXX
with Spark_Mode
is
	pragma Pure;

	type Uint8_Type    is mod 2**8  with Size => 1 * 8;
	type Uint32_Type   is mod 2**32 with Size => 4 * 8;
	type Uint64_Type   is mod 2**64 with Size => 8 * 8;
	type CXX_Bool_Type is range 0..1 with Size => Uint8_Type'Size;
	type CXX_Size_Type is new Uint64_Type;
	type CXX_Object_Size_Type is new Uint32_Type;

	type CXX_Index_Type is new Uint32_Type;
	type CXX_Physical_Block_Address_Type is new Uint64_Type;

	type Number_Of_Bytes_Type          is mod 2**64;
	type Number_Of_Bits_Type           is new Natural;
	type CXX_Number_Of_Primitives_Type is new Uint64_Type;

	--
	-- Bits_To_Bytes
	--
	function Number_Of_Bits_To_Bytes(Bits : in Number_Of_Bits_Type)
	return Number_Of_Bytes_Type;

	--
	-- Boolean_To_CXX
	--
	function Boolean_To_CXX(Boolean_Value : in Boolean) return CXX_Bool_Type;

	--
	-- Index_Type_To_CXX
	--
	function Index_Type_To_CXX(Index_Value : in Index_Type) return CXX_Index_Type;

	--
	-- Physical_Block_Address_Type_To_CXX
	--
	function Physical_Block_Address_Type_To_CXX(Pba : in Physical_Block_Address_Type)
	return CXX_Physical_Block_Address_Type;

	--
	-- Correct_Object_Size
	--
	function Correct_Object_Size(
		Spark_Sz : in Number_Of_Bits_Type;
		CXX_Sz   : in CXX_Size_Type)
	return Boolean
	is (Number_Of_Bytes_Type(CXX_Sz) = Number_Of_Bits_To_Bytes(Spark_Sz));

	--
	-- Enough_Storage_Space
	--
	function Enough_Storage_Space(
		Spark_Sz : in Number_Of_Bits_Type;
		CXX_Sz   : in CXX_Size_Type)
	return Boolean
	is (Number_Of_Bytes_Type(CXX_Sz) >= Number_Of_Bits_To_Bytes(Spark_Sz));

end CBE.CXX;
