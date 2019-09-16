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

	type Uint8_Type                      is mod 2**8  with Size => 1 * 8;
	type Uint32_Type                     is mod 2**32 with Size => 4 * 8;
	type Uint64_Type                     is mod 2**64 with Size => 8 * 8;
	type Number_Of_Bytes_Type            is mod 2**64;
	type Number_Of_Bits_Type             is new Natural;
	type CXX_Bool_Type                   is range 0..1 with Size => Uint8_Type'Size;
	type CXX_Object_Size_Type            is new Uint32_Type;
	type CXX_Index_Type                  is new Uint32_Type;
	type CXX_Number_Of_Primitives_Type   is new Uint64_Type;
	type CXX_Generation_Type             is new Uint64_Type;
	type CXX_Superblock_Index_Type       is new Uint64_Type;
	type CXX_Block_Number_Type           is new Uint64_Type;
	type CXX_Virtual_Block_Address_Type  is new Uint64_Type;
	type CXX_Size_Type                   is new Uint64_Type;
	type CXX_Timestamp_Type              is new Uint64_Type;
	type CXX_Tag_Type                    is new Uint32_Type;
	type CXX_Operation_Type              is range 0..3 with Size => 32;

	type CXX_Timeout_Request_Type is record
		Valid   : CXX_Bool_Type;
		Timeout : Timestamp_Type;
	end record;

	--
	-- Bits_To_Bytes
	--
	function Number_Of_Bits_To_Bytes(Bits : Number_Of_Bits_Type)
	return Number_Of_Bytes_Type;

	--
	-- CXX_Bool_From_SPARK
	--
	function CXX_Bool_From_SPARK(Value : Boolean)
	return CXX_Bool_Type;

	--
	-- CXX_Bool_To_SPARK
	--
	function CXX_Bool_To_SPARK(Value : CXX_Bool_Type)
	return Boolean;

	--
	-- CXX_Index_From_SPARK
	--
	function CXX_Index_From_SPARK(Value : Index_Type)
	return CXX_Index_Type;

	--
	-- CXX_Superblock_Index_From_SPARK
	--
	function CXX_Superblock_Index_From_SPARK(Value : Superblock_Index_Type)
	return CXX_Superblock_Index_Type;

	--
	-- Correct_Object_Size
	--
	function Correct_Object_Size(
		Spark_Sz : Number_Of_Bits_Type;
		CXX_Sz   : CXX_Size_Type)
	return Boolean
	is (Number_Of_Bytes_Type(CXX_Sz) = Number_Of_Bits_To_Bytes(Spark_Sz));

	--
	-- Enough_Storage_Space
	--
	function Enough_Storage_Space(
		Spark_Sz : Number_Of_Bits_Type;
		CXX_Sz   : CXX_Size_Type)
	return Boolean
	is (Number_Of_Bytes_Type(CXX_Sz) >= Number_Of_Bits_To_Bytes(Spark_Sz));


	function CXX_Timeout_Request_From_SPARK (Obj : Timeout_Request_Type)
	return CXX_Timeout_Request_Type
	is (
		Valid => CXX_Bool_From_SPARK (Obj.Valid),
		Timeout => Obj.Timeout);

end CBE.CXX;
