--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with SHA256_4K;

package CBE
with Spark_Mode
is
	pragma Pure;

	type U8  is mod 2**8;
	type U16 is mod 2**16;
	type U32 is mod 2**32;
	type U64 is mod 2**64;

	procedure print_u8  (U : U8)  with Import, Convention => C, External_Name => "print_u8";
	procedure print_u16 (U : U16) with Import, Convention => C, External_Name => "print_u16";
	procedure print_u32 (U : U32) with Import, Convention => C, External_Name => "print_u32";
	procedure print_u64 (U : U64) with Import, Convention => C, External_Name => "print_u64";

	subtype Byte_Type                is SHA256_4K.Byte;
	type Block_Data_Type             is array (1..4096) of Byte_Type with Size => 4096 * 8;
	type Translation_Data_Type       is array (0..0) of Block_Data_Type with Size => 1 * 4096 * 8;
	type Number_Of_Primitives_Type   is mod 2**64;
	type Index_Type                  is mod 2**64;
	type Generation_Type             is mod 2**64;
	type Superblock_Index_Type       is mod 2**64;
	type Physical_Block_Address_Type is mod 2**64;
	type Virtual_Block_Address_Type  is mod 2**64;
	type Timestamp_Type              is mod 2**64;
	type Tree_Number_Of_Leafs_Type   is mod 2**64;
	type Tree_Degree_Type            is mod 2**32;
	type Tree_Degree_Mask_Type       is mod 2**32;
	type Tree_Degree_Log_2_Type      is mod 2**32;
	type Tree_Level_Type             is mod 2**32;
	type Tree_Child_Index_Type       is mod 2**32;
	type Tag_Type                    is mod 2**32;

	-- FIXME should be architecture-dependent
	type Address_Type is mod 2**64;

	type Hash_Type is array (1..32) of Byte_Type with Size => 32 * 8;

	type Type_I_Node_Padding_Type is array (0..15) of Byte_Type;

	type Type_I_Node_Type is record
		PBA      : Physical_Block_Address_Type;
		Gen      : Generation_Type;
		Hash     : Hash_Type;
		Padding  : Type_I_Node_Padding_Type;
	end record with Size => 8 * 8 + 8 * 8 + 32 * 8 + 16 * 8;

	type Type_I_Node_Block_Type is
		array (0..(Block_Data_Type'Size / Type_I_Node_Type'Size) - 1)
		of Type_I_Node_Type;

	type Type_1_Node_Info_Type is record
		PBA  : Physical_Block_Address_Type;
		Gen  : Generation_Type;
		Hash : Hash_Type;
	end record;

	type Type_1_Node_Infos_Type is array (0..5) of Type_1_Node_Info_Type;

	function Type_1_Node_Info_Invalid
	return Type_1_Node_Info_Type
	is (
		PBA  => 0,
		Gen  => 0,
		Hash => (others => 0));

	function PBA_Invalid return Physical_Block_Address_Type is (Physical_Block_Address_Type'Last);
	function Tree_Level_Invalid return Tree_Level_Type is (Tree_Level_Type'Last);
	function Tag_Translation return Tag_Type is (16#60#);

end CBE;
