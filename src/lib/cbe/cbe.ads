--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package CBE
with Spark_Mode
is
	pragma Pure;

	type U8 is mod 2**8;
	type U16 is mod 2**16;
	type U32 is mod 2**32;
	type U64 is mod 2**64;

	procedure print_u8(U : U8) with Import, Convention => C, External_Name => "print_u8";
	procedure print_u16(U : U16) with Import, Convention => C, External_Name => "print_u16";
	procedure print_u32(U : U32) with Import, Convention => C, External_Name => "print_u32";
	procedure print_u64(U : U64) with Import, Convention => C, External_Name => "print_u64";

	type Number_Of_Primitives_Type is mod 2**64;

	type Byte_Type is range 0..255 with Size => 8;

	type Block_Data_Type is array (0..4095) of Byte_Type;

	type Index_Type is mod 2**64;

	type Physical_Block_Address_Type is mod 2**64;

	type Virtual_Block_Address_Type is mod 2**64;

	type Timestamp_Type is mod 2**64;

end CBE;
