--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package CBE.Debug
with SPARK_Mode
is
   pragma Pure;

   type Uint8_Type  is mod 2**8  with Size => 8;
   type Uint16_Type is mod 2**16 with Size => 16;
   type Uint32_Type is mod 2**32 with Size => 32;
   type Uint64_Type is mod 2**64 with Size => 64;

   procedure Print_Uint8  (Int : Uint8_Type)
   with
      Import,
      Convention => C,
      External_Name => "print_u8",
      SPARK_Mode => Off;

   procedure Print_Uint16 (Int : Uint16_Type)
   with
      Import,
      Convention => C,
      External_Name => "print_u16",
      SPARK_Mode => Off;

   procedure Print_Uint32 (Int : Uint32_Type)
   with
      Import,
      Convention => C,
      External_Name => "print_u32",
      SPARK_Mode => Off;

   procedure Print_Uint64 (Int : Uint64_Type)
   with
      Import,
      Convention => C,
      External_Name => "print_u64",
      SPARK_Mode => Off;

   procedure Print_Cstring (Str : String; Length : Uint64_Type)
   with
      Import,
      Convention => C,
      External_Name => "print_cstring",
      SPARK_Mode => Off;

   procedure Print_String (Str : String)
   with
      SPARK_Mode => Off;

   function To_String (Int : Uint64_Type)
   return String;

   function To_String (Bool : Boolean)
   return String;

   function To_String (Tag : Tag_Type)
   return String;

   function To_String (PBA : Physical_Block_Address_Type)
   return String;

   function To_String (Blk : Block_Data_Type)
   return String;

   function Image (Int : Uint64_Type)
   return String;

end CBE.Debug;
