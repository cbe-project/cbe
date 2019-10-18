--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Debug
with SPARK_Mode
is
   procedure Print_String (Str : String)
   with SPARK_Mode => Off
   is
   begin
      Print_Cstring (Str, Str'Length);
   end Print_String;

   --  XXX based on ada-runtime/src/minimal/a-except.adb
   function Image (Int : Uint64_Type)
   return String
   is
      Str        : String (1 .. 24) := (others => '_');
      Int_Remain : Uint64_Type := Int;
   begin
      for Idx in reverse Str'First + 1 .. Str'Last loop
         Str (Idx) := Character'Val (48 + (Int_Remain rem 10));
         Int_Remain := Int_Remain / 10;
         if Int_Remain = 0 then
            return Str (Idx .. Str'Last);
         end if;
      end loop;
      return Str;
   end Image;

   function To_String (Int : Uint64_Type)
   return String
   is (
      Image (Int));

   function To_String (Bool : Boolean)
   return String
   is (
      if Bool then "True" else "False");

   function To_String (Tag : Tag_Type)
   return String
   is (
      To_String (Uint64_Type (Tag)));

   function To_String (PBA : Physical_Block_Address_Type)
   return String
   is (
      if PBA = Physical_Block_Address_Type'Last then
         "Invalid"
      else
         To_String (Uint64_Type (PBA)));

   function To_String (Blk : Block_Data_Type)
   return String
   is (
      "Block_Data (" &
      To_String (Uint64_Type (Blk (1))) & ", " &
      To_String (Uint64_Type (Blk (2))) & ", " &
      To_String (Uint64_Type (Blk (3))) & ", " &
      To_String (Uint64_Type (Blk (4))) & ", " &
      To_String (Uint64_Type (Blk (5))) & ", " &
      "...)");

end CBE.Debug;
