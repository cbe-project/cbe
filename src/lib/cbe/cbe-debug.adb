--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Debug
with SPARK_Mode => Off
is
   procedure Print_String (Str : String)
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

   function To_String (H : Hash_Type)
   return String
   is (
      "HASH (" &
      To_String (Uint64_Type (H (1))) & "," &
      To_String (Uint64_Type (H (2))) & "," &
      To_String (Uint64_Type (H (3))) & "," &
      To_String (Uint64_Type (H (4))) & "," &
      To_String (Uint64_Type (H (5))) & "," &
      To_String (Uint64_Type (H (6))) & "," &
      To_String (Uint64_Type (H (7))) & "," &
      To_String (Uint64_Type (H (8))) & "," &
      "...)");

   procedure Dump_Superblock (
      SB_Index : Superblocks_Index_Type;
      SB       : Superblock_Type)
   is
   begin
      Debug.Print_String (
         "Dump SB: " &
         Debug.To_String (Debug.Uint64_Type (SB_Index)) &
         " " & " SN: " &
         Debug.To_String (Debug.Uint64_Type (SB.Curr_Snap)));

      for I in Snapshots_Index_Type loop
         if Snapshot_Valid (SB.Snapshots (I))
         then
            Debug.Print_String ("SB: "
               & Debug.To_String (Debug.Uint64_Type (SB_Index))
               & " SN: "
               & Debug.To_String (Debug.Uint64_Type (I))
               & " PBA: "
               & Debug.To_String (Debug.Uint64_Type (SB.Snapshots (I).PBA))
               & " GEN: "
               & Debug.To_String (Debug.Uint64_Type (SB.Snapshots (I).Gen))
               & " ID: "
               & Debug.To_String (Debug.Uint64_Type (SB.Snapshots (I).ID))
               & " KEEP: "
               & Debug.To_String (Debug.Uint64_Type (SB.Snapshots (I).Flags))
               & " "
               & Debug.To_String (SB.Snapshots (I).Hash));
         end if;
      end loop;
   end Dump_Superblock;

end CBE.Debug;
