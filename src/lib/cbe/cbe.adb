--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE
with SPARK_Mode
is
   function Snapshot_Valid (Snap : Snapshot_Type)
   return Boolean
   is
   begin
      case Snap.Valid is
      when 0      => return False;
      when 1      => return True;
      when others => raise Program_Error;
      end case;
   end Snapshot_Valid;

   procedure Snapshot_Valid (
      Snap  : in out Snapshot_Type;
      Valid :        Boolean)
   is
   begin
      case Valid is
      when False => Snap.Valid := 0;
      when True  => Snap.Valid := 1;
      end case;
   end Snapshot_Valid;

   procedure Print_String (S : String)
   is
   begin
      Print_Cstring (S, S'Length);
   end Print_String;

   --  XXX based on ada-runtime/src/minimal/a-except.adb
   function Image (I : U64) return String
   is
      S : String (1 .. 24) := (others => '_');
      V : U64 := I;
   begin
      for J in reverse S'First + 1 .. S'Last loop
         S (J) := Character'Val (48 + (V rem 10));
         V := V / 10;
         if V = 0 then
            return S (J .. S'Last);
         end if;
      end loop;
      return S;
   end Image;

   function To_String (N : U64)
   return String
   is
   begin
      return Image (N);
   end To_String;

   function To_String (B : Boolean) return String
   is
   begin
      if B then
         return "True";
      else
         return "False";
      end if;
   end To_String;

   function To_String (T : Tag_Type) return String
   is
   begin
      return To_String (U64 (T));
   end To_String;

   function To_String (A : Physical_Block_Address_Type) return String
   is
   begin
      if A = Physical_Block_Address_Type'Last then
         return "Invalid";
      end if;
      return To_String (U64 (A));
   end To_String;

   function To_String (B : Block_Data_Type) return String
   is
   begin
      return "Block_Data (" &
             To_String (U64 (B (1))) & ", " &
             To_String (U64 (B (2))) & ", " &
             To_String (U64 (B (3))) & ", " &
             To_String (U64 (B (4))) & ", " &
             To_String (U64 (B (5))) & ", " &
             "...)";
   end To_String;

end CBE;
