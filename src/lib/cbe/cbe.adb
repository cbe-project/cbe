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

end CBE;
