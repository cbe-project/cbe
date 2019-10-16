--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Block_IO
with SPARK_Mode
is
   function Invalid_Entry return Entry_Type
   is (Orig_Tag => Tag_Invalid,
       Prim     => Primitive.Invalid_Object,
       State    => Unused);

   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin
      Obj := Initialized_Object;
   end Initialize_Object;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is (
      Entries => (others => Invalid_Entry),
      Used_Entries => 0);

   function Primitive_Acceptable (Obj : Object_Type) return Boolean
   is (Obj.Used_Entries < Num_Entries_Type'Last);

   procedure Submit_Primitive (
      Obj        : in out Object_Type;
      Tag        :        Tag_Type;
      Prim       :        Primitive.Object_Type;
      Data_Index :    out Data_Index_Type)
   is
   begin
      for Idx in Obj.Entries'Range loop
         if Obj.Entries (Idx).State = Unused then
            Obj.Entries (Idx) := (
               Orig_Tag => Primitive.Tag (Prim),
               Prim     => Primitive.Valid_Object (
               Op       => Primitive.Operation (Prim),
               Succ     => Primitive.Success (Prim),
               Tg       => Tag,
               Blk_Nr   => Primitive.Block_Number (Prim),
               Idx      => Primitive.Index (Prim)),
               State    => Pending);

            Data_Index       := Idx;
            Obj.Used_Entries := Obj.Used_Entries + 1;
            return;
         end if;
      end loop;
      raise Program_Error;
   end Submit_Primitive;

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      for I in Obj.Entries'Range loop
         if Obj.Entries (I).State = Complete then
            return Obj.Entries (I).Prim;
         end if;
      end loop;

      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   function Peek_Completed_Data_Index (Obj : Object_Type)
   return Data_Index_Type
   is
   begin
      for I in Obj.Entries'Range loop
         if Obj.Entries (I).State = Complete then
            return I;
         end if;
      end loop;

      --  XXX precondition
      raise Program_Error;
   end Peek_Completed_Data_Index;

   function Peek_Completed_Tag (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return CBE.Tag_Type
   is
   begin
      for I in Obj.Entries'Range loop
         if
            Obj.Entries (I).State = Complete and then
            Primitive.Equal (Prim, Obj.Entries (I).Prim)
         then
            return Obj.Entries (I).Orig_Tag;
         end if;
      end loop;

      --  XXX precondition
      raise Program_Error;
   end Peek_Completed_Tag;

   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      for I in Obj.Entries'Range loop
         if
            Obj.Entries (I).State = Complete and then
            Primitive.Equal (Prim, Obj.Entries (I).Prim)
         then
            Obj.Entries (I) := Invalid_Entry;
            Obj.Used_Entries := Obj.Used_Entries - 1;
            return;
         end if;
      end loop;
   end Drop_Completed_Primitive;

   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      for I in Obj.Entries'Range loop
         if Obj.Entries (I).State = Pending then
            return Obj.Entries (I).Prim;
         end if;
      end loop;

      return Primitive.Invalid_Object;
   end Peek_Generated_Primitive;

   function Peek_Generated_Data_Index (
      Obj  : Object_Type;
      Prim : CBE.Primitive.Object_Type)
   return Data_Index_Type
   is
   begin
      for I in Obj.Entries'Range loop
         --
         --  XXX why is the condition different from
         --     'Peek_Generated_Primitive' and 'Drop_Completed_Primitive'?
         --
         if
            Obj.Entries (I).State = Pending or else
            Obj.Entries (I).State = In_Progress
         then
            if Primitive.Equal (Prim, Obj.Entries (I).Prim) then
               return I;
            end if;
         end if;
      end loop;

      --  XXX precondition
      raise Program_Error;
   end Peek_Generated_Data_Index;

   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      for I in Obj.Entries'Range loop
         if
            Obj.Entries (I).State = Pending and then
            Primitive.Equal (Prim, Obj.Entries (I).Prim)
         then
            Obj.Entries (I).State := In_Progress;
            return;
         end if;
      end loop;
   end Drop_Generated_Primitive;

   procedure Drop_Generated_Primitive_2 (
      Obj      : in out Object_Type;
      Data_Idx :        Data_Index_Type)
   is
   begin
      if Obj.Entries (Data_Idx).State /= Pending then
         raise Program_Error;
      end if;
      Obj.Entries (Data_Idx).State := In_Progress;
   end Drop_Generated_Primitive_2;

   procedure Mark_Generated_Primitive_Complete (
      Obj      : in out Object_Type;
      Data_Idx :        Data_Index_Type;
      Success  :        Boolean)
   is
   begin
      if Obj.Entries (Data_Idx).State /= In_Progress then
         raise Program_Error;
      end if;
      Primitive.Success (Obj.Entries (Data_Idx).Prim, Success);
      Obj.Entries (Data_Idx).State := Complete;
   end Mark_Generated_Primitive_Complete;

end CBE.Block_IO;
