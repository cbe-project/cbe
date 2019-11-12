--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;
with SHA256_4K;

package body CBE.Write_Back
with SPARK_Mode
is
   --
   --  Private
   --

   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type);

   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type);

   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type)
   is
      SHA_Idx : SHA256_4K.Hash_Index_Type := SHA256_4K.Hash_Index_Type'First;
   begin
      for CBE_Idx in CBE_Hash'Range loop
         CBE_Hash (CBE_Idx) := Byte_Type (SHA_Hash (SHA_Idx));
         if CBE_Idx < CBE_Hash'Last then
            SHA_Idx := SHA_Idx + 1;
         end if;
      end loop;
   end CBE_Hash_From_SHA256_4K_Hash;

   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type)
   is
      CBE_Idx : Block_Data_Index_Type := Block_Data_Index_Type'First;
   begin
      for SHA_Idx in SHA_Data'Range loop
         SHA_Data (SHA_Idx) := SHA256_4K.Byte (CBE_Data (CBE_Idx));
         if SHA_Idx < SHA_Data'Last then
            CBE_Idx := CBE_Idx + 1;
         end if;
      end loop;
   end SHA256_4K_Data_From_CBE_Data;

   procedure Fail_If_Pending_Primitive_Not_Complete (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   is
   begin
      if
         Obj.State /= Complete or else
         Primitive.Block_Number (Prim) /=
            Primitive.Block_Number (Obj.Pending_Primitive)
      then
         raise Program_Error;
      end if;
   end Fail_If_Pending_Primitive_Not_Complete;

   --
   --  The method is called for all entries by the specific
   --  'Peek_Generated_*_Primitve' method, which contains the
   --  state guard.
   --
   function Peek_Generated_Leaf_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.Entries (0).State /= Pending then
         return Primitive.Invalid_Object;
      end if;
      return Primitive.Valid_Object (
         Op     => Write,
         Succ   => False,
         Tg     => Obj.Entries (0).Tag,
         Pl_Idx =>
            Pool_Idx_Slot_Content (
               Primitive.Pool_Idx_Slot (Obj.Pending_Primitive)),
         Blk_Nr => Block_Number_Type (Obj.Entries (0).Update_PBA),
         Idx    => 0);
   end Peek_Generated_Leaf_Primitive;

   --
   --  The method is called for all entries by the specific
   --  'Peek_Generated_*_Data' method, which contains the
   --  state guard.
   --
   function Peek_Generated_Leaf_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Data_Index_Type
   is
   begin
      if
         Primitive.Block_Number (Prim) /=
            Block_Number_Type (Obj.Entries (0).Update_PBA)
      then
         raise Program_Error;
      end if;
      return Data_Index_Type (0);
   end Peek_Generated_Leaf_Data;

   procedure Mark_Entry_As_In_Progress (
      Prim :        Primitive.Object_Type;
      E    : in out Entry_Type)
   is
   begin
      --  XXX condition differs from 'Peek_Generated_Leaf_Data'
      if
         E.State /= Pending or else
         Primitive.Block_Number (Prim) /=
            Block_Number_Type (E.Update_PBA)
      then
         raise Program_Error;
      end if;

      E.State := In_Progress;
   end Mark_Entry_As_In_Progress;

   --
   --  The method is called for all entries by the specific
   --  'drop_generated_*_primitive' method, which contains the
   --  state guard.
   --
   procedure Drop_Generated_Leaf_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Mark_Entry_As_In_Progress (Prim, Obj.Entries (0));
   end Drop_Generated_Leaf_Primitive;

   function Invalid_Entry return Entry_Type
   is (
      PBA        => 0,
      Update_PBA => 0,
      State      => Invalid,
      Tag        => Primitive.Tag_Invalid);

   --
   --  Public
   --

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
      Entries           => (others => Invalid_Entry),
      Hashes            => (others => (others => 0)),
      Levels            => 0,
      State             => Invalid,
      Pending_Primitive => Primitive.Invalid_Object,
      Pending_Failure   => False,
      VBA               => Virtual_Block_Address_Type'Last,
      New_Generation    => 0);

   procedure Update (
      Obj         : in out Object_Type;
      PBA         :        Physical_Block_Address_Type;
      Tree        :        Tree_Helper.Object_Type;
      Data        :        Block_Data_Type;
      Update_Data : in out Block_Data_Type)
   is
   begin
      --
      --  Start at 1 since we only care about the inner nodes. The
      --  leaf node was already handled and the hash is stored, which
      --  makes accessing _entry[i-1] in the first round safe. Currently
      --  the way the arbiter operates and the Write_back module generates
      --  primitives should lead to updating the tree level by level.
      --
      --  (And if that's not the case, updating will corrupt the tree
      --  for obvious reasons.)
      --
      For_Each_Inner_Lvl :
      for Lvl in 1 .. Obj.Levels loop

         if Primitive."=" (Obj.Entries (Lvl).Tag, Primitive.Tag_Cache) and then
            Obj.Entries (Lvl).PBA = PBA
         then

            --  CoW action incoming
            if Obj.Entries (Lvl).PBA /= Obj.Entries (Lvl).Update_PBA then
               Update_Data := Data;
            end if;

            declare
               --  save as long as only inner nodes in cache
               Child_Update_PBA : constant Physical_Block_Address_Type :=
                  Obj.Entries (Lvl - 1).Update_PBA;

               Child_Hash : constant Hash_Type := Obj.Hashes (Lvl - 1);

               --  get index from VBA in inner node
               Index : constant Tree_Child_Index_Type :=
                  Tree_Helper.Index (Tree, Obj.VBA, Lvl);

               Nodes : Type_1_Node_Block_Type;
            begin
               Type_1_Node_Block_From_Block_Data (Nodes, Update_Data);
               Nodes (Natural (Index)).PBA  := Child_Update_PBA;
               Nodes (Natural (Index)).Gen  := Obj.New_Generation;
               Nodes (Natural (Index)).Hash := Child_Hash;
               Block_Data_From_Type_1_Node_Block (Update_Data, Nodes);
            end;

            --  calculate hash
            Declare_SHA_Args :
            declare
               SHA_Data : SHA256_4K.Data_Type;
               SHA_Hash : SHA256_4K.Hash_Type;
            begin
               SHA256_4K_Data_From_CBE_Data (SHA_Data, Update_Data);
               SHA256_4K.Hash (SHA_Data, SHA_Hash);
               CBE_Hash_From_SHA256_4K_Hash (Obj.Hashes (Lvl), SHA_Hash);
            end Declare_SHA_Args;

            Obj.Entries (Lvl).State := Complete;

            exit For_Each_Inner_Lvl;
         end if;
      end loop For_Each_Inner_Lvl;

      --  for now always check all entries after update
      declare
         All_Entries_Complete : Boolean := True;
      begin
         for I in 1 .. Obj.Levels loop
            if Obj.Entries (I).State /= Complete then
               All_Entries_Complete := False;
               exit;
            end if;
         end loop;

         if All_Entries_Complete then
            Obj.State := Complete;
         end if;
      end;
   end Update;

   function Primitive_Acceptable (Obj : Object_Type) return Boolean
   is (not Primitive.Valid (Obj.Pending_Primitive));

   procedure Submit_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Gen      :        Generation_Type;
      VBA      :        Virtual_Block_Address_Type;
      New_PBAs :        New_PBAs_Type;
      Old_PBAs :        Type_1_Node_Walk_Type;
      N        :        Tree_Level_Index_Type;
      Data     :        Block_Data_Type;
      WB_Data  : in out Data_Type)
   is
   begin

      --  set internal state
      Obj.Pending_Primitive := Prim;
      Obj.New_Generation    := Gen;
      Obj.VBA               := VBA;
      Obj.Levels            := N;

      --
      --  Currently only the leaf node is written to disk directly,
      --  the other entries are all inner nodes and are stored in
      --  the cache and will be written to disk on a cache flush.
      --

      --  always start by encrypting the new leaf data
      Obj.State := Crypto;

      --
      --  handle common members of an entry...
      --
      --  XXX iterate from 0?
      --
      for I in 0 .. Obj.Levels loop
         Obj.Entries (I).PBA        :=
            Old_PBAs (Tree_Level_Index_Type (I)).PBA;

         Obj.Entries (I).Update_PBA := New_PBAs (I);
         Obj.Entries (I).State      := Pending;
         Obj.Entries (I).Tag        := Primitive.Tag_Cache;
      end loop;

      --  ... but the data or rather leaf node is special
      Obj.Entries (0).Tag := Primitive.Tag_Encrypt;
      WB_Data (0) := Data;
   end Submit_Primitive;

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
      Prim : Primitive.Object_Type := Obj.Pending_Primitive;
   begin
      if not Primitive.Valid (Obj.Pending_Primitive) then
         return Primitive.Invalid_Object;
      end if;

      if Obj.State /= Complete then
         return Primitive.Invalid_Object;
      end if;

      Primitive.Success (Prim, Request.Success_Type (not Obj.Pending_Failure));
      return Prim;

   end Peek_Completed_Primitive;

   function Peek_Completed_Root (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Physical_Block_Address_Type
   is
   begin
      Fail_If_Pending_Primitive_Not_Complete (Obj, Prim);
      return Obj.Entries (Obj.Levels).Update_PBA;
   end Peek_Completed_Root;

   procedure Peek_Completed_Root_Hash (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type;
      Hash : out Hash_Type)
   is
   begin
      Fail_If_Pending_Primitive_Not_Complete (Obj, Prim);
      Hash := Obj.Hashes (Obj.Levels);
   end Peek_Completed_Root_Hash;

   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Fail_If_Pending_Primitive_Not_Complete (Obj, Prim);
      Obj.Pending_Primitive := Primitive.Invalid_Object;
   end Drop_Completed_Primitive;

   function Peek_Generated_Crypto_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.State = Crypto then
         return Peek_Generated_Leaf_Primitive (Obj);
      end if;
      return Primitive.Invalid_Object;
   end Peek_Generated_Crypto_Primitive;

   function Peek_Generated_Crypto_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Data_Index_Type
   is
   begin
      if Obj.State /= Crypto then
         raise Program_Error;
      end if;
      return Peek_Generated_Leaf_Data (Obj, Prim);
   end Peek_Generated_Crypto_Data;

   procedure Drop_Generated_Crypto_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.State /= Crypto then
         raise Program_Error;
      end if;
      Drop_Generated_Leaf_Primitive (Obj, Prim);
   end Drop_Generated_Crypto_Primitive;

   procedure Mark_Completed_Crypto_Primitive (
      Obj         : in out Object_Type;
      Prim        :        Primitive.Object_Type;
      Crypto_Data :        Block_Data_Type)
   is
   begin
      if Obj.State /= Crypto then
         raise Program_Error;
      end if;
      if
         Obj.Entries (0).State /= In_Progress or else
         Primitive.Block_Number (Prim) /=
            Block_Number_Type (Obj.Entries (0).Update_PBA)
      then
         raise Program_Error;
      end if;

      Declare_SHA_Args :
      declare
         SHA_Data : SHA256_4K.Data_Type;
         SHA_Hash : SHA256_4K.Hash_Type;
      begin
         SHA256_4K_Data_From_CBE_Data (SHA_Data, Crypto_Data);
         SHA256_4K.Hash (SHA_Data, SHA_Hash);
         CBE_Hash_From_SHA256_4K_Hash (Obj.Hashes (0), SHA_Hash);
      end Declare_SHA_Args;

      if not Primitive.Success (Prim) then
         Obj.Pending_Failure := True;
         Obj.State           := Complete;
         return;
      end if;

      Obj.Entries (0).State := Pending;
      Obj.Entries (0).Tag   := Primitive.Tag_IO;

      Obj.State := IO;

   end Mark_Completed_Crypto_Primitive;

   function Peek_Generated_IO_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.State /= IO then
         return Primitive.Invalid_Object;
      end if;
      return Peek_Generated_Leaf_Primitive (Obj);
   end Peek_Generated_IO_Primitive;

   function Peek_Generated_IO_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Data_Index_Type
   is
   begin
      if Obj.State /= IO then
         raise Program_Error;
      end if;
      return Peek_Generated_Leaf_Data (Obj, Prim);
   end Peek_Generated_IO_Data;

   procedure Drop_Generated_IO_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.State /= IO then
         raise Program_Error;
      end if;

      Drop_Generated_Leaf_Primitive (Obj, Prim);
   end Drop_Generated_IO_Primitive;

   procedure Mark_Completed_IO_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.State /= IO then
         raise Program_Error;
      end if;

      if
         Obj.Entries (0).State /= In_Progress or else
         Primitive.Block_Number (Prim) /=
            Block_Number_Type (Obj.Entries (0).Update_PBA)
      then
         raise Program_Error;
      end if;

      if not Primitive.Success (Prim) then
         Obj.Pending_Failure := True;
         Obj.State           := Complete;
         return;
      end if;

      Obj.Entries (0).State := Complete;

      Obj.State := Cache;
   end Mark_Completed_IO_Primitive;

   function Peek_Generated_Cache_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.State /= Cache then
         return Primitive.Invalid_Object;
      end if;

      For_Each_Entry : for I in 1 .. Obj.Levels loop
         if Obj.Entries (I).State = Pending then
            return Primitive.Valid_Object (
               Op     => Write,
               Succ   => Request.Success_Type (False),
               Tg     => Obj.Entries (I).Tag,
               Pl_Idx =>
                  Pool_Idx_Slot_Content (
                     Primitive.Pool_Idx_Slot (Obj.Pending_Primitive)),
               Blk_Nr => Block_Number_Type (Obj.Entries (I).PBA),
               Idx    => 0);
         end if;
      end loop For_Each_Entry;
      return Primitive.Invalid_Object;
   end Peek_Generated_Cache_Primitive;

   function Peek_Generated_Cache_Update_PBA (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Physical_Block_Address_Type
   is
      PBA : constant Physical_Block_Address_Type :=
         Physical_Block_Address_Type (Primitive.Block_Number (Prim));
   begin
      if Obj.State /= Cache then
         raise Program_Error;
      end if;

      For_Each_Entry : for I in 1 .. Obj.Levels loop
         if Obj.Entries (I).PBA = PBA then
            return Obj.Entries (I).Update_PBA;
         end if;
      end loop For_Each_Entry;

      raise Program_Error;
   end Peek_Generated_Cache_Update_PBA;

   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
      PBA : constant Physical_Block_Address_Type :=
         Physical_Block_Address_Type (Primitive.Block_Number (Prim));
   begin
      if Obj.State /= Cache then
         raise Program_Error;
      end if;

      For_Each_Entry : for I in 1 .. Obj.Levels loop
         if Obj.Entries (I).PBA = PBA then
            Obj.Entries (I).State := In_Progress;
            return;
         end if;
      end loop For_Each_Entry;

      raise Program_Error;
   end Drop_Generated_Cache_Primitive;

end CBE.Write_Back;
