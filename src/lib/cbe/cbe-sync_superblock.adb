--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;

package body CBE.Sync_Superblock
with SPARK_Mode
is
   package body Item
   with SPARK_Mode
   is
      --
      --  Pending_Item
      --
      function Invalid_Item
      return Item_Type
      is (
         Sta => Invalid,
         Idx => 0,
         Gen => 0);

      --
      --  Pending_Item
      --
      procedure Pending_Item (
         Obj : out Item_Type;
         Idx :     Superblocks_Index_Type;
         Gen :     Generation_Type)
      is
      begin
         Set_State (Obj, Sta => Pending);

         Obj.Idx := Idx;
         Obj.Gen := Gen;
      end Pending_Item;

      -----------------
      --  Accessors  --
      -----------------

      function Invalid     (Obj : Item_Type) return Boolean
      is (Obj.Sta = Invalid);

      function Pending     (Obj : Item_Type) return Boolean
      is (Obj.Sta = Pending);

      function In_Progress (Obj : Item_Type) return Boolean
      is (Obj.Sta = In_Progress);

      function Complete    (Obj : Item_Type) return Boolean
      is (Obj.Sta = Complete);

      function State      (Obj : Item_Type) return State_Type
      is (Obj.Sta);

      function Index      (Obj : Item_Type) return Superblocks_Index_Type
      is (Obj.Idx);

      function Generation (Obj : Item_Type) return Generation_Type
      is (Obj.Gen);

      procedure Set_State (
         Obj : in out Item_Type;
         Sta :        State_Type)
      is
      begin
         Obj.Sta := Sta;
      end Set_State;
   end Item;

   --
   --  Initialize_Object
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
      Current_Item      => Item.Invalid_Item,
      Current_Primitive => Primitive.Invalid_Object);

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean
   is
   begin
      return not Primitive.Valid (Obj.Current_Primitive);
   end Request_Acceptable;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj : in out Object_Type;
      Idx :        Superblocks_Index_Type;
      Gen :        Generation_Type)
   is
   begin
      Item.Pending_Item (Obj.Current_Item, Idx, Gen);
      Obj.Current_Primitive := Primitive.Valid_Object (
         Op     => Write,
         Succ   => Request.Success_Type (False),
         Tg     => 16#80#,
         --  there is currently a 1:1 mapping between SB slot and pba
         Blk_Nr => Block_Number_Type (Item.Index (Obj.Current_Item)),
         Idx    => 0);
   end Submit_Request;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Item.Complete (Obj.Current_Item)
      then
         return Obj.Current_Primitive;
      end if;
      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   --
   --  Peek_Completed_Generation
   --
   function Peek_Completed_Generation (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Generation_Type
   is
   begin
      if Item.Complete (Obj.Current_Item) and then
         Primitive.Block_Number (Obj.Current_Primitive) =
            Primitive.Block_Number (Prim)
      then
         return Item.Generation (Obj.Current_Item);
      end if;
      return Generation_Type (0);
   end Peek_Completed_Generation;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Item.Complete (Obj.Current_Item) and then
         Primitive.Block_Number (Obj.Current_Primitive) =
            Primitive.Block_Number (Prim)
      then
         Obj.Current_Item := Item.Invalid_Item;
         Obj.Current_Primitive := Primitive.Invalid_Object;
         return;
      end if;
   end Drop_Completed_Primitive;

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Item.Pending (Obj.Current_Item)
      then
         return Obj.Current_Primitive;
      end if;

      return Primitive.Invalid_Object;
   end Peek_Generated_Primitive;

   --
   --  Peek_Generated_Index
   --
   function Peek_Generated_Index (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Superblocks_Index_Type
   is
   begin
      if Item.Pending (Obj.Current_Item) and then
         Primitive.Block_Number (Obj.Current_Primitive) =
            Primitive.Block_Number (Prim)
      then
         return Item.Index (Obj.Current_Item);
      end if;
      raise Program_Error;
   end Peek_Generated_Index;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Item.Pending (Obj.Current_Item) and then
         Primitive.Block_Number (Obj.Current_Primitive) =
            Primitive.Block_Number (Prim)
      then
         Item.Set_State (Obj.Current_Item, Item.In_Progress);
         return;
      end if;
   end Drop_Generated_Primitive;

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Item.In_Progress (Obj.Current_Item) and then
         Primitive.Block_Number (Obj.Current_Primitive) =
            Primitive.Block_Number (Prim)
      then
         Item.Set_State (Obj.Current_Item, Item.Complete);
         Primitive.Success (Obj.Current_Primitive, Primitive.Success (Prim));
         return;
      end if;
   end Mark_Generated_Primitive_Complete;
end CBE.Sync_Superblock;
