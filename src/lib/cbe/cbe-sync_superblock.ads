--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Sync_Superblock
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   --
   --  Initialize_Object
   --
   --  FIXME will not be used anymore when the library module is in spark
   --
   procedure Initialize_Object (Obj : out Object_Type);

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj : in out Object_Type;
      Idx :        Superblocks_Index_Type;
      Gen :        Generation_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Completed_Generation
   --
   function Peek_Completed_Generation (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Generation_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   with
      Pre => Primitive.Valid (Prim);

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Index
   --
   function Peek_Generated_Index (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Superblocks_Index_Type
   with
      Pre => Primitive.Valid (Prim);

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   with
      Pre => Primitive.Valid (Prim);

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

private

   --
   --  Item
   --
   package Item
   with SPARK_Mode
   is
      type State_Type is (Invalid, Pending, In_Progress, Complete);
      type Item_Type  is private;

      --
      --  Invalid_Item
      --
      function Invalid_Item
      return Item_Type;

      --
      --  Pending_Item
      --
      procedure Pending_Item (
         Obj : out Item_Type;
         Idx :     Superblocks_Index_Type;
         Gen :     Generation_Type);

      -----------------
      --  Accessors  --
      -----------------

      function Invalid     (Obj : Item_Type) return Boolean;
      function Pending     (Obj : Item_Type) return Boolean;
      function In_Progress (Obj : Item_Type) return Boolean;
      function Complete    (Obj : Item_Type) return Boolean;

      function State      (Obj : Item_Type) return State_Type;
      function Index      (Obj : Item_Type) return Superblocks_Index_Type;
      function Generation (Obj : Item_Type) return Generation_Type;

      procedure Set_State (
         Obj : in out Item_Type;
         Sta :        State_Type);

   private

      --
      --  Item_Type
      --
      type Item_Type is record
         Idx : Superblocks_Index_Type;
         Gen : Generation_Type;
         Sta : State_Type;
      end record;

   end Item;

   --
   --  Object_Type
   --
   type Object_Type is record
      Current_Primitive : Primitive.Object_Type;
      Current_Item      : Item.Item_Type;
   end record;

end CBE.Sync_Superblock;
