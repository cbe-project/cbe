--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.Cache;

--
--  The Cache_Flusher is used to flush dirty cache entries. It could
--  be merged with the Cache as the Cache_Flusher.Item_Type is more
--  or less the same as the Cache.Job_Item_Type and both serve the
--  same purpose which is doing I/O on behalf of the cache.
--

package CBE.Cache_Flusher
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
   --  Active
   --
   function Active (Obj : Object_Type)
   return Boolean;

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
      PBA :        Physical_Block_Address_Type;
      Idx :        Cache.Cache_Index_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

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
   --  Peek_Generated_Data_Index
   --
   function Peek_Generated_Data_Index (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Cache.Cache_Index_Type
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
         PBA :     Physical_Block_Address_Type;
         Idx :     Cache.Cache_Index_Type);

      -----------------
      --  Accessors  --
      -----------------

      function Invalid     (Obj : Item_Type) return Boolean;
      function Pending     (Obj : Item_Type) return Boolean;
      function In_Progress (Obj : Item_Type) return Boolean;
      function Complete    (Obj : Item_Type) return Boolean;
      function Success     (Obj : Item_Type) return Boolean;

      function Sta (Obj : Item_Type) return State_Type;

      function PBA   (Obj : Item_Type) return Physical_Block_Address_Type;
      function Index (Obj : Item_Type) return Cache.Cache_Index_Type;

      procedure State (
         Obj : in out Item_Type;
         Sta :        State_Type);

      procedure Set_Success (
         Obj : in out Item_Type;
         Suc :        Boolean);

   private

      --
      --  Item_Type
      --
      type Item_Type is record
         PBA : Physical_Block_Address_Type;
         Idx : Cache.Cache_Index_Type;
         Sta : State_Type;
         Suc : Boolean;
      end record;

   end Item;

   type Items_Type is array (Cache.Cache_Index_Type'Range) of Item.Item_Type;

   --
   --  Object_Type
   --
   type Object_Type is record
      Items : Items_Type;
   end record;

end CBE.Cache_Flusher;
