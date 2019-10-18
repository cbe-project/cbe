--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Cache
with SPARK_Mode
is
   pragma Pure;

   type Cache_Index_Type is range 0 .. 31;
   type Cache_Job_Index_Type is range 0 .. 1;

   type Cache_Data_Type is array (Cache_Index_Type'Range) of Block_Data_Type;
   type Cache_Job_Data_Type
   is array (Cache_Job_Index_Type'Range) of Block_Data_Type;

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
   --  Data_Available
   --
   function Data_Available (
      Obj : Object_Type;
      PBA : Physical_Block_Address_Type)
   return Boolean;

   --
   --  Data_Index
   --
   procedure Data_Index (
      Obj    : in out Object_Type;
      PBA    :        Physical_Block_Address_Type;
      Ts     :        Timestamp_Type;
      Result :    out Cache_Index_Type);

   --
   --  Invalidate
   --
   procedure Invalidate (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type);

   --
   --  Dirty
   --
   function Dirty (
      Obj : Object_Type;
      Idx : Cache_Index_Type)
   return Boolean;

   --
   --  Flush
   --
   function Flush (
      Obj : Object_Type;
      Idx : Cache_Index_Type)
   return Physical_Block_Address_Type;

   --
   --  Mark_Dirty
   --
   procedure Mark_Dirty (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type);

   --
   --  Mark_Clean
   --
   procedure Mark_Clean (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type);

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (
      Obj : Object_Type;
      PBA : Physical_Block_Address_Type)
   return Boolean;

   --
   --  Request_Acceptable_Logged
   --
   function Request_Acceptable_Logged (
      Obj : Object_Type;
      PBA : Physical_Block_Address_Type)
   return Boolean;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj : in out Object_Type;
      PBA : Physical_Block_Address_Type);

   --
   --  Submit_Request_Logged
   --
   procedure Submit_Request_Logged (
      Obj : in out Object_Type;
      PBA : Physical_Block_Address_Type);

   --
   --  Fill_Cache
   --
   procedure Fill_Cache (
      Obj      : in out Object_Type;
      Data     : in out Cache_Data_Type;
      Job_Data : in     Cache_Job_Data_Type;
      Time     :        Timestamp_Type);

   --
   --  Execute_Progress
   --
   function Execute_Progress (Obj : Object_Type)
   return Boolean;

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
   return Cache_Index_Type
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
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

private

   function Get_Cache_Slot (Obj  : Object_Type) return Cache_Index_Type;

   --
   --  Cache_Item
   --
   package Cache_Item
   with SPARK_Mode
   is
      type State_Type is (Unused, Used);
      type Cache_Item_Type  is private;

      --
      --  Unused_Object
      --
      function Unused_Object
      return Cache_Item_Type;

      --
      --  Initialize_Object
      --
      procedure Initialize_Object (
         Obj : out Cache_Item_Type;
         PBA :     Physical_Block_Address_Type;
         Ts  :     Timestamp_Type);

      -----------------
      --  Accessors  --
      -----------------

      function Unused (Obj : Cache_Item_Type) return Boolean;
      function Used   (Obj : Cache_Item_Type) return Boolean;
      function Dirty  (Obj : Cache_Item_Type) return Boolean;

      function PBA (Obj : Cache_Item_Type) return Physical_Block_Address_Type;
      function Ts  (Obj : Cache_Item_Type) return Timestamp_Type;

      procedure State (
         Obj : in out Cache_Item_Type;
         Sta :        State_Type);

      procedure Set_Ts (
         Obj : in out Cache_Item_Type;
         Ts  :        Timestamp_Type);

      procedure Invalidate (Obj : in out Cache_Item_Type);

      procedure Mark_Dirty (Obj : in out Cache_Item_Type);
      procedure Mark_Clean (Obj : in out Cache_Item_Type);

   private

      --
      --  Cache_Item_Type
      --
      type Cache_Item_Type is record
         PBA     : Physical_Block_Address_Type;
         Ts      : Timestamp_Type;
         State   : State_Type;
         Dirty   : Boolean;
      end record;

   end Cache_Item;

   type Cache_Items_Type
   is array (Cache_Index_Type'Range) of Cache_Item.Cache_Item_Type;

   --
   --  Job_Item
   --
   package Job_Item
   with SPARK_Mode
   is
      type State_Type is (Unused, Pending, In_Progress, Complete);
      type Job_Item_Type  is private;

      --
      --  Pending_Object
      --
      function Pending_Object (PBA : Physical_Block_Address_Type)
      return Job_Item_Type;

      --
      --  Unused_Object
      --
      function Unused_Object
      return Job_Item_Type;

      --
      --  Already_Pending
      --
      function Already_Pending (
         Obj : Job_Item_Type;
         PBA : Physical_Block_Address_Type)
      return Boolean;

      -----------------
      --  Accessors  --
      -----------------

      function Unused      (Obj : Job_Item_Type) return Boolean;
      function Pending     (Obj : Job_Item_Type) return Boolean;
      function In_Progress (Obj : Job_Item_Type) return Boolean;
      function Complete    (Obj : Job_Item_Type) return Boolean;

      function PBA (Obj : Job_Item_Type) return Physical_Block_Address_Type;
      function Success (Obj : Job_Item_Type) return Boolean;

      procedure State (
         Obj : in out Job_Item_Type;
         Sta :        State_Type);

      procedure Set_Unused (Obj : in out Job_Item_Type);
      procedure Set_Success (Obj : in out Job_Item_Type; Suc : Boolean);

   private

      --
      --  Job_Item_Type
      --
      type Job_Item_Type is record
         PBA     : Physical_Block_Address_Type;
         State   : State_Type;
         Success : Boolean;
      end record;

   end Job_Item;

   type Job_Items_Type
   is array (Cache_Job_Index_Type'Range) of Job_Item.Job_Item_Type;

   --
   --  Object_Type
   --
   type Object_Type is record
      Cache_Items      : Cache_Items_Type;
      Job_Items        : Job_Items_Type;
      Active_Jobs      : Natural;
      Execute_Progress : Boolean;
   end record;

end CBE.Cache;
