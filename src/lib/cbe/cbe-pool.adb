--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Pool
with SPARK_Mode
is

   package body Item
   with SPARK_Mode
   is
      --
      --  Primitive_Completed
      --
      procedure Primitive_Completed (
         Obj  : in out Item_Type;
         Prim :        Primitive.Object_Type)
      is
         Rq : Request.Object_Type;
      begin
         if not Primitive.Success (Prim) then
            Rq := Item.Req (Obj);
            Request.Success (Rq, False);
            Item.Req (Obj, Rq);
         end if;
         Obj.Nr_Of_Done_Prims := Obj.Nr_Of_Done_Prims + 1;
         if Obj.Nr_Of_Done_Prims = Obj.Nr_Of_Prims then
            Obj.State := Complete;
         end if;
      end Primitive_Completed;

      --
      --  Invalid_Object
      --
      function Invalid_Object
      return Item_Type
      is (
         State            => Invalid,
         Req              => Request.Invalid_Object,
         Snap_ID          => 0,
         Nr_Of_Prims      => 0,
         Nr_Of_Done_Prims => 0);

      --
      --  Pending_Object
      --
      function Pending_Object (
         Rq               : Request.Object_Type;
         ID               : Snapshot_ID_Type;
         Nr_Of_Prims      : Number_Of_Primitives_Type;
         Nr_Of_Done_Prims : Number_Of_Primitives_Type)
      return Item_Type
      is (
         State            => Pending,
         Req              => Rq,
         Snap_ID          => ID,
         Nr_Of_Prims      => Nr_Of_Prims,
         Nr_Of_Done_Prims => Nr_Of_Done_Prims);

      -----------------
      --  Accessors  --
      -----------------

      function Invalid (Obj : Item_Type) return Boolean
      is (Obj.State = Invalid);

      function Pending (Obj : Item_Type) return Boolean
      is (Obj.State = Pending);

      function In_Progress (Obj : Item_Type) return Boolean
      is (Obj.State = In_Progress);

      function Complete   (Obj : Item_Type) return Boolean
      is (Obj.State = Complete);

      function Req (Obj : Item_Type) return Request.Object_Type
      is (Obj.Req);

      function Snap_ID (Obj : Item_Type) return Snapshot_ID_Type
      is (Obj.Snap_ID);

      procedure Req (
         Obj : in out Item_Type;
         Rq  :        Request.Object_Type)
      is
         Rq1 : constant Request.Object_Type := Rq;
      begin
         Obj.Req := Rq1;
      end Req;

      procedure State (
         Obj : in out Item_Type;
         Sta :        State_Type)
      is
      begin
         Obj.State := Sta;
      end State;

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
      Items => (others => Item.Invalid_Object),
      Indices => Index_Queue.Empty_Index_Queue);

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type) return Boolean is
   (not Index_Queue.Full (Obj.Indices) and then (
      for some I of Obj.Items => Item.Invalid (I)));

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj         : in out Object_Type;
      Req         :        Request.Object_Type;
      ID          :        Snapshot_ID_Type;
      Nr_Of_Prims :        Number_Of_Primitives_Type)
   is
      Req_Buf : Request.Object_Type := Req;
   begin

      Items_Loop :
      for Item_Id in Obj.Items'Range loop

         if Item.Invalid (Obj.Items (Item_Id)) then

            Request.Success (Req_Buf, True);
            Obj.Items (Item_Id) :=
               Item.Pending_Object (
                  Rq               => Req_Buf,
                  ID               => ID,
                  Nr_Of_Prims      => Nr_Of_Prims,
                  Nr_Of_Done_Prims => 0);

            Index_Queue.Enqueue (Obj.Indices, Item_Id);

            exit Items_Loop;

         end if;

      end loop Items_Loop;

   end Submit_Request;

   --
   --  Peek_Pending_Request
   --
   function Peek_Pending_Request (Obj : Object_Type)
   return Pool_Index_Slot_Type
   is
   begin
      if Index_Queue.Empty (Obj.Indices) then
         return Pool_Idx_Slot_Invalid;
      end if;
      return Pool_Idx_Slot_Valid (Index_Queue.Head (Obj.Indices));
   end Peek_Pending_Request;

   --
   --  Drop_Pending_Request
   --
   procedure Drop_Pending_Request (Obj : in out Object_Type)
   is
   begin
      if Index_Queue.Empty (Obj.Indices) then
         raise Program_Error;
      end if;

      Index_Queue.Dequeue_Head (Obj.Indices);
   end Drop_Pending_Request;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Item.Primitive_Completed (
         Obj.Items (Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim))),
         Prim);
   end Mark_Completed_Primitive;

   --
   --  Peek_Completed_Request
   --
   function Peek_Completed_Request (Obj : Pool.Object_Type)
   return Request.Object_Type
   is
   begin
      for Item_Id in Obj.Items'Range loop
         if Item.Complete (Obj.Items (Item_Id)) then
            return Item.Req (Obj.Items (Item_Id));
         end if;
      end loop;
      return Request.Invalid_Object;
   end Peek_Completed_Request;

   --
   --  Drop_Completed_Request
   --
   procedure Drop_Completed_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      For_Each_Item : for Idx in Obj.Items'Range loop
         if Request.Equal (Item.Req (Obj.Items (Idx)), Req) then
            if not Item.Complete (Obj.Items (Idx)) then
               raise Program_Error;
            end if;
            Obj.Items (Idx) := Item.Invalid_Object;
            return;
         end if;
      end loop For_Each_Item;
      raise Program_Error;
   end Drop_Completed_Request;

   --
   --  Snap_ID_For_Request
   --
   function Snap_ID_For_Request (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Snapshot_ID_Type
   is
   begin
      for Itm of Obj.Items loop
         if not Item.Invalid (Itm) then
            if Request.Equal (Item.Req (Itm), (Req)) then
               return Item.Snap_ID (Itm);
            end if;
         end if;
      end loop;
      raise Program_Error;
   end Snap_ID_For_Request;

   --
   --  Request_For_Index
   --
   function Request_For_Index (
      Obj : Object_Type;
      Idx : Pool_Index_Type)
   return Request.Object_Type
   is
   begin
      if not Request.Valid (Item.Req (Obj.Items (Idx))) then
         raise Program_Error;
      end if;
      return Item.Req (Obj.Items (Idx));
   end Request_For_Index;

   package body Index_Queue
   with SPARK_Mode
   is
      function Empty_Index_Queue
      return Index_Queue_Type
      is (
         Head   => Queue_Index_Type'First,
         Tail   => Queue_Index_Type'First,
         Used   => Used_Type'First,
         Indices => (others => Pool_Index_Type'First));

      procedure Enqueue (
         Obj : in out Index_Queue_Type;
         Idx :        Pool_Index_Type)
      is
      begin
         Obj.Indices (Obj.Tail) := Idx;

         if Obj.Tail < Queue_Index_Type'Last then
            Obj.Tail := Queue_Index_Type'Succ (Obj.Tail);
         else
            Obj.Tail := Queue_Index_Type'First;
         end if;
         Obj.Used := Used_Type'Succ (Obj.Used);
      end Enqueue;

      function Head (Obj : Index_Queue_Type)
      return Pool_Index_Type
      is (Obj.Indices (Obj.Head));

      procedure Dequeue_Head (Obj : in out Index_Queue_Type)
      is
      begin
         if Obj.Head < Queue_Index_Type'Last then
            Obj.Head := Queue_Index_Type'Succ (Obj.Head);
         else
            Obj.Head := Queue_Index_Type'First;
         end if;
         Obj.Used := Used_Type'Pred (Obj.Used);
      end Dequeue_Head;

      function Empty (Obj : Index_Queue_Type)
      return Boolean
      is (Obj.Used = Used_Type'First);

      function Full (Obj : Index_Queue_Type)
      return Boolean
      is (Obj.Used = Used_Type'Last);
   end Index_Queue;

end CBE.Pool;
