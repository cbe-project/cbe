--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;

package body CBE.Cache_Flusher
with SPARK_Mode
is

   package body Item
   with SPARK_Mode
   is
      --
      --  Invalid_Item
      --
      function Invalid_Item
      return Item_Type
      is (
         Sta => Invalid,
         PBA => 0,
         Idx => Cache.Cache_Index_Type'Last,
         Suc => False);

      --
      --  Pending_Item
      --
      procedure Pending_Item (
         Obj : out Item_Type;
         PBA :     Physical_Block_Address_Type;
         Idx :     Cache.Cache_Index_Type)
      is
      begin
         State (Obj, Sta => Pending);
         Obj.PBA := PBA;
         Obj.Idx := Idx;
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

      function Sta (Obj : Item_Type) return State_Type is (Obj.Sta);

      function PBA (Obj : Item_Type) return Physical_Block_Address_Type
      is (Obj.PBA);

      function Index (Obj : Item_Type) return Cache.Cache_Index_Type
      is (Obj.Idx);

      function Success (Obj : Item_Type) return Boolean is (Obj.Suc);

      procedure State (
         Obj : in out Item_Type;
         Sta :        State_Type)
      is
      begin
         Obj.Sta := Sta;
      end State;

      procedure Set_Success (
         Obj : in out Item_Type;
         Suc :        Boolean)
      is
      begin
         Obj.Suc := Suc;
      end Set_Success;

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
   is (Items => (others => Item.Invalid_Item));

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean
   is
   begin
      for Item_Id in Obj.Items'Range loop

         --
         --  XXX change interface so that we cannot submit requests multiple
         --      times. checking for any non invalid entry leads to only 1
         --      request pending at all times.
         --
         if not Item.Invalid (Obj.Items (Item_Id)) then
            return False;
         end if;
      end loop;

      return True;
   end Request_Acceptable;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type;
      Idx :        Cache.Cache_Index_Type)
   is
   begin
      for Item_Id in Obj.Items'Range loop
         if Item.Invalid (Obj.Items (Item_Id))
         then
            Item.Pending_Item (Obj.Items (Item_Id), PBA, Idx);
            return;
         end if;
      end loop;
   end Submit_Request;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      Peek_Primitive : for Item_Id in Obj.Items'Range loop
         if Item.Complete (Obj.Items (Item_Id))
         then
            return Primitive.Valid_Object (
               Op     => Write,
               Succ   => Request.Success_Type (
                            Item.Success (Obj.Items (Item_Id))),
               Tg     => 16#21#,
               Blk_Nr => Block_Number_Type (Item.PBA (Obj.Items (Item_Id))),
               Idx    => 0);
         end if;
      end loop Peek_Primitive;

      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Drop_Primitive : for Item_Id in Obj.Items'Range loop
         if Item.Complete (Obj.Items (Item_Id)) and then
            Item.PBA (Obj.Items (Item_Id)) =
               Physical_Block_Address_Type (Primitive.Block_Number (Prim))
         then
            Obj.Items (Item_Id) := Item.Invalid_Item;
            return;
         end if;
      end loop Drop_Primitive;
   end Drop_Completed_Primitive;

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      Peek_Primitive : for Item_Id in Obj.Items'Range loop
         if Item.Pending (Obj.Items (Item_Id))
         then
            return Primitive.Valid_Object (
               Op     => Write,
               Succ   => Request.Success_Type (False),
               Tg     => 16#21#,
               Blk_Nr => Block_Number_Type (Item.PBA (Obj.Items (Item_Id))),
               Idx => 0);
         end if;
      end loop Peek_Primitive;

      return Primitive.Invalid_Object;
   end Peek_Generated_Primitive;

   --
   --  Peek_Generated_Data_Index
   --
   function Peek_Generated_Data_Index (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Cache.Cache_Index_Type
   is
   begin
      Pending_Data : for Item_Id in Obj.Items'Range loop
         if Item.Pending (Obj.Items (Item_Id)) and then
            Item.PBA (Obj.Items (Item_Id)) =
               Physical_Block_Address_Type (Primitive.Block_Number (Prim))
         then
            return Item.Index (Obj.Items (Item_Id));
         end if;
      end loop Pending_Data;

      return Cache.Cache_Index_Type'Last;
   end Peek_Generated_Data_Index;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Drop_Primitive : for Item_Id in Obj.Items'Range loop
         if Item.Pending (Obj.Items (Item_Id)) and then
            Item.PBA (Obj.Items (Item_Id)) =
               Physical_Block_Address_Type (Primitive.Block_Number (Prim))
         then
            Item.State (Obj.Items (Item_Id), Item.In_Progress);
            return;
         end if;
      end loop Drop_Primitive;
   end Drop_Generated_Primitive;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Complete_Primitive : for Item_Id in Obj.Items'Range loop
         if Item.In_Progress (Obj.Items (Item_Id)) and then
            Item.PBA (Obj.Items (Item_Id)) =
               Physical_Block_Address_Type (Primitive.Block_Number (Prim))
         then
            Item.State (Obj.Items (Item_Id), Item.Complete);
            Item.Set_Success (
               Obj.Items (Item_Id), Boolean (Primitive.Success (Prim)));
            return;
         end if;
      end loop Complete_Primitive;
   end Mark_Generated_Primitive_Complete;
end CBE.Cache_Flusher;
