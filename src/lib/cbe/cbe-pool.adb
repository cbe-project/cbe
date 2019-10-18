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
         Nr_Of_Prims      => 0,
         Nr_Of_Done_Prims => 0);

      --
      --  Pending_Object
      --
      function Pending_Object (
         Rq               : Request.Object_Type;
         Nr_Of_Prims      : Number_Of_Primitives_Type;
         Nr_Of_Done_Prims : Number_Of_Primitives_Type)
      return Item_Type
      is (
         State            => Pending,
         Req              => Rq,
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
   is (Items => (others => Item.Invalid_Object));

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean
   is
   begin
      for Itm of Obj.Items loop
         if Item.Invalid (Itm) then
            return True;
         end if;
      end loop;
      return False;
   end Request_Acceptable;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj         : in out Object_Type;
      Req         :        Request.Object_Type;
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
                  Nr_Of_Prims      => Nr_Of_Prims,
                  Nr_Of_Done_Prims => 0);

            exit Items_Loop;

         end if;

      end loop Items_Loop;

   end Submit_Request;

   --
   --  Peek_Pending_Request
   --
   function Peek_Pending_Request (Obj : Pool.Object_Type)
   return Request.Object_Type
   is
   begin
      for Item_Id in Obj.Items'Range loop
         if Item.Pending (Obj.Items (Item_Id)) then
            return Item.Req (Obj.Items (Item_Id));
         end if;
      end loop;
      return Request.Invalid_Object;
   end Peek_Pending_Request;

   --
   --  Drop_Pending_Request
   --
   procedure Drop_Pending_Request (Obj : in out Object_Type)
   is
   begin
      for Item_Id in Obj.Items'Range loop
         if Item.Pending (Obj.Items (Item_Id)) then
            Item.State (Obj.Items (Item_Id), Item.In_Progress);
            return;
         end if;
      end loop;
   end Drop_Pending_Request;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
      Req : Request.Object_Type;
   begin
      for Item_Id in Obj.Items'Range loop
         Req := Item.Req (Obj.Items (Item_Id));
         if Request.Tag (Req) = Primitive.Tag (Prim) then
            Item.Primitive_Completed (Obj.Items (Item_Id), Prim);
         end if;
      end loop;
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
   procedure Drop_Completed_Request (Obj : in out Object_Type)
   is
   begin
      for Item_Id in Obj.Items'Range loop
         if Item.Complete (Obj.Items (Item_Id)) then
            Obj.Items (Item_Id) := Item.Invalid_Object;
            return;
         end if;
      end loop;
   end Drop_Completed_Request;

   --
   --  Request_For_Tag
   --
   function Request_For_Tag (
      Obj : Object_Type;
      Tag : Tag_Type)
   return Request.Object_Type
   is
      Req : Request.Object_Type;
   begin
      for Itm of Obj.Items loop
         if not Item.Invalid (Itm) then
            Req := Item.Req (Itm);
            if Request.Tag (Req) = Tag then
               return Req;
            end if;
         end if;
      end loop;
      return Request.Invalid_Object;
   end Request_For_Tag;

end CBE.Pool;
