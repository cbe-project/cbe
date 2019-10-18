--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Crypto
with SPARK_Mode
is
   --
   --  Item
   --
   package body Item
   with SPARK_Mode
   is
      --
      --  Mark_Completed_Primitive
      --
      procedure Mark_Completed_Primitive (
         Obj     : in out Item_Type;
         Success :        Boolean)
      is
      begin

         if Obj.State /= In_Progress then
            raise Program_Error;
         end if;

         Obj.State := Item.Complete;
         Primitive.Success (Obj.Prim, Success);

      end Mark_Completed_Primitive;

      --
      --  Invalid_Object
      --
      function Invalid_Object
      return Item_Type
      is (
         State => Invalid,
         Prim  => Primitive.Invalid_Object);

      --
      --  Submitted_Encryption_Object
      --
      function Submitted_Encryption_Object (Prm : Primitive.Object_Type)
      return Item_Type
      is (
         State => Pending,
         Prim  => Prm);

      --
      --  Submitted_Decryption_Object
      --
      function Submitted_Decryption_Object (Prm : Primitive.Object_Type)
      return Item_Type
      is (
         State => Pending,
         Prim  => Prm);

      ----------------------
      --  Read Accessors  --
      ----------------------

      function Invalid (Obj : Item_Type) return Boolean
      is (Obj.State = Invalid);

      function Pending (Obj : Item_Type) return Boolean
      is (Obj.State = Pending);

      function In_Progress (Obj : Item_Type) return Boolean
      is (Obj.State = In_Progress);

      function Complete (Obj : Item_Type) return Boolean
      is (Obj.State = Complete);

      function Prim (Obj : Item_Type) return Primitive.Object_Type
      is (Obj.Prim);

      -----------------------
      --  Write Accessors  --
      -----------------------

      procedure State (Obj : in out Item_Type; Sta : State_Type)
      is begin Obj.State := Sta; end State;

   end Item;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is (
      Items            => (others => Item.Invalid_Object),
      Execute_Progress => False);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (for some Itm of Obj.Items => Item.Invalid (Itm));

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Data_Idx :    out Item_Index_Type)
   is
   begin
      For_Items : for Item_Idx in Obj.Items'Range loop
         if Item.Invalid (Obj.Items (Item_Idx)) then
            Obj.Items (Item_Idx) := Item.Submitted_Encryption_Object (Prim);
            Data_Idx := Item_Idx;
            return;
         end if;
      end loop For_Items;
      raise Program_Error;
   end Submit_Primitive;

   --
   --  Peek_Generated_Primitive
   --
   procedure Peek_Generated_Primitive (
      Obj      :     Object_Type;
      Item_Idx : out Item_Index_Type;
      Prim     : out Primitive.Object_Type)
   is
   begin
      Prim     := Primitive.Invalid_Object;
      Item_Idx := Item_Index_Type'First;
      Items_Loop :
      for Curr_Item_Idx in Obj.Items'Range loop
         if Item.Pending (Obj.Items (Curr_Item_Idx)) then
            Item_Idx := Curr_Item_Idx;
            Prim     := Item.Prim (Obj.Items (Curr_Item_Idx));
            return;
         end if;
      end loop Items_Loop;
   end Peek_Generated_Primitive;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj        : in out Object_Type;
      Item_Index :        Item_Index_Type)
   is
   begin
      if not Item.Pending (Obj.Items (Item_Index)) then
         raise Program_Error;
      end if;
      Item.State (Obj.Items (Item_Index), Item.In_Progress);
   end Drop_Generated_Primitive;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      for Item_Id in Obj.Items'Range loop
         if Item.Complete (Obj.Items (Item_Id)) then
            return Item.Prim (Obj.Items (Item_Id));
         end if;
      end loop;
      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type)
   is
   begin
      for Item_Id in Obj.Items'Range loop
         if Item.Complete (Obj.Items (Item_Id)) then
            Obj.Items (Item_Id) := Item.Invalid_Object;
            return;
         end if;
      end loop;
   end Drop_Completed_Primitive;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj        : in out Object_Type;
      Item_Index :        Item_Index_Type;
      Success    :        Boolean)
   is
   begin
      Item.Mark_Completed_Primitive (Obj.Items (Item_Index), Success);
   end Mark_Completed_Primitive;

   function Data_Index (
      Obj  : Crypto.Object_Type;
      Prim : Primitive.Object_Type)
   return Item_Index_Type
   is
   begin
      For_Items :
      for Item_Idx in Obj.Items'Range loop
         if Primitive.Equal (Item.Prim (Obj.Items (Item_Idx)), Prim) then
            return Item_Idx;
         end if;
      end loop For_Items;
      raise Program_Error;
   end Data_Index;

   function Data_Index_By_Request (
      Obj  : Crypto.Object_Type;
      Req  : Request.Object_Type)
   return Item_Index_Type
   is
      function Request_Equals_Primitive (
         Req  : Request.Object_Type;
         Prim : Primitive.Object_Type)
      return Boolean
      is
         (Request.Block_Number (Req) = Primitive.Block_Number (Prim) and then
          Request.Operation (Req) = Primitive.Operation (Prim) and then
          Request.Tag (Req) = Primitive.Tag (Prim));

      Result       : Item_Index_Type := Item_Index_Type'First;
      Result_Valid : Boolean         := False;
   begin
      For_Items :
      for Item_Idx in Obj.Items'Range loop
         if Request_Equals_Primitive (
               Req, Item.Prim (Obj.Items (Item_Idx)))
         then
            --
            --  We have to check whether the result is unambiguous as from
            --  a request we cannot determine the index of the primitive.
            --
            if Result_Valid then
               raise Program_Error;
            else
               Result := Item_Idx;
               Result_Valid := True;
            end if;
         end if;
      end loop For_Items;
      if not Result_Valid then
         raise Program_Error;
      end if;
      return Result;
   end Data_Index_By_Request;

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean
   is (Obj.Execute_Progress);

end CBE.Crypto;
