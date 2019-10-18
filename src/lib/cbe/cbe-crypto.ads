--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.Request;

package CBE.Crypto
with SPARK_Mode
is
   --  Disable for now because of libsparkcrypto
   --  pragma Pure;

   subtype Plain_Data_Type  is CBE.Block_Data_Type;
   subtype Cipher_Data_Type is CBE.Block_Data_Type;

   type Item_Index_Type is range 0 .. 0;

   type Plain_Buffer_Index_Type is new Item_Index_Type;

   type Cipher_Buffer_Index_Type is new Item_Index_Type;

   type Plain_Buffer_Type is array (Item_Index_Type) of Plain_Data_Type;

   type Cipher_Buffer_Type is array (Item_Index_Type) of Cipher_Data_Type;

   type Object_Type is private;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Data_Idx :    out Item_Index_Type)
   with
      Pre => (Primitive_Acceptable (Obj) and then Primitive.Valid (Prim));

   --
   --  Peek_Generated_Primitive
   --
   procedure Peek_Generated_Primitive (
      Obj      :     Object_Type;
      Item_Idx : out Item_Index_Type;
      Prim     : out Primitive.Object_Type);

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj        : in out Object_Type;
      Item_Index :        Item_Index_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type);

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj        : in out Object_Type;
      Item_Index :        Item_Index_Type;
      Success    :        Boolean);

   --
   --  Data_Index
   --
   function Data_Index (
      Obj  : Crypto.Object_Type;
      Prim : Primitive.Object_Type)
   return Item_Index_Type;

   --
   --  FIXME This function shouldn't be necessary in the future as we should
   --        always remember a definite correlation from request to primitive.
   --        This is currently not the case at least when creating a request
   --        for the CBE crypto back-end from a primitive of the crypto module.
   --
   function Data_Index_By_Request (
      Obj : Crypto.Object_Type;
      Req : Request.Object_Type)
   return Item_Index_Type;

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean;

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
      --  Mark_Completed_Primitive
      --
      procedure Mark_Completed_Primitive (
         Obj     : in out Item_Type;
         Success :        Boolean);

      --
      --  Invalid_Object
      --
      function Invalid_Object
      return Item_Type;

      --
      --  Submitted_Encryption_Object
      --
      function Submitted_Encryption_Object (Prm : Primitive.Object_Type)
      return Item_Type;

      --
      --  Submitted_Decryption_Object
      --
      function Submitted_Decryption_Object (Prm : Primitive.Object_Type)
      return Item_Type;

      ----------------------
      --  Read Accessors  --
      ----------------------

      function Invalid     (Obj : Item_Type) return Boolean;
      function Pending     (Obj : Item_Type) return Boolean;
      function In_Progress (Obj : Item_Type) return Boolean;
      function Complete    (Obj : Item_Type) return Boolean;
      function Prim        (Obj : Item_Type) return Primitive.Object_Type;

      -----------------------
      --  Write Accessors  --
      -----------------------

      procedure State (Obj : in out Item_Type; Sta : State_Type)
      with Pre => not Invalid (Obj);

   private

      --
      --  Item_Type
      --
      type Item_Type is record
         State : State_Type;
         Prim  : Primitive.Object_Type;
      end record;

   end Item;

   type Items_Type is array (Item_Index_Type) of Item.Item_Type;

   --
   --  Object_Type
   --
   type Object_Type is record
      Items            : Items_Type;
      Execute_Progress : Boolean;
   end record;

end CBE.Crypto;
