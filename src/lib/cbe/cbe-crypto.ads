--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Crypto
with SPARK_Mode
is
   --  Disable for now because of libsparkcrypto
   --  pragma Pure;

	subtype Plain_Data_Type  is CBE.Block_Data_Type;
	subtype Cipher_Data_Type is CBE.Block_Data_Type;

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
   --  Submit_Decryption_Primitive
   --
   procedure Submit_Decryption_Primitive (
      Obj         : in out Object_Type;
      Prim        :        Primitive.Object_Type;
      Cipher_Data :        Cipher_Data_Type)
   with
      Pre => (Primitive_Acceptable (Obj) and then Primitive.Valid (Prim));

   --
   --  Submit_Encryption_Primitive
   --
   procedure Submit_Encryption_Primitive (
      Obj         : in out Object_Type;
      Prim        :        Primitive.Object_Type;
      Plain_Data  :        Plain_Data_Type)
   with
      Pre => (Primitive_Acceptable (Obj) and then Primitive.Valid (Prim));

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   with
      Pre => (Primitive.Valid (Prim));

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
      Pre => (Primitive.Valid (Prim));

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Copy_Decrypted_Data
   --
   procedure Copy_Decrypted_Data (
      Obj        :     Object_Type;
      Prim       :     Primitive.Object_Type;
      Plain_Data : out Plain_Data_Type);

   --
   --  Copy_Encrypted_Data
   --
   procedure Copy_Encrypted_Data (
      Obj         :     Object_Type;
      Prim        :     Primitive.Object_Type;
      Cipher_Data : out Cipher_Data_Type);

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean;

	procedure Obtain_Plain_Data(
		Obj        :     Crypto.Object_Type;
		Prim       :     Primitive.Object_Type;
		Plain_Data : out Crypto.Plain_Data_Type);

	procedure Supply_Cipher_Data(
		Obj         : out Crypto.Object_Type;
		Prim        :     Primitive.Object_Type;
		Cipher_Data :     Cipher_Data_Type);

	procedure Obtain_Cipher_Data(
		Obj         :     Crypto.Object_Type;
		Prim        :     Primitive.Object_Type;
		Cipher_Data : out Cipher_Data_Type);

	procedure Supply_Plain_Data(
		Obj         : out Crypto.Object_Type;
		Prim        :     Primitive.Object_Type;
		Plain_Data :     Plain_Data_Type);

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
         Obj : in out Item_Type;
         Prm :        Primitive.Object_Type);

      --
      --  Copy_Decrypted_Data
      --
      procedure Copy_Decrypted_Data (
         Item       :     Item_Type;
         Prim       :     Primitive.Object_Type;
         Plain_Data : out Crypto.Plain_Data_Type);

      --
      --  Copy_Encrypted_Data
      --
      procedure Copy_Encrypted_Data (
         Item        :     Item_Type;
         Prim        :     Primitive.Object_Type;
         Cipher_Data : out Crypto.Cipher_Data_Type);

      --
      --  Invalid_Object
      --
      function Invalid_Object
      return Item_Type;

      --
      --  Submitted_Encryption_Object
      --
      function Submitted_Encryption_Object (
         Prm        : Primitive.Object_Type;
         Plain_Dat  : Plain_Data_Type)
      return Item_Type;

      --
      --  Submitted_Decryption_Object
      --
      function Submitted_Decryption_Object (
         Prm        : Primitive.Object_Type;
         Cipher_Dat : Cipher_Data_Type)
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

		--
		-- XXX remove later
		--

		procedure Obtain_Plain_Data (
			Item        :     Item_Type;
			Prim        :     Primitive.Object_Type;
			Plain_Data : out Plain_Data_Type);

		procedure Supply_Cipher_Data (
			Item        : out Item_Type;
			Prim        :     Primitive.Object_Type;
			Cipher_Data :     Cipher_Data_Type);

		procedure Obtain_Cipher_Data (
			Item        :     Item_Type;
			Prim        :     Primitive.Object_Type;
			Cipher_Data : out Cipher_Data_Type);

		procedure Supply_Plain_Data (
			Item        : out Item_Type;
			Prim        :     Primitive.Object_Type;
			Plain_Data :      Plain_Data_Type);

   private

      --
      --  Item_Type
      --
      type Item_Type is record
         State       : State_Type;
         Prim        : Primitive.Object_Type;
         Plain_Data  : Plain_Data_Type;
         Cipher_Data : Cipher_Data_Type;
      end record;

   end Item;

   type Items_Type is array (1 .. 1) of Item.Item_Type;

   --
   --  Object_Type
   --
   type Object_Type is record
      Items            : Items_Type;
      Execute_Progress : Boolean;
   end record;

end CBE.Crypto;
