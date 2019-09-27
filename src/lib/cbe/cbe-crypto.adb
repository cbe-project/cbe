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
      --  Execute
      --
      procedure Execute (
         Obj : in out Item_Type;
         Key :        Key_Type)
      is
      begin
         case Primitive.Operation (Obj.Prim) is
         when Write =>

            Aes_Cbc_4k.Encrypt (
               Key,
               Aes_Cbc_4k.Block_Number_Type (
                  Primitive.Block_Number (Obj.Prim)),
               Obj.Plain_Data, Obj.Cipher_Data);

            Primitive.Success (Obj.Prim, True);
            Obj.State := Complete;

         when others =>

            Obj.State := Pending;

         end case;
      end Execute;

      --
      --  Mark_Completed_Primitive
      --
      procedure Mark_Completed_Primitive (
         Obj : in out Item_Type;
         Prm :        Primitive.Object_Type;
         Key :        Key_Type)
      is
      begin
         if
            Obj.State /= In_Progress or else
            not Primitive.Equal (Obj.Prim, Prm)
         then
            return;
         end if;

         if
            Primitive.Operation (Obj.Prim) = Read and then
            Primitive.Success (Prm)
         then
            Aes_Cbc_4k.Decrypt (
               Key,
               Aes_Cbc_4k.Block_Number_Type (
                  Primitive.Block_Number (Obj.Prim)),
               Obj.Cipher_Data, Obj.Plain_Data);
         end if;

         Obj.State := Item.Complete;
         Primitive.Success (Obj.Prim, Primitive.Success (Prm));

      end Mark_Completed_Primitive;

      --
      --  Return true if crypto item belongs to primitive and the result
      --  is ready to be consumed.
      --
      function Item_Complete (
         Item : Item_Type;
         Prim : Primitive.Object_Type)
      return Boolean
      is (Item.State = Complete and then Primitive.Equal (Item.Prim, Prim));

      --
      --  Copy_Decrypted_Data
      --
      procedure Copy_Decrypted_Data (
         Item       :     Item_Type;
         Prim       :     Primitive.Object_Type;
         Plain_Data : out Plain_Data_Type)
      is
      begin
         if not Item_Complete (Item, Prim) then
            return;
         end if;

         if
            Primitive.Operation (Item.Prim) = Read and then
            Primitive.Success (Prim)
         then
            Plain_Data := Item.Plain_Data;
         end if;
      end Copy_Decrypted_Data;

      --
      --  Copy_Encrypted_Data
      --
      procedure Copy_Encrypted_Data (
         Item        :     Item_Type;
         Prim        :     Primitive.Object_Type;
         Cipher_Data : out Cipher_Data_Type)
      is
      begin
         if not Item_Complete (Item, Prim) then
            return;
         end if;

         if
            Primitive.Operation (Item.Prim) = Write and then
            Primitive.Success (Prim)
         then
            Cipher_Data := Item.Cipher_Data;
         end if;
      end Copy_Encrypted_Data;

      --
      --  Invalid_Object
      --
      function Invalid_Object
      return Item_Type
      is (
         State       => Invalid,
         Prim        => Primitive.Invalid_Object,
         Plain_Data  => (others => 0),
         Cipher_Data => (others => 0));

      --
      --  Submitted_Encryption_Object
      --
      function Submitted_Encryption_Object (
         Prm        : Primitive.Object_Type;
         Plain_Dat  : Plain_Data_Type)
      return Item_Type
      is (
         State       => Submitted,
         Prim        => Prm,
         Plain_Data  => Plain_Dat,
         Cipher_Data => (others => 0));

      --
      --  Submitted_Decryption_Object
      --
      function Submitted_Decryption_Object (
         Prm        : Primitive.Object_Type;
         Cipher_Dat : Cipher_Data_Type)
      return Item_Type
      is (
         State       => Submitted,
         Prim        => Prm,
         Plain_Data  => (others => 0),
         Cipher_Data => Cipher_Dat);

      ----------------------
      --  Read Accessors  --
      ----------------------

      function Invalid (Obj : Item_Type) return Boolean
      is (Obj.State = Invalid);

      function Pending (Obj : Item_Type) return Boolean
      is (Obj.State = Pending);

      function Submitted (Obj : Item_Type) return Boolean
      is (Obj.State = Submitted);

      function In_Progress (Obj : Item_Type) return Boolean
      is (Obj.State = In_Progress);

      function Complete (Obj : Item_Type) return Boolean
      is (Obj.State = Complete);

      function Prim (Obj : Item_Type) return Primitive.Object_Type
      is (Obj.Prim);

      function Plain_Data (Obj : Item_Type) return Plain_Data_Type
      is (Obj.Plain_Data);

      function Cipher_Data (Obj : Item_Type) return Cipher_Data_Type
      is (Obj.Cipher_Data);

      -----------------------
      --  Write Accessors  --
      -----------------------

      procedure State (Obj : in out Item_Type; Sta : State_Type)
      is begin Obj.State := Sta; end State;

   end Item;

   --
   --  Initialize_Object
   --
   procedure Initialize_Object (
      Obj : out Object_Type;
      Key :     Key_Type)
   is
   begin
      Obj := Initialized_Object (Key);
   end Initialize_Object;

   --
   --  Initialized_Object
   --
   function Initialized_Object (Key : Key_Type)
   return Object_Type
   is (
      Key              => Key,
      Items            => (others => Item.Invalid_Object),
      Execute_Progress => False);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (for some Itm of Obj.Items => Item.Invalid (Itm));

   --
   --  Submit_Encryption_Primitive
   --
   procedure Submit_Encryption_Primitive (
      Obj         : in out Object_Type;
      Prim        :        Primitive.Object_Type;
      Plain_Data  :        Plain_Data_Type)
   is
      Prim_Buf : constant Primitive.Object_Type := Prim;
   begin

      Items_Loop : for Item_Id in Obj.Items'Range loop

         if Item.Invalid (Obj.Items (Item_Id)) then

            Obj.Items (Item_Id) :=
               Item.Submitted_Encryption_Object (Prim_Buf, Plain_Data);

            exit Items_Loop;

         end if;

      end loop Items_Loop;

   end Submit_Encryption_Primitive;

   --
   --  Submit_Decryption_Primitive
   --
   procedure Submit_Decryption_Primitive (
      Obj         : in out Object_Type;
      Prim        :        Primitive.Object_Type;
      Cipher_Data :        Cipher_Data_Type)
   is
      Prim_Buf : constant Primitive.Object_Type := Prim;
   begin

      Items_Loop : for Item_Id in Obj.Items'Range loop

         if Item.Invalid (Obj.Items (Item_Id)) then

            Obj.Items (Item_Id) :=
               Item.Submitted_Decryption_Object (Prim_Buf, Cipher_Data);

            exit Items_Loop;

         end if;

      end loop Items_Loop;

   end Submit_Decryption_Primitive;

   --
   --  Execute
   --
   procedure Execute (Obj : in out Object_Type)
   is
   begin
      Obj.Execute_Progress := False;
      Items_Loop : for Item_Id in Obj.Items'Range loop
         if Item.Submitted (Obj.Items (Item_Id)) then
            Item.Execute (Obj.Items (Item_Id), Obj.Key);
            Obj.Execute_Progress := True;
         end if;
      end loop Items_Loop;
   end Execute;

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      Items_Loop : for Item_Id in Obj.Items'Range loop
         if Item.Pending (Obj.Items (Item_Id)) then
            return Item.Prim (Obj.Items (Item_Id));
         end if;
      end loop Items_Loop;
      return Primitive.Invalid_Object;
   end Peek_Generated_Primitive;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Items_Loop : for Item_Id in Obj.Items'Range loop
         if Item.Pending (Obj.Items (Item_Id)) then
            Item.State (Obj.Items (Item_Id), Item.In_Progress);
            return;
         end if;
      end loop Items_Loop;
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
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
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
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      for Item_Id in Obj.Items'Range loop
         Item.Mark_Completed_Primitive (Obj.Items (Item_Id), Prim, Obj.Key);
      end loop;
   end Mark_Completed_Primitive;

   procedure Copy_Decrypted_Data (
      Obj        :     Crypto.Object_Type;
      Prim       :     Primitive.Object_Type;
      Plain_Data : out Crypto.Plain_Data_Type)
   is
   begin
      for Item_Id in Obj.Items'Range loop
         Item.Copy_Decrypted_Data (Obj.Items (Item_Id), Prim, Plain_Data);
      end loop;
   end Copy_Decrypted_Data;

   procedure Copy_Encrypted_Data (
      Obj         :     Crypto.Object_Type;
      Prim        :     Primitive.Object_Type;
      Cipher_Data : out Cipher_Data_Type)
   is
   begin
      for Item_Id in Obj.Items'Range loop
         Item.Copy_Encrypted_Data (Obj.Items (Item_Id), Prim, Cipher_Data);
      end loop;
   end Copy_Encrypted_Data;

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean
   is (Obj.Execute_Progress);

end CBE.Crypto;
