--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;

package CBE.Primitive
with SPARK_Mode
is
   pragma Pure;

   type Tag_Type is (
      Tag_Invalid,
      Tag_Splitter,
      Tag_IO,
      Tag_Translation,
      Tag_Write_Back,
      Tag_Cache,
      Tag_Cache_Flush,
      Tag_Decrypt,
      Tag_Encrypt,
      Tag_Sync_SB,
      Tag_Free_Tree_Query,
      Tag_Free_Tree_IO,
      Tag_Free_Tree_WB);

   function To_String (Tag : Tag_Type)
   return String
   is (
      case Tag is
      when Tag_Invalid => "Invalid",
      when Tag_Splitter => "Splitter",
      when Tag_IO => "IO",
      when Tag_Translation => "Translation",
      when Tag_Write_Back => "Write_Back",
      when Tag_Cache => "Cache",
      when Tag_Cache_Flush => "Cache_Flush",
      when Tag_Decrypt => "Decrypt",
      when Tag_Encrypt => "Encrypt",
      when Tag_Sync_SB => "Sync_SB",
      when Tag_Free_Tree_Query => "Free_Tree_Query",
      when Tag_Free_Tree_IO => "Free_Tree_IO",
      when Tag_Free_Tree_WB => "Free_Tree_WB");

   type Index_Type  is mod 2**64;
   type Object_Type is private;

   function Invalid_Index return Index_Type is (Index_Type'Last);

   --
   --  Copy_Valid_Object_New_Tag
   --
   function Copy_Valid_Object_New_Tag (
      Obj : Object_Type;
      Tag : Tag_Type)
   return Object_Type
   with Pre => (Valid (Obj));

   --
   --  Copy_Valid_Object_New_Succ_Blk_Nr
   --
   function Copy_Valid_Object_New_Succ_Blk_Nr (
      Obj    : Object_Type;
      Succ   : Request.Success_Type;
      Blk_Nr : Block_Number_Type)
   return Object_Type
   with Pre => (Valid (Obj));

   --
   --  Invalid_Object
   --
   function Invalid_Object
   return Object_Type
   with Post => (not Valid (Invalid_Object'Result));

   --
   --  Valid_Object
   --
   function Valid_Object (
      Op     : Operation_Type;
      Succ   : Request.Success_Type;
      Tg     : Tag_Type;
      Pl_Idx : Pool_Index_Type;
      Blk_Nr : Block_Number_Type;
      Idx    : Index_Type)
   return Object_Type
   with
      Post => (
         Valid (Valid_Object'Result) and then (
            Operation    (Valid_Object'Result) = Op     and then
            Success      (Valid_Object'Result) = Succ   and then
            Block_Number (Valid_Object'Result) = Blk_Nr and then
            Index        (Valid_Object'Result) = Idx    and then
            Tag          (Valid_Object'Result) = Tg));

   --
   --  Valid_Object_No_Pool_Idx
   --
   function Valid_Object_No_Pool_Idx (
      Op     : Operation_Type;
      Succ   : Request.Success_Type;
      Tg     : Tag_Type;
      Blk_Nr : Block_Number_Type;
      Idx    : Index_Type)
   return Object_Type
   with
      Post => (
         Valid (Valid_Object_No_Pool_Idx'Result) and then (
            Operation    (Valid_Object_No_Pool_Idx'Result) = Op     and then
            Success      (Valid_Object_No_Pool_Idx'Result) = Succ   and then
            Block_Number (Valid_Object_No_Pool_Idx'Result) = Blk_Nr and then
            Index        (Valid_Object_No_Pool_Idx'Result) = Idx    and then
            Tag          (Valid_Object_No_Pool_Idx'Result) = Tg));

   --
   --  Equal
   --
   function Equal (
      Obj_1 : Object_Type;
      Obj_2 : Object_Type)
   return Boolean
   with
      Pre => Valid (Obj_1) and then Valid (Obj_2);

   ----------------------
   --  Read Accessors  --
   ----------------------

   function Valid (Obj : Object_Type) return Boolean;

   function Operation (Obj : Object_Type) return Operation_Type
   with Pre => (Valid (Obj));

   function Success (Obj : Object_Type) return Request.Success_Type
   with Pre => (Valid (Obj));

   function Tag (Obj : Object_Type) return Tag_Type
   with Pre => (Valid (Obj));

   function Pool_Idx_Slot (Obj : Object_Type) return Pool_Index_Slot_Type
   with Pre => (Valid (Obj));

   function Block_Number (Obj : Object_Type) return Block_Number_Type
   with Pre => (Valid (Obj));

   function Index (Obj : Object_Type) return Index_Type
   with Pre => (Valid (Obj));

   -----------------------
   --  Write Accessors  --
   -----------------------

   procedure Success (
      Obj   : in out Object_Type;
      Value : Request.Success_Type)
   with Pre => (Valid (Obj));

   procedure Block_Number (
      Obj   : in out Object_Type;
      Value : Block_Number_Type)
   with Pre => (Valid (Obj));

   procedure Operation (
      Obj   : in out Object_Type;
      Value : Operation_Type)
   with Pre => (Valid (Obj));

   function To_String (Obj : Object_Type) return String;

   function Has_Tag_Splitter (Obj : Object_Type) return Boolean;
   function Has_Tag_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_Translation (Obj : Object_Type) return Boolean;
   function Has_Tag_Write_Back (Obj : Object_Type) return Boolean;
   function Has_Tag_Cache (Obj : Object_Type) return Boolean;
   function Has_Tag_Cache_Flush (Obj : Object_Type) return Boolean;
   function Has_Tag_Decrypt (Obj : Object_Type) return Boolean;
   function Has_Tag_Encrypt (Obj : Object_Type) return Boolean;
   function Has_Tag_Sync_SB (Obj : Object_Type) return Boolean;
   function Has_Tag_Free_Tree_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_Free_Tree_WB (Obj : Object_Type) return Boolean;

private

   --
   --  Object_Type
   --
   type Object_Type is record
      Valid         : Boolean;
      Operation     : Operation_Type;
      Success       : Request.Success_Type;
      Tag           : Tag_Type;
      Pool_Idx_Slot : Pool_Index_Slot_Type;
      Block_Number  : Block_Number_Type;
      Index         : Index_Type;
   end record;

end CBE.Primitive;
