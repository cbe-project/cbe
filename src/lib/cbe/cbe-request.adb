--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;

package body CBE.Request
with SPARK_Mode
is
   --
   --  Invalid_Object
   --
   function Invalid_Object
   return Object_Type
   is (
      Valid        => False,
      Operation    => Read,
      Success      => False,
      Block_Number => 0,
      Offset       => 0,
      Count        => 0,
      Tag          => 0);

   --
   --  Valid_Object
   --
   function Valid_Object (
      Op     : Operation_Type;
      Succ   : Request.Success_Type;
      Blk_Nr : Block_Number_Type;
      Off    : Offset_Type;
      Cnt    : Count_Type;
      Tg     : Tag_Type)
   return Object_Type
   is (
      Valid        => True,
      Operation    => Op,
      Success      => Succ,
      Block_Number => Blk_Nr,
      Offset       => Off,
      Count        => Cnt,
      Tag          => Tg);

   --
   --  Equal
   --
   function Equal (
      Obj_1 : Object_Type;
      Obj_2 : Object_Type)
   return Boolean
   is (
      Obj_1.Block_Number = Obj_2.Block_Number and then
      Obj_1.Tag          = Obj_2.Tag and then
      Obj_1.Operation    = Obj_2.Operation);

   -----------------
   --  Accessors  --
   -----------------

   function Valid       (Obj : Object_Type) return Boolean
   is (Obj.Valid);

   function Operation   (Obj : Object_Type) return Operation_Type
   is (Obj.Operation);

   function Success     (Obj : Object_Type) return Success_Type
   is (Obj.Success);

   function Block_Number (Obj : Object_Type) return Block_Number_Type
   is (Obj.Block_Number);

   function Offset      (Obj : Object_Type) return Offset_Type
   is (Obj.Offset);

   function Count       (Obj : Object_Type) return Count_Type
   is (Obj.Count);

   function Tag         (Obj : Object_Type) return Tag_Type
   is (Obj.Tag);

   procedure Success (
      Obj  : in out Object_Type;
      Succ : Success_Type)
   is
   begin
      Obj.Success := Succ;
   end Success;

   function To_String (B : Block_Number_Type) return String
   is
   begin
      return Debug.To_String (Debug.Uint64_Type (B));
   end To_String;

   function To_String (Obj : Object_Type)
   return String
   is (
      if not Obj.Valid then
         "Invalid Request"
      else
         "Request (Op=" &
         Obj.Operation'Image &
         " Tag=" &
         Debug.To_String (Obj.Tag) &
         " Success=" &
         Debug.To_String (Obj.Success) &
         " Block_Number=" &
         Request.To_String (Obj.Block_Number) &
         " Offset=" &
         Debug.To_String (Debug.Uint64_Type (Obj.Offset)) &
         " Count=" &
         Debug.To_String (Debug.Uint64_Type (Obj.Count)) &
         ")");

end CBE.Request;
