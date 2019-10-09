--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;

package CBE.CXX
with SPARK_Mode
is
   pragma Pure;

   type CXX_UInt8_Type           is range 0 .. 2**8  - 1 with Size => 1 * 8;
   type CXX_UInt32_Type          is range 0 .. 2**32 - 1 with Size => 4 * 8;
   type CXX_UInt64_Type          is mod        2**64     with Size => 8 * 8;
   type CXX_Bool_Type            is range 0 .. 1         with Size => 1 * 8;
   type CXX_Operation_Type       is range 0 .. 3         with Size => 4 * 8;
   type CXX_Success_Type         is range 0 .. 1         with Size => 4 * 8;
   type CXX_Object_Size_Type     is new CXX_UInt32_Type;
   type CXX_Block_Number_Type    is new CXX_UInt64_Type;
   type CXX_Timestamp_Type       is new CXX_UInt64_Type;
   type CXX_Tag_Type             is new CXX_UInt32_Type;
   type CXX_Block_Offset_Type    is new CXX_UInt64_Type;
   type CXX_Block_Count_Type     is new CXX_UInt32_Type;
   type CXX_Primitive_Index_Type is new CXX_UInt64_Type;

   type CXX_Superblocks_Index_Type is record
      Value : CXX_UInt64_Type;
   end record;

   type CXX_Crypto_Plain_Buffer_Index_Type is record
      Value : CXX_Uint32_Type;
   end record;
   pragma Pack (CXX_Crypto_Plain_Buffer_Index_Type);

   type CXX_Crypto_Cipher_Buffer_Index_Type is record
      Value : CXX_Uint32_Type;
   end record;
   pragma Pack (CXX_Crypto_Cipher_Buffer_Index_Type);

   type CXX_Request_Type is record
      Operation    : CXX_Operation_Type;
      Success      : CXX_Success_Type;
      Block_Number : CXX_Block_Number_Type;
      Offset       : CXX_Block_Offset_Type;
      Count        : CXX_Block_Count_Type;
      Tag          : CXX_Tag_Type;
   end record;
   pragma Pack (CXX_Request_Type);

   function CXX_Bool_From_SPARK (Input : Boolean)
   return CXX_Bool_Type
   is (
      case Input is
      when False => 0,
      when True  => 1);

   function CXX_Bool_To_SPARK (Input : CXX_Bool_Type)
   return Boolean
   is (
      case Input is
      when 0 => False,
      when 1 => True);

   function CXX_Success_From_SPARK (Input : Request.Success_Type)
   return CXX_Success_Type
   is (
      case Input is
      when False => 0,
      when True  => 1);

   function CXX_Success_To_SPARK (Input : CXX_Success_Type)
   return Request.Success_Type
   is (
      case Input is
      when 0 => False,
      when 1 => True);

   function CXX_Operation_From_SPARK (Input : Operation_Type)
   return CXX_Operation_Type
   is (
      case Input is
      when Read  => 1,
      when Write => 2,
      when Sync  => 3);

   function CXX_Request_Valid_To_SPARK (
      Req : CXX_Request_Type;
      Op  : Operation_Type)
   return Request.Object_Type
   is (
      Request.Valid_Object (
         Op,
         CXX_Success_To_SPARK (Req.Success),
         Block_Number_Type    (Req.Block_Number),
         Request.Offset_Type  (Req.Offset),
         Request.Count_Type   (Req.Count),
         Tag_Type             (Req.Tag)));

   function CXX_Request_To_SPARK (Input : CXX_Request_Type)
   return Request.Object_Type
   is (
      case Input.Operation is
      when 0 => Request.Invalid_Object,
      when 1 => CXX_Request_Valid_To_SPARK (Input, Read),
      when 2 => CXX_Request_Valid_To_SPARK (Input, Write),
      when 3 => CXX_Request_Valid_To_SPARK (Input, Sync));

   function CXX_Request_From_SPARK (Obj : Request.Object_Type)
   return CXX_Request_Type
   is (
      case Request.Valid (Obj) is
      when True => (
         Operation    => CXX_Operation_From_SPARK (Request.Operation    (Obj)),
         Success      => CXX_Success_From_SPARK   (Request.Success      (Obj)),
         Block_Number => CXX_Block_Number_Type    (Request.Block_Number (Obj)),
         Offset       => CXX_Block_Offset_Type    (Request.Offset       (Obj)),
         Count        => CXX_Block_Count_Type     (Request.Count        (Obj)),
         Tag          => CXX_Tag_Type             (Request.Tag          (Obj))),
      when False => (
         Operation    => 0,
         Success      => 0,
         Block_Number => 0,
         Offset       => 0,
         Count        => 0,
         Tag          => 0));

end CBE.CXX;
