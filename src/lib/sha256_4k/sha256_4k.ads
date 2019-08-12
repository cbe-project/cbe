--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero Gener-- version 3.
--

package SHA256_4K with SPARK_Mode
is
   pragma Pure;

   type Byte is mod 2**8 with Size => 8;
   type Hash_Base_type is array (Natural range <>) of Byte;
   subtype Hash_Index_Type is Natural range 1 .. 32;
   subtype Hash_Type is Hash_Base_type (Hash_Index_Type);

   type Data_Base_Type is array (Natural range <>) of Byte;
   subtype Data_Index_Type is Natural range 1 .. 4096;
   subtype Data_Type is Data_Base_Type (Data_Index_Type);

   procedure Hash (Data  :     Data_Type;
                   Hash        : out Hash_type)
   with Export,
      Convention    => C,
      External_Name => "_ZN9Sha256_4k4hashERKNS_4DataERNS_4HashE";

end SHA256_4K;
