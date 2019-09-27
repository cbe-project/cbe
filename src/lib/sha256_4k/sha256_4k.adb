--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with LSC.SHA2_Generic;

package body SHA256_4K with SPARK_Mode
is
   procedure Hash (Data :     Data_Type;
                   Hash : out Hash_Type)
   is

      function H is new LSC.SHA2_Generic.Hash_SHA256
         (Natural, Byte, Data_Base_Type, Hash_Index_Type, Byte, Hash_Type);

   begin
      Hash := H (Data);
   end Hash;

end SHA256_4K;
