--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.CXX
with SPARK_Mode
is
   function CXX_Crypto_Cipher_Buffer_Index_To_SPARK (
      Input : CXX_Crypto_Cipher_Buffer_Index_Type)
   return Crypto.Cipher_Buffer_Index_Type
   is
   begin
   end CXX_Crypto_Cipher_Buffer_Index_To_SPARK

end CBE.CXX;
