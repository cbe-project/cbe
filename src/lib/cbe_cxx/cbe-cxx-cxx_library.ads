--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Library;
with CBE.Crypto;
with CBE.Block_IO;

package CBE.CXX.CXX_Library
with SPARK_Mode
is
   --  FIXME cannot be pure yet because of CBE.Library
   --  pragma Pure;

   function Object_Size (Obj : Library.Object_Type)
   return CXX_Object_Size_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZN3Cbe11object_sizeERKNS_7LibraryE";

   procedure Initialize_Object (
      Obj     : out Library.Object_Type;
      SBs     :     Superblocks_Type;
      Curr_SB :     CXX_Superblocks_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7LibraryC2ERKNS_11SuperblocksERKNS_17Superblocks_indexE";

   function Cache_Dirty (Obj : Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library11cache_dirtyEv";

   function Superblock_Dirty (Obj : Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library16superblock_dirtyEv";

   function Is_Securing_Superblock (Obj : Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library22is_securing_superblockEv";

   function Is_Sealing_Generation (Obj : Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library21is_sealing_generationEv";

   procedure Start_Securing_Superblock (Obj : in out Library.Object_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZN3Cbe7Library25start_securing_superblockEv";

   procedure Start_Sealing_Generation (Obj : in out Library.Object_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZN3Cbe7Library24start_sealing_generationEv";

   function Max_VBA (Obj : Library.Object_Type)
   return Virtual_Block_Address_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library7max_vbaEv";

   procedure Execute (
      Obj               : in out Library.Object_Type;
      IO_Buf            : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Now               :        Timestamp_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library7executeERNS_9Io_bufferERNS_" &
         "19Crypto_plain_bufferERNS_20Crypto_cipher_bufferEy";

   function Request_Acceptable (Obj : Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library25client_request_acceptableEv";

   procedure Submit_Request (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZN3Cbe7Library21submit_client_requestERKNS_7RequestE";

   function Peek_Completed_Request (Obj : Library.Object_Type)
   return CXX_Request_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library29peek_completed_client_requestEv";

   procedure Drop_Completed_Request (Obj : in out Library.Object_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library29drop_completed_client_requestEv";

   procedure IO_Request_Completed (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_Crypto_Cipher_Buffer_Index_Type;
      Success    :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library20io_request_completedERKNS_9Io_buffer5IndexEb";

   procedure Has_IO_Request (
      Obj        : in out Library.Object_Type;
      Req        :    out CXX_Request_Type;
      Data_Index :    out CXX_IO_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library15_has_io_requestERNS_7RequestERNS_9Io_buffer5IndexE";

   procedure IO_Request_In_Progress (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_IO_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library22io_request_in_progressERKNS_9Io_buffer5IndexE";

   procedure Client_Data_Ready (
      Obj : in out Library.Object_Type;
      Req :    out CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZN3Cbe7Library18_client_data_readyERNS_7RequestE";

   function Give_Data_Index (
      Obj : Library.Object_Type;
      Req : CXX_Request_Type)
   return CXX_Primitive_Index_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library15give_data_indexERKNS_7RequestE";

   procedure Obtain_Client_Data (
      Obj              : in out Library.Object_Type;
      Req              :        CXX_Request_Type;
      Data_Index       :    out CXX_Crypto_Plain_Buffer_Index_Type;
      Data_Index_Valid :    out CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library19_obtain_client_dataERKNS_7RequestERNS_" &
         "19Crypto_plain_buffer5IndexERb";

   procedure Client_Data_Required (
      Obj : in out Library.Object_Type;
      Req :    out CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZN3Cbe7Library21_client_data_requiredERNS_7RequestE";

   procedure Supply_Client_Data (
      Obj      : in out Library.Object_Type;
      Now      :        Timestamp_Type;
      Req      :        CXX_Request_Type;
      Data     :        Block_Data_Type;
      Progress :    out CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library19_supply_client_dataEyRKNS_7RequestERKNS_" &
         "10Block_dataERb";

   function Execute_Progress (Obj : Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library16execute_progressEv";

   procedure Crypto_Cipher_Data_Required (
      Obj        : in out Library.Object_Type;
      Req        :    out CXX_Request_Type;
      Data_Index :    out CXX_Crypto_Plain_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library28_crypto_cipher_data_requiredERNS_7RequestERNS_" &
         "19Crypto_plain_buffer5IndexE";

   procedure Crypto_Cipher_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_Crypto_Plain_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library28crypto_cipher_data_requestedERKNS_" &
         "19Crypto_plain_buffer5IndexE";

   procedure Supply_Crypto_Cipher_Data (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_Crypto_Cipher_Buffer_Index_Type;
      Data_Valid :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library25supply_crypto_cipher_dataERKNS_" &
         "20Crypto_cipher_buffer5IndexEb";

   procedure Crypto_Plain_Data_Required (
      Obj        : in out Library.Object_Type;
      Req        :    out CXX_Request_Type;
      Data_Index :    out CXX_Crypto_Cipher_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library27_crypto_plain_data_requiredERNS_7RequestERNS_" &
         "20Crypto_cipher_buffer5IndexE";

   procedure Crypto_Plain_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_Crypto_Cipher_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library27crypto_plain_data_requestedERKNS_" &
         "20Crypto_cipher_buffer5IndexE";

   procedure Supply_Crypto_Plain_Data (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_Crypto_Plain_Buffer_Index_Type;
      Data_Valid :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library24supply_crypto_plain_dataERKNS_" &
         "19Crypto_plain_buffer5IndexEb";

end CBE.CXX.CXX_Library;
