--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Pool;
with CBE.Splitter;
with CBE.Crypto;
with CBE.Cache;
with CBE.Cache_Flusher;
with CBE.Virtual_Block_Device;
with CBE.Write_Back;
with CBE.Sync_Superblock;
with CBE.Free_Tree;
with CBE.Block_IO;
with CBE.Request;
with CBE.Primitive;

package CBE.Library
with SPARK_Mode
is
   --  FIXME cannot be pure yet because of CBE.Crypto
   --  pragma Pure;

   type Object_Type is private;

   --
   --  Constructor
   --
   --  \param  now     current time as timestamp
   --  \param  sync    interval in ms after which the current generation
   --                 should be sealed
   --  \param  secure  interval in ms after which the current super-block
   --                 should be secured
   --  \param  block   reference to the Block::Connection used by the I/O
   --                 module
   --  \param  sbs     array of all super-blocks, will be copied
   --
   --  \param  current_sb  super-block that should be used initially
   --
   procedure Initialize_Object (
      Obj     : out Object_Type;
      SBs     :     Superblocks_Type;
      Curr_SB :     Superblocks_Index_Type);

   function Cache_Dirty (Obj : Object_Type)
   return Boolean;

   function Superblock_Dirty (Obj : Object_Type)
   return Boolean;

   function Is_Securing_Superblock (Obj : Object_Type)
   return Boolean;

   function Is_Sealing_Generation (Obj : Object_Type)
   return Boolean;

   procedure Start_Securing_Superblock (Obj : in out Object_Type);

   procedure Start_Sealing_Generation (Obj : in out Object_Type);

   --
   --  Check if the CBE can accept a new requeust
   --
   --  \return true if a request can be accepted, otherwise false
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit a new request
   --
   --  This method must only be called after executing 'Request_Acceptable'
   --  returned true.
   --
   --  \param Req  block request
   --
   procedure Submit_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Check for any completed request
   --
   --  \return a valid block request will be returned if there is an
   --         completed request, otherwise an invalid one
   --
   function Peek_Completed_Request (Obj : Object_Type)
   return Request.Object_Type;

   --
   --  Drops the completed request
   --
   --  This method must only be called after executing
   --  'Peek_Completed_Request' returned a valid request.
   --
   procedure Drop_Completed_Request (Obj : in out Object_Type);

   --
   --  Submit read request data from the backend block session to the CBE
   --
   --  The given data will be transfered to the CBE.
   --
   --  \param Req       reference to the request from the CBE
   --  \param Data      reference to the data associated with the request
   --  \param Progress  return true if the CBE acknowledged the request
   --
   procedure IO_Request_Completed (
      Obj        : in out Object_Type;
      Data_Index :        Block_IO.Data_Index_Type;
      Success    :        Boolean);

   --
   --  Return write request for the backend block session
   --
   --  \param Req  return valid request in case the is one pending that
   --             needs data, otherwise an invalid one is returned
   --
   procedure Has_IO_Request (
      Obj      : in out Object_Type;
      Req      :    out Request.Object_Type;
      Data_Idx :    out Block_IO.Data_Index_Type);

   --
   --  Obtain data for write request for the backend block session
   --
   --  The CBE will transfer the payload to the given data.
   --
   --  \param Req       reference to the request processed by the CBE
   --  \param Data      reference to the data associated with the request
   --  \param Progress  return true if the CBE could process the request
   --
   procedure IO_Request_In_Progress (
      Obj      : in out Object_Type;
      Data_Idx :        Block_IO.Data_Index_Type);

   --
   --  Return a client request that provides data to the frontend block data
   --
   --  \param Req  return valid request in case the is one pending that
   --             needs data, otherwise an invalid one is returned
   --
   procedure Client_Data_Ready (
      Obj : in out Object_Type;
      Req :    out Request.Object_Type);

   --
   --  Get primitive index
   --
   function Give_Data_Index (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Primitive.Index_Type;

   --
   --  Return data index for the given client read request
   --
   --  \param Req              reference to the request processed by the CBE
   --  \param Data_Index       returns index of client data in crypto
   --                          plain-data buffer if Data_Index_Valid returns
   --                          True
   --  \param Data_Index_Valid returns whether Data_Index is valid
   --
   procedure Obtain_Client_Data (
      Obj              : in out Object_Type;
      Req              :        Request.Object_Type;
      Data_Index       :    out Crypto.Plain_Buffer_Index_Type;
      Data_Index_Valid :    out Boolean);

   --
   --  Return a client request that provides data to the frontend block data
   --
   --  \param Req  return valid request in case the is one pending that
   --             needs data, otherwise an invalid one is returned
   --
   procedure Client_Data_Required (
      Obj : in out Object_Type;
      Req :    out Request.Object_Type);

   --
   --  Request access to the Block::Request data for reading data
   --
   --  \param Request  reference to the Block::Request processed
   --                 by the CBE
   --  \param Data     reference to the data associated with the
   --                 Block::Request
   --
   --  \return  true if the CBE could process the request
   --
   procedure Supply_Client_Data (
      Obj      : in out Object_Type;
      Now      :        Timestamp_Type;
      Req      :        Request.Object_Type;
      Data     :        Block_Data_Type;
      Progress :    out Boolean);

   --
   --  Determine whether the encryption of plain data is required
   --
   --  \param Req         returns valid request if encryption of plain data is
   --                     is required
   --  \param Data_Index  returns index of plain data in crypto buffer if
   --                     returned request is valid
   --
   procedure Crypto_Cipher_Data_Required (
      Obj        : in out Object_Type;
      Req        :    out Request.Object_Type;
      Data_Index :    out Crypto.Plain_Buffer_Index_Type);

   --
   --  Acknowledge that the encryption of plain data was requested
   --
   --  \param Data_Index  index of plain data in crypto buffer
   --
   procedure Crypto_Cipher_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type);

   --
   --  Acknowledge that the encryption of plain data was completed
   --
   --  \param Data_Index  index of the resulting cipher data in crypto buffer
   --  \param Data_Valid  whether the encryption was successful
   --
   procedure Supply_Crypto_Cipher_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type;
      Data_Valid :        Boolean);

   --
   --  Request decryption of data
   --
   --  \param Req         returns valid request in case there is one pending
   --                     that needs data, otherwise an invalid one is returned
   --  \param Data_Index  returns index of request data-slot in crypto
   --                     plain-data buffer if returned request is valid
   --
   procedure Crypto_Plain_Data_Required (
      Obj        : in out Object_Type;
      Req        :    out Request.Object_Type;
      Data_Index :    out Crypto.Cipher_Buffer_Index_Type);

   --
   --  Acknowledge that required crypto plain-data was requested
   --
   --  \param Data_Index  index of the data in crypto buffer
   --
   procedure Crypto_Plain_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type);

   --
   --  Collect plain data for given completed decryption request
   --
   --  \param Data_Index  index of data in crypto plain-data buffer
   --  \param Data_Valid  whether the data is valid or not (whether the
   --                     data could be processed successfully by the outer
   --                     world)
   --
   procedure Supply_Crypto_Plain_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type;
      Data_Valid :        Boolean);

   --
   --  Execute one loop of the CBE
   --
   --  \param  now               current time as timestamp
   --  \param  show_progress     if true, generate a LOG message of the current
   --                            progress (basically shows the progress state
   --                            of all modules)
   --  \param  show_if_progress  if true, generate LOG message only when
   --                            progress was
   --                            acutally made
   --
   procedure Execute (
      Obj               : in out Object_Type;
      IO_Buf            : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Now               :        Timestamp_Type);

   --
   --  Get highest virtual-block-address useable by the current active snapshot
   --
   --  \return  highest addressable virtual-block-address
   --
   function Max_VBA (Obj : Object_Type)
   return Virtual_Block_Address_Type;

   function Execute_Progress (Obj : Object_Type)
   return Boolean;

   function To_String (Obj : Object_Type) return String;

private

   type Free_Tree_Retry_Count_Type is mod 2**32;

   Free_Tree_Retry_Limit : constant := 3;

   --
   --  Defining the structure here is just an interims solution
   --  and should be properly managed, especially handling more
   --  than one request is "missing".
   --
   type Request_Primitive_Type is record
      Req         : Request.Object_Type;
      Prim        : Primitive.Object_Type;
      Tag         : Tag_Type;
      In_Progress : Boolean;
   end record;

   function Request_Primitive_Invalid
   return Request_Primitive_Type
   is (
      Req         => Request.Invalid_Object,
      Prim        => Primitive.Invalid_Object,
      Tag         => Tag_Invalid,
      In_Progress => False);

   type Object_Type is record
      Execute_Progress        : Boolean;
      Request_Pool_Obj        : Pool.Object_Type;
      Splitter_Obj            : Splitter.Object_Type;
      Crypto_Obj              : Crypto.Object_Type;
      IO_Obj                  : Block_IO.Object_Type;
      Cache_Obj               : Cache.Object_Type;
      Cache_Data              : Cache.Cache_Data_Type;
      Cache_Job_Data          : Cache.Cache_Job_Data_Type;
      Cache_Flusher_Obj       : Cache_Flusher.Object_Type;
      Trans_Data              : Translation_Data_Type;
      VBD                     : Virtual_Block_Device.Object_Type;
      Write_Back_Obj          : Write_Back.Object_Type;
      Write_Back_Data         : Write_Back.Data_Type;
      Sync_SB_Obj             : Sync_Superblock.Object_Type;
      Free_Tree_Obj           : Free_Tree.Object_Type;
      Free_Tree_Retry_Count   : Free_Tree_Retry_Count_Type;
      Free_Tree_Trans_Data    : Translation_Data_Type;
      Free_Tree_Query_Data    : Query_Data_Type;
      Superblocks             : Superblocks_Type;
      Cur_SB                  : Superblocks_Index_Type;
      Cur_Gen                 : Generation_Type;
      Last_Secured_Generation : Generation_Type;
      Last_Snapshot_ID        : Snapshot_ID_Type;
      Seal_Generation         : Boolean;
      Secure_Superblock       : Boolean;
      Superblock_Dirty        : Boolean;
      Front_End_Req_Prim      : Request_Primitive_Type;
      Back_End_Req_Prim       : Request_Primitive_Type;
   end record;

   procedure Discard_Snapshot (
      Snaps     : in out Snapshots_Type;
      Keep_Snap :        Snapshots_Index_Type;
      Success   :    out Boolean);

   function To_String (Req_Prim : Request_Primitive_Type) return String;

   function Curr_Snap (Obj : Object_Type)
   return Snapshots_Index_Type;

   procedure Create_New_Snapshot (
      Obj  : in out Object_Type;
      Snap :        Snapshots_Index_Type;
      Prim :        Primitive.Object_Type);

   procedure Update_Snapshot_Hash (
      Obj  :        Object_Type;
      Snap : in out Snapshot_Type;
      Prim :        Primitive.Object_Type);

   procedure Assign_Front_End_Req_Prim (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Tag  :        Tag_Type);

end CBE.Library;
