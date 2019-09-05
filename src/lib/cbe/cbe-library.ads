--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
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
with Spark_Mode
is
--	FIXME cannot be pure yet because of CBE.Crypto
--	pragma Pure;

	type Key_Value_Index_Type is range 0 .. 63;
	type Key_Value_Type is array (Key_Value_Index_Type) of Byte_Type with Size => 64 * 8;

	--
	-- The Cbe::Key contains the key-material that is used to
	-- process cipher-blocks.
	--
	-- (For now it is not used but the ID field is already referenced
	--  by type 2 nodes.)
	--
	type Key_Type is record
		Value : Key_Value_Type;
		ID    : Key_ID_Type;
	end record with Size =>
		64 * 8 + -- Value
		 4 * 8;  -- ID

	type Keys_Index_Type is range 0 .. 1;
	type Keys_Type is array (Keys_Index_Type) of Key_Type;
	type Super_Block_Padding_Type is array (0 .. 423) of Byte_Type;

	--
	-- The Cbe::Super_block contains all information of a CBE
	-- instance including the list of active snapshots. For now
	-- the super-blocks are stored consecutively at the beginning
	-- of the block device, i.e., there is a 1:1 mapping between
	-- the physical-block-address and the SB id.
	--
	-- Per super-block we have a fixed number of snapshots (about
	-- the amount we can store within one disk sector). Whenever
	-- a generation is sealed, a new snapshot will be created
	-- automatically. If a snapshot is flagged as KEEP, it will never
	-- be overriden.
	--
	type Super_Block_Type is record
		--
		-- FIXME w/o snapshots about 265 bytes,
		--       snapshots about 68 bytes each, all in all 3529 bytes
		--

		--
		-- (At the moment we just check the active snapshots of
		--  the active super-block but should it not make sense
		--  to iterate overall super-blocks when trying to determine
		--  if a block may be safely freed? Because if the most
		--  recent SB is corrupted and we try to use an older one,
		--  chances are that the snapshot in the corrupt SB has
		--  reused blocks reference by a snapshot in the older SB.)
		--

		Keys                    : Keys_Type;
		Snapshots               : Snapshots_Type;
		Last_Secured_Generation : Generation_Type;
		Snapshot_ID             : Snapshot_ID_Type;
		Degree                  : Tree_Degree_Type;
		Free_Gen                : Generation_Type;
		Free_Number             : Physical_Block_Address_Type;
		Free_Hash               : Hash_Type;
		Free_Height             : Tree_Level_Type;
		Free_Degree             : Tree_Degree_Type;
		Free_Leafs              : Tree_Number_Of_Leafs_Type;
		Padding                 : Super_Block_Padding_Type;
	end record with Size =>
		 2 * 68 * 8 + -- Keys
		48 * 72 * 8 + -- Snapshots
		      8 * 8 + -- Last_Secured_Generation
		      4 * 8 + -- Snapshot_ID
		      4 * 8 + -- Degree
		      8 * 8 + -- Free_Gen
		      8 * 8 + -- Free_Number
		     32 * 8 + -- Free_Hash
		      4 * 8 + -- Free_Height
		      4 * 8 + -- Free_Degree
		      8 * 8 + -- Free_Leafs
		    424 * 8;  -- Padding

	pragma Assert (Super_Block_Type'Size = Block_Data_Type'Size);

	type Super_Blocks_Index_Type is range 0 .. 7;
	type Super_Blocks_Type is array (Super_Blocks_Index_Type) of Super_Block_Type;

	type Timeout_Request_Type is record
		Valid   : Boolean;
		Timeout : Timestamp_Type;
	end record;

	type Object_Type is private;

	procedure Initialize_Object (
		Obj     : out Object_Type;
		Now     :     Timestamp_Type;
		Sync    :     Timestamp_Type;
		Secure  :     Timestamp_Type;
		SBs     :     Super_Blocks_Type;
		Curr_SB :     Super_Blocks_Index_Type);

	function Peek_Sync_Timeout_Request (Obj : Object_Type)
	return Timeout_Request_Type;

	function Peek_Secure_Timeout_Request (Obj : Object_Type)
	return Timeout_Request_Type;

	procedure Ack_Sync_Timeout_Request (Obj : in out Object_Type);

	procedure Ack_Secure_Timeout_Request (Obj : in out Object_Type);

	--
	-- Return a request for the backend block session
	--
	-- \param Req  return valid request in case the is one pending that
	--             needs data, otherwise an invalid one is returned
	--
	procedure Need_Data (
		Obj : in out Object_Type;
		Req :    out Request.Object_Type);

	--
	-- Request access to the Block::Request data for reading data
	--
	-- \param  Request  reference to the Block::Request processed
	--                  by the CBE
	-- \param  Data     reference to the data associated with the
	--                  Block::Request
	--
	-- \return  true if the CBE could process the request
	--
	function Give_Write_Data (
		Obj  : in out Object_Type;
		Now  :        Timestamp_Type;
		Req  :        Request.Object_Type;
		Data :        Block_Data_Type)
	return Boolean;

private

	type Free_Tree_Retry_Count_Type is mod 2**32;

	--
	-- Defining the structure here is just an interims solution
	-- and should be properly managed, especially handling more
	-- than one request is "missing".
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
		Sync_Interval           : Timestamp_Type;
		Last_Time               : Timestamp_Type;
		Secure_Interval         : Timestamp_Type;
		Last_Secure_Time        : Timestamp_Type;
		Sync_Timeout_Request    : Timeout_Request_Type;
		Secure_Timeout_Request  : Timeout_Request_Type;
		Execute_Progress        : Boolean;
		Request_Pool_Obj        : Pool.Object_Type;
		Splitter_Obj            : Splitter.Object_Type;
		Crypto_Obj              : Crypto.Object_Type;
		Crypto_Data             : Block_Data_Type;
		Io_Obj                  : Block_Io.Object_Type;
		Io_Data                 : Block_Io.Data_Type;
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
		Super_Blocks            : Super_Blocks_Type;
		Cur_SB                  : Superblock_Index_Type;
		Cur_Gen                 : Generation_Type;
		Last_Secured_Generation : Generation_Type;
		Cur_Snap                : Snapshot_ID_Type;
		Last_Snapshot_ID        : Snapshot_ID_Type;
		Seal_Generation         : Boolean;
		Secure_Superblock       : Boolean;
		Superblock_Dirty        : Boolean;
		Front_End_Req_Prim      : Request_Primitive_Type;
		Back_End_Req_Prim       : Request_Primitive_Type;
	end record;

	function Super_Block_Snapshot_Slot (SB : Super_Block_Type)
	return Snapshot_ID_Type;

	function Discard_Snapshot (Active_Snaps : in out Snapshots_Type;
	                           Curr_Snap_ID :        Snapshot_ID_Type)
	return Boolean;

	function Timeout_Request_Valid (Time : Timestamp_Type)
	return Timeout_Request_Type;

	function Timeout_Request_Invalid
	return Timeout_Request_Type;

	function Max_VBA (Obj : Object_Type)
	return Virtual_Block_Address_Type;

end CBE.Library;
