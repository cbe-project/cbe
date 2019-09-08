--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with SHA256_4K;

package CBE
with Spark_Mode
is
	pragma Pure;

	type U8  is mod 2**8;
	type U16 is mod 2**16;
	type U32 is mod 2**32;
	type U64 is mod 2**64;

	procedure print_u8  (U : U8)  with Import, Convention => C, External_Name => "print_u8";
	procedure print_u16 (U : U16) with Import, Convention => C, External_Name => "print_u16";
	procedure print_u32 (U : U32) with Import, Convention => C, External_Name => "print_u32";
	procedure print_u64 (U : U64) with Import, Convention => C, External_Name => "print_u64";

	subtype Byte_Type                 is SHA256_4K.Byte;
	type Block_Data_Type              is array (1..4096) of Byte_Type with Size => 4096 * 8;
	type Translation_Data_Type        is array (0..0) of Block_Data_Type with Size => 1 * 4096 * 8;
	type Number_Of_Primitives_Type    is mod 2**64;
	type Index_Type                   is mod 2**64;
	type Generation_Type              is mod 2**64 with Size => 64;
	type Superblock_Index_Type        is mod 2**64;
	type Physical_Block_Address_Type  is mod 2**64 with Size => 64;
	type Virtual_Block_Address_Type   is mod 2**64;
	type Timestamp_Type               is mod 2**64;
	type Tree_Level_Index_Type        is range 0..5;
	type Tree_Number_Of_Leafs_Type    is mod 2**64 with Size => 64;
	type Tree_Degree_Type             is mod 2**32 with Size => 32;
	type Tree_Degree_Mask_Type        is mod 2**32;
	type Tree_Degree_Log_2_Type       is mod 2**32;
	type Tree_Level_Type              is mod 2**32 with Size => 32;
	type Tree_Child_Index_Type        is mod 2**32;
	type Tag_Type                     is mod 2**32;
	type Number_Of_Blocks_Type        is mod 2**32;
	type Snapshot_ID_Type             is mod 2**32;
	type Snapshot_Flags_Type          is mod 2**32;
	type Key_ID_Type                  is mod 2**32 with Size => 32;

	-- FIXME should be architecture-dependent
	type Address_Type is mod 2**64;

	type Hash_Type is array (1..32) of Byte_Type with Size => 32 * 8;

	type Type_I_Node_Padding_Type is array (0..15) of Byte_Type;

	type Type_I_Node_Type is record
		PBA      : Physical_Block_Address_Type;
		Gen      : Generation_Type;
		Hash     : Hash_Type;
		Padding  : Type_I_Node_Padding_Type;
	end record with Size =>
		 8 * 8 + -- PBA
		 8 * 8 + -- Gen
		32 * 8 + -- Hash
		16 * 8;  -- Padding

	type Type_I_Node_Block_Type is
		array (0..(Block_Data_Type'Size / Type_I_Node_Type'Size) - 1)
		of Type_I_Node_Type;

	pragma Pack (Type_I_Node_Block_Type);

	type Type_1_Node_Info_Type is record
		PBA  : Physical_Block_Address_Type;
		Gen  : Generation_Type;
		Hash : Hash_Type;
	end record;

	type Type_II_Node_Padding_Type is array (0..26) of Byte_Type;

	--
	-- The Cbe::Type_i_node contains the on-disk type 2 inner node
	-- information. This node is only used in the free-tree at the
	-- level directly above the leaf nodes.
	--
	type Type_II_Node_Type is record
		PBA         : Physical_Block_Address_Type;
		Last_VBA    : Virtual_Block_Address_Type;
		Alloc_Gen   : Generation_Type;
		Free_Gen    : Generation_Type;
		Last_Key_ID : Key_ID_Type;
		Reserved    : Boolean;
		Padding     : Type_II_Node_Padding_Type;
	end record with Size =>
		 8 * 8 + -- PBA
		 8 * 8 + -- Last_VBA
		 8 * 8 + -- Alloc_Gen
		 8 * 8 + -- Free_Gen
		 4 * 8 + -- Last_Key_Id
		 1 * 8 + -- Reserved
		27 * 8;  -- Padding


	type Type_II_Node_Block_Type is
		array (0..(Block_Data_Type'Size / Type_II_Node_Type'Size) - 1)
		of Type_II_Node_Type;

	pragma Pack (Type_II_Node_Block_Type);

	type Query_Data_Type is array (0..0) of Block_Data_Type with Size => 1 * 4096 * 8;

	--
	-- The Cbe::Snapshot stores the information about a given tree within
	-- the CBE.
	--
	type Snapshot_Type is record
		Hash        : Hash_Type;
		PBA         : Physical_block_address_Type;
		Gen         : Generation_Type;
		Nr_Of_Leafs : Tree_Number_Of_Leafs_Type;
		Height      : Tree_Level_Type;
		ID          : Snapshot_ID_Type;
		Flags       : Snapshot_Flags_Type;
	end record;

	for Snapshot_Type use record
		Hash        at 0  range 0 .. 32 * 8 - 1;
		PBA         at 32 range 0 ..  8 * 8 - 1;
		Gen         at 40 range 0 ..  8 * 8 - 1;
		Nr_Of_Leafs at 48 range 0 ..  8 * 8 - 1;
		Height      at 56 range 0 ..  4 * 8 - 1;
		ID          at 64 range 0 ..  4 * 8 - 1;
		Flags       at 68 range 0 ..  4 * 8 - 1;
	end record;

	for Snapshot_Type'Size use
		32 * 8 + -- Hash
		 8 * 8 + -- PBA
		 8 * 8 + -- Gen
		 8 * 8 + -- Nr_Of_Leafs
		 4 * 8 + -- Height
		 4 * 8 + -- <Padding>
		 4 * 8 + -- ID
		 4 * 8;  -- Flags

	function Tree_Min_Degree
	return Tree_Degree_Type
	is (1);

	function Tree_Min_Height
	return Tree_Level_Type
	is (1);

	function Tree_Max_Height
	return Tree_Level_Type
	is (Tree_Level_Type (Tree_Level_Index_Type'Last) + 1);

	function Snapshot_ID_Invalid
	return Snapshot_ID_Type
	is (Snapshot_ID_Type'Last);

	function Snapshot_Flags_Keep_Mask
	return Snapshot_Flags_Type
	is (1);

	function Snapshot_Valid (Snap : Snapshot_Type)
	return Boolean
	is (Snap.ID /= Snapshot_ID_Invalid);

	function Snapshot_Keep (Snap : Snapshot_Type)
	return Boolean
	is ((Snap.Flags and Snapshot_Flags_Keep_Mask) /= 0);

	procedure Snapshot_Discard (Snap : in out Snapshot_Type);

	type Snapshots_Index_Type is range 0..47;
	type Snapshots_Type is array (Snapshots_Index_Type) of Snapshot_Type with Size => 48 * 72 * 8;

	type Type_1_Node_Infos_Type is array (0..Natural (Tree_Level_Index_Type'Last)) of Type_1_Node_Info_Type;


	function Snapshot_ID_Invalid_Slot
	return Snapshot_ID_Type
	is (Snapshot_ID_Type (Snapshots_Index_Type'Last) + 1);

	function Type_1_Node_Info_Invalid
	return Type_1_Node_Info_Type
	is (
		PBA  => 0,
		Gen  => 0,
		Hash => (others => 0));

	function Index_Invalid return Index_Type is (Index_Type'Last);
	function VBA_Invalid return Virtual_Block_Address_Type is (Virtual_Block_Address_Type'Last);
	function PBA_Invalid return Physical_Block_Address_Type is (Physical_Block_Address_Type'Last);
	function Tree_Level_Invalid return Tree_Level_Type is (Tree_Level_Type'Last);
	function Superblock_Index_Invalid return Superblock_Index_Type is (Superblock_Index_Type'Last);

	--
	-- Tag meanings
	--
	function Tag_Invalid      return Tag_Type is (16#00#);
	function Tag_IO           return Tag_Type is (16#10#);
	function Tag_Translation  return Tag_Type is (16#60#);
	function Tag_Write_Back   return Tag_Type is (16#70#);
	function Tag_Cache        return Tag_Type is (16#20#);
	function Tag_Cache_Flush  return Tag_Type is (16#21#);
	function Tag_Crypto       return Tag_Type is (16#30#);
	function Tag_Decrypt      return Tag_Type is (16#31#);
	function Tag_Encrypt      return Tag_Type is (16#32#);
	function Tag_Sync_SB      return Tag_Type is (16#80#);
	function Tag_VBD          return Tag_Type is (16#100#);
	function Tag_Free_Tree    return Tag_Type is (16#200#);
	function Tag_Free_Tree_IO return Tag_Type is (16#210#);
	function Tag_Free_Tree_WB return Tag_Type is (16#270#);

end CBE;
