--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package CBE
with SPARK_Mode
is
   pragma Pure;

   type U8  is mod 2**8;
   type U16 is mod 2**16;
   type U32 is mod 2**32;
   type U64 is mod 2**64;

   procedure print_u8  (U : U8)
   with Import, Convention => C, External_Name => "print_u8";

   procedure print_u16 (U : U16)
   with Import, Convention => C, External_Name => "print_u16";

   procedure print_u32 (U : U32)
   with Import, Convention => C, External_Name => "print_u32";

   procedure print_u64 (U : U64)
   with Import, Convention => C, External_Name => "print_u64";

   procedure Print_Cstring (S : String; Length : U64)
      with Import, Convention => C, External_Name => "print_cstring";

   procedure Print_String (S : String);

   type Byte_Type is range 0 .. 255 with Size => 8;

   Superblock_Nr_Of_Snapshots : constant := 48;
   Block_Size : constant := 4096;

   type Block_Data_Type
   is array (0 .. Block_Size - 1) of Byte_Type with Size => Block_Size * 8;

   type Translation_Data_Type
   is array (0 .. 0) of Block_Data_Type with Size => 1 * Block_Size * 8;

   type Number_Of_Primitives_Type    is mod 2**64;
   type Index_Type                   is mod 2**64;
   type Generation_Type              is mod 2**64;
   type Block_Number_Type            is mod 2**64;
   type Physical_Block_Address_Type  is mod 2**64;
   type Virtual_Block_Address_Type   is mod 2**64;
   type Timestamp_Type               is mod 2**64;
   type Tree_Level_Index_Type        is range 0 .. 5;
   type Tree_Number_Of_Leafs_Type    is mod 2**64;
   type Tree_Degree_Type             is mod 2**32;
   type Tree_Degree_Mask_Type        is mod 2**32;
   type Tree_Degree_Log_2_Type       is mod 2**32;
   type Tree_Level_Type              is mod 2**32;
   type Tree_Child_Index_Type        is mod 2**32;
   type Tag_Type                     is mod 2**32;
   type Number_Of_Blocks_Type        is mod 2**32;
   type Snapshot_ID_Type             is range 0 .. 2**32 - 1;
   type Snapshot_ID_Storage_Type     is range 0 .. 2**32 - 1 with Size => 32;
   type Snapshot_Valid_Storage_Type  is range 0 .. 2**8 - 1 with Size => 8;
   type Snapshot_Flags_Type          is mod 2**32;
   type Key_ID_Type                  is range 0 .. 2**32 - 1;
   type Key_ID_Storage_Type          is range 0 .. 2**32 - 1 with Size => 32;
   type Operation_Type               is (Read, Write, Sync);

   type Hash_Type is array (1 .. 32) of Byte_Type with Size => 32 * 8;

   type Type_I_Node_Padding_Type is array (0 .. 15) of Byte_Type;

   type Type_I_Node_Type is record
      PBA      : Physical_Block_Address_Type;
      Gen      : Generation_Type;
      Hash     : Hash_Type;
      Padding  : Type_I_Node_Padding_Type;
   end record with Size =>
       8 * 8 + --  PBA
       8 * 8 + --  Gen
      32 * 8 + --  Hash
      16 * 8;  --  Padding

   type Type_I_Node_Block_Type is
      array (0 .. (Block_Data_Type'Size / Type_I_Node_Type'Size) - 1)
      of Type_I_Node_Type;

   pragma Pack (Type_I_Node_Block_Type);

   type Type_1_Node_Info_Type is record
      PBA  : Physical_Block_Address_Type;
      Gen  : Generation_Type;
      Hash : Hash_Type;
   end record;

   type Type_II_Node_Padding_Type is array (0 .. 26) of Byte_Type;

   --
   --  The CBE::Type_i_node contains the on-disk type 2 inner node
   --  information. This node is only used in the free-tree at the
   --  level directly above the leaf nodes.
   --
   type Type_II_Node_Type is record
      PBA         : Physical_Block_Address_Type;
      Last_VBA    : Virtual_Block_Address_Type;
      Alloc_Gen   : Generation_Type;
      Free_Gen    : Generation_Type;
      Last_Key_ID : Key_ID_Storage_Type;
      Reserved    : Boolean;
      Padding     : Type_II_Node_Padding_Type;
   end record with Size =>
       8 * 8 + --  PBA
       8 * 8 + --  Last_VBA
       8 * 8 + --  Alloc_Gen
       8 * 8 + --  Free_Gen
       4 * 8 + --  Last_Key_Id
       1 * 8 + --  Reserved
      27 * 8;  --  Padding

   type Type_II_Node_Block_Type is
      array (0 .. (Block_Data_Type'Size / Type_II_Node_Type'Size) - 1)
      of Type_II_Node_Type;

   pragma Pack (Type_II_Node_Block_Type);

   type Query_Data_Type
   is array (0 .. 0) of Block_Data_Type with Size => 1 * Block_Size * 8;

   --
   --  The CBE::Snapshot stores the information about a given tree within
   --  the CBE.
   --
   type Snapshot_Type is record
      Hash        : Hash_Type;
      PBA         : Physical_Block_Address_Type;
      Gen         : Generation_Type;
      Nr_Of_Leafs : Tree_Number_Of_Leafs_Type;
      Height      : Tree_Level_Type;
      Valid       : Snapshot_Valid_Storage_Type;
      ID          : Snapshot_ID_Storage_Type;
      Flags       : Snapshot_Flags_Type;
   end record;

   for Snapshot_Type use record
      Hash        at 0  range 0 .. 32 * 8 - 1;
      PBA         at 32 range 0 ..  8 * 8 - 1;
      Gen         at 40 range 0 ..  8 * 8 - 1;
      Nr_Of_Leafs at 48 range 0 ..  8 * 8 - 1;
      Height      at 56 range 0 ..  4 * 8 - 1;
      Valid       at 60 range 0 ..  1 * 8 - 1;
      ID          at 64 range 0 ..  4 * 8 - 1;
      Flags       at 68 range 0 ..  4 * 8 - 1;
   end record;

   for Snapshot_Type'Size use
      32 * 8 + --  Hash
       8 * 8 + --  PBA
       8 * 8 + --  Gen
       8 * 8 + --  Nr_Of_Leafs
       4 * 8 + --  Height
       1 * 8 + --  Valid
       3 * 8 + --  <Padding>
       4 * 8 + --  ID
       4 * 8;  --  Flags

   function Tree_Min_Degree
   return Tree_Degree_Type
   is (1);

   function Tree_Min_Height
   return Tree_Level_Type
   is (1);

   function Tree_Max_Height
   return Tree_Level_Type
   is (Tree_Level_Type (Tree_Level_Index_Type'Last) + 1);

   function Snapshot_Valid (Snap : Snapshot_Type)
   return Boolean;

   procedure Snapshot_Valid (
      Snap  : in out Snapshot_Type;
      Valid :        Boolean);

   function Snapshot_Flags_Keep_Mask
   return Snapshot_Flags_Type
   is (1);

   function Snapshot_Keep (Snap : Snapshot_Type)
   return Boolean
   is ((Snap.Flags and Snapshot_Flags_Keep_Mask) /= 0);

   type Snapshots_Index_Type is range 0 .. Superblock_Nr_Of_Snapshots - 1;
   type Snapshots_Index_Storage_Type is range 0 .. 2**32 - 1 with Size => 32;
   type Snapshots_Type
   is array (Snapshots_Index_Type) of Snapshot_Type
   with Size => Superblock_Nr_Of_Snapshots * 72 * 8;

   type Type_1_Node_Infos_Type
   is array (0 .. Natural (Tree_Level_Index_Type'Last))
      of Type_1_Node_Info_Type;

   function Type_1_Node_Info_Invalid
   return Type_1_Node_Info_Type
   is (
      PBA  => 0,
      Gen  => 0,
      Hash => (others => 0));

   function Index_Invalid return Index_Type is (Index_Type'Last);
   function VBA_Invalid return Virtual_Block_Address_Type
   is (Virtual_Block_Address_Type'Last);

   function PBA_Invalid return Physical_Block_Address_Type
   is (Physical_Block_Address_Type'Last);

   function Tree_Level_Invalid return Tree_Level_Type
   is (Tree_Level_Type'Last);

   --
   --  Tag meanings
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

   type Key_Value_Index_Type is range 0 .. 63;
   type Key_Value_Type
   is array (Key_Value_Index_Type) of Byte_Type with Size => 64 * 8;

   --
   --  The CBE::Key contains the key-material that is used to
   --  process cipher-blocks.
   --
   --  (For now it is not used but the ID field is already referenced
   --  by type 2 nodes.)
   --
   type Key_Type is record
      Value : Key_Value_Type;
      ID    : Key_ID_Storage_Type;
   end record with Size =>
      64 * 8 + --  Value
       4 * 8;  --  ID

   type Keys_Index_Type is range 0 .. 1;
   type Keys_Type is array (Keys_Index_Type) of Key_Type;
   type Superblock_Padding_Type is array (0 .. 423) of Byte_Type;

   --
   --  The CBE::Superblock contains all information of a CBE
   --  instance including the list of active snapshots. For now
   --  the super-blocks are stored consecutively at the beginning
   --  of the block device, i.e., there is a 1:1 mapping between
   --  the physical-block-address and the SB id.
   --
   --  Per super-block we have a fixed number of snapshots (about
   --  the amount we can store within one disk sector). Whenever
   --  a generation is sealed, a new snapshot will be created
   --  automatically. If a snapshot is flagged as KEEP, it will never
   --  be overriden.
   --
   type Superblock_Type is record
      --
      --  FIXME w/o snapshots about 265 bytes,
      --       snapshots about 68 bytes each, all in all 3529 bytes
      --

      --
      --  (At the moment we just check the active snapshots of
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
      Curr_Snap               : Snapshots_Index_Storage_Type;
      Degree                  : Tree_Degree_Type;
      Free_Gen                : Generation_Type;
      Free_Number             : Physical_Block_Address_Type;
      Free_Hash               : Hash_Type;
      Free_Height             : Tree_Level_Type;
      Free_Degree             : Tree_Degree_Type;
      Free_Leafs              : Tree_Number_Of_Leafs_Type;
      Padding                 : Superblock_Padding_Type;
   end record with Size =>
                               2 * 68 * 8 + --  Keys
      Superblock_Nr_Of_Snapshots * 72 * 8 + --  Snapshots
                                    8 * 8 + --  Last_Secured_Generation
                                    4 * 8 + --  Curr_Snap
                                    4 * 8 + --  Degree
                                    8 * 8 + --  Free_Gen
                                    8 * 8 + --  Free_Number
                                   32 * 8 + --  Free_Hash
                                    4 * 8 + --  Free_Height
                                    4 * 8 + --  Free_Degree
                                    8 * 8 + --  Free_Leafs
                                  424 * 8;  --  Padding

   pragma Assert (Superblock_Type'Size = Block_Data_Type'Size);

   type Superblocks_Index_Type is range 0 .. 7;
   type Superblocks_Type
   is array (Superblocks_Index_Type) of Superblock_Type
   with Size => 8 * Block_Size * 8;

   type Timeout_Request_Type is record
      Valid   : Boolean;
      Timeout : Timestamp_Type;
   end record;

   function To_String (N : U64)                         return String;
   function To_String (B : Boolean)                     return String;
   function To_String (T : Tag_Type)                    return String;
   function To_String (A : Physical_Block_Address_Type) return String;
   function To_String (B : Block_Data_Type)             return String;

   function Image (I : U64) return String;

end CBE;
