--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.Cache;
with CBE.Tree_Helper;
with CBE.Translation;

--
-- The Free_tree meta-module handles the allocation and freeing, i.e.,
-- reservation, of nodes. It is vital to implement the CoW semantics.
--
-- (It currently _does not_ feature CoW semanitcs as it will only
--  exchange entries in the leaf nodes.)
--
package CBE.Free_Tree
with Spark_Mode
is
	pragma Pure;

	type IO_Entry_State_Type is (Invalid, Pending, In_Progress, Complete);

	--
	-- The Io_entry provides a stateful I/O operation abstraction
	--
	type IO_Entry_Type is record
		PBA         : Physical_Block_Address_Type;
		State       : IO_Entry_State_Type;
		Index       : Cache.Cache_Index_Type;
	end record;

	type Object_Type is private;
	type Free_PBAs_Type is array (Tree_Level_Index_Type) of Physical_Block_Address_Type;
	type Query_Branches_Index_Type is range 0 .. 7;
	type WB_IO_Entries_Index_Type is range 0 .. (Natural ((Tree_Level_Index_Type'Last + 1)) * Natural ((Query_Branches_Index_Type'Last + 1))) - 1;
	type WB_IO_Entries_Type is array (WB_IO_Entries_Index_Type) of IO_Entry_Type;
	type WB_Data_New_PBAs_Type is array (Tree_Level_Index_Type) of Physical_Block_Address_Type;

	--
	-- Constructor
	--
	-- \param  root      physical-block-address of the root node
	-- \param  root_gen  generation of the root node
	-- \param  hash      hash of the root node
	-- \param  height    height of the free-tree
	-- \param  dregree   number of edges of a node
	-- \param  leafs     total number of leafs in the tree
	--
	procedure Initialize_Object (
		Obj     : out Object_Type;
		Rt_PBA  :     Physical_Block_Address_Type;
		Rt_Gen  :     Generation_Type;
		Rt_Hash :     Hash_Type;
		Hght    :     Tree_Level_Type;
		Degr    :     Tree_Degree_Type;
		Lfs     :     Tree_Number_Of_Leafs_Type);

	----------------------
	-- Module interface --
	----------------------

	--
	-- Instruct the free-tree module to retry the current pending request
	--
	-- This method relies on the requests information being available, i.e.,
	-- _do not_ call 'drop_completed_primitive' if one intends to retry the
	-- allocation after discarding a snapshot (in case the former allocation
	-- did not succeed the module will emit a failed completed primitive.
	--
	procedure Retry_Allocation (Obj : in out Object_Type);

	function Request_Acceptable (Obj : Object_Type)
	return Boolean;

	--
	-- FIXME refer to tree_height for number of valid elements
	--
	procedure Submit_Request (
		Obj             : in out Object_Type;
		Curr_Gen        :        Generation_Type;
		Nr_of_Blks      :        Number_Of_Blocks_Type;
		New_PBAs        :        WB_Data_New_PBAs_Type;
		Old_PBAs        :        Type_1_Node_Infos_Type;
		Tree_Height     :        Tree_Level_Type;
		Fr_PBAs         :        Free_PBAs_Type;
		-- Nr_Of_Free_Blks :        Number_Of_Blocks_Type;
		Req_Prim        :        Primitive.Object_Type;
		VBA             :        Virtual_Block_Address_Type);

	--
	-- Execute module
	--
	-- Since the FT module incorporates a number of other modules or
	-- needs access to the data ofther modules, we pass them in from
	-- the outside to limit the copying of data.
	--
	-- \param  active        list of currently active snapshots
	-- \param  last_secured  last secured generation
	-- \param  trans_data    reference to data used by the Translation module
	-- \param  cache         reference to the cache module
	-- \param  cache_data    reference to the data used by the Cache module
	-- \param  query_data    reference to the dat used for the processing
	--                       of querying a branch in the FT
	-- \param  time          reference to the time object
	--
	-- \return true if some progress was made, otherwise false
	--
	procedure Execute (
		Obj              : in out Object_Type;
		Active_Snaps     :        Snapshots_Type;
		Last_Secured_Gen :        Generation_Type;
		Trans_Data       : in out Translation_Data_Type;
		Cach             : in out Cache.Object_Type;
		Cach_Data        : in out Cache.Cache_Data_Type;
		Query_Data       :        Query_Data_Type;
		Timestamp        :        Timestamp_Type);

private

	--
	-- FIXME account for n + m blocks
	-- FIXME the number of pba depends on degree which for now
	--       is dynamically set. Since 64 edges are the eventually
	--       used ones, hardcode that.
	--
	type Query_Branch_PBAs_Type is array (0 .. 63) of Physical_Block_Address_Type;
	type Found_PBAs_Type is array (0 .. ((Tree_Level_Index_Type'Last + 1) * 2) - 1) of Physical_Block_Address_Type;

	type Query_Branch_Type is record
		Trans_Infos       : Type_1_Node_Infos_Type;
		PBAs              : Query_Branch_PBAs_Type;
		Nr_Of_Free_Blocks : Number_Of_Blocks_Type;
		VBA               : Virtual_Block_Address_Type;
	end record;

	type Query_Branches_Type is array (Query_Branches_Index_Type) of Query_Branch_Type;

	type Write_Back_Data_Type is record
		Prim        : Primitive.Object_Type;
		Gen         : Generation_Type;
		VBA         : Virtual_Block_Address_Type;
		Tree_Height : Tree_Level_Type;
		New_PBAs    : WB_Data_New_PBAs_Type;
		Old_PBAs    : Type_1_Node_Infos_Type;
		Finished    : Boolean;
	end record;

	type Object_Type is record

		Trans_Helper : Tree_Helper.Object_Type;

		--
		-- The Translation module is used to traverse
		-- the FT by looking for (re-)usable leaf nodes
		-- in each branch of the tree.
		--
		Trans : Translation.Object_Type;

		Execute_Progress : Boolean;

		--
		-- Internal states of the module
		--
		-- FIXME Should be replaced by a proper State type.
		--
		Do_Update : Boolean;
		Do_WB     : Boolean;
		WB_Done   : Boolean;

		Nr_Of_Blocks       : Number_Of_Blocks_Type;
		Nr_Of_Found_Blocks : Number_Of_Blocks_Type;
		Query_Branches     : Query_Branches_Type;
		Curr_Query_Branch  : Query_Branches_Index_Type;
		Found_PBAs         : Found_PBAs_Type;
		Free_PBAs          : Free_PBAs_Type;
		Root_PBA           : Physical_Block_Address_Type;
		Root_Hash          : Hash_Type;
		Root_Gen           : Generation_Type;
		Curr_Query_Prim    : Primitive.Object_Type;

		--
		-- There is always only one type 2 node query pending, branch
		-- by branch.
		--
		-- (Eventually it might makes sense to query all disjunct branches
		--  concurrently.)
		--
		Curr_Type_2 : IO_Entry_Type;

		--
		-- As we might need to write multiple branches back to the disk
		-- make sure we have enough entries available.
		--
		-- (Maybe it makes sense to reorganize all these array in array
		--  structures in a better way.)
		--
		WB_IOs  : WB_IO_Entries_Type;
		WB_Data : Write_Back_Data_Type;

	end record;

	--
	-- Helper method that will reset the internal state
	--
	-- (It currently resets all already found free blocks,
	--  it would be better to merge them with any newly
	--  found ones.)
	--
	procedure Reset_Query_Prim (Obj : in out Object_Type);

	function Query_Branch_Invalid return Query_Branch_Type
	is (
		Trans_Infos       => (others => Type_1_Node_Info_Invalid),
		PBAs              => (others => PBA_Invalid),
		Nr_Of_Free_Blocks => 0,
		VBA               => VBA_Invalid);

	function IO_Entry_Invalid return IO_Entry_Type
	is (
		PBA   => PBA_Invalid,
		State => Invalid,
		Index => 0);

	function Write_Back_Data_Invalid return Write_Back_Data_Type
	is (
		Prim        => Primitive.Invalid_Object,
		Gen         => 0,
		VBA         => VBA_Invalid,
		Tree_Height => 0,
		New_PBAs    => (others => PBA_Invalid),
		Old_PBAs    => (others => Type_1_Node_Info_Invalid),
		Finished    => False);

	--
	-- Check if the entry is usable for allocation
	--
	-- A node is useable either if it is currently not reserved,
	-- or if it is reserved by the generation it was freed in is
	-- not covered by any active snapshot, i.e., it was already
	-- free before any snapshot was created and is therefor not
	-- in use anymore.
	--
	-- \param  active        list of current active snapshots
	-- \param  last_secured  last secured generation
	-- \param  node          reference to the to be checked entry
	--
	-- \return true if the entry is useable, false otherwise
	--
	function Leaf_Usable (
		Active_Snaps     : Snapshots_Type;
		Last_Secured_Gen : Generation_Type;
		Node             : Type_II_Node_Type)
	return Boolean;

end CBE.Free_Tree;
