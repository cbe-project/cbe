--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Tree_Helper;

package CBE.CXX
with Spark_Mode
is
	pragma Pure;

	type Uint8_Type                      is mod 2**8  with Size => 1 * 8;
	type Uint32_Type                     is mod 2**32 with Size => 4 * 8;
	type Uint64_Type                     is mod 2**64 with Size => 8 * 8;
	type Number_Of_Bytes_Type            is mod 2**64;
	type Number_Of_Bits_Type             is new Natural;
	type CXX_Bool_Type                   is range 0..1 with Size => Uint8_Type'Size;
	type CXX_Object_Size_Type            is new Uint32_Type;
	type CXX_Index_Type                  is new Uint32_Type;
	type CXX_Number_Of_Primitives_Type   is new Uint64_Type;
	type CXX_Generation_Type             is new Uint64_Type;
	type CXX_Superblock_Index_Type       is new Uint64_Type;
	type CXX_Physical_Block_Address_Type is new Uint64_Type;
	type CXX_Virtual_Block_Address_Type  is new Uint64_Type;
	type CXX_Size_Type                   is new Uint64_Type;
	type CXX_Tree_Number_Of_Leafs_Type   is new Uint64_Type;
	type CXX_Timestamp_Type              is new Uint64_Type;
	type CXX_Tree_Level_Type             is new Uint32_Type;
	type CXX_Tree_Degree_Type            is new Uint32_Type;
	type CXX_Tree_Child_Index_Type       is new Uint32_Type;

	type CXX_Tree_Helper_Type is record
		Degree       : CXX_Tree_Degree_Type;
		Height       : CXX_Tree_Level_Type;
		Leafs        : CXX_Tree_Number_Of_Leafs_Type;
		Degree_Log_2 : CXX_Tree_Degree_Type;
		Degree_Mask  : CXX_Tree_Degree_Type;
	end record;

	function CXX_Tree_Helper_Object_Size (Obj : CXX_Tree_Helper_Type)
	return CXX_Object_Size_Type
	is (Obj'Size / 8)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe11object_sizeERKNS_11Tree_helperE";

	function CXX_Tree_Helper_From_SPARK (Input : Tree_Helper.Object_Type)
	return CXX_Tree_Helper_Type
	is (
		Degree       => CXX_Tree_Degree_Type (Tree_Helper.Degree (Input)),
		Height       => CXX_Tree_Level_Type (Tree_Helper.Height (Input)),
		Leafs        => CXX_Tree_Number_Of_Leafs_Type (Tree_Helper.Leafs (Input)),
		Degree_Log_2 => CXX_Tree_Degree_Type (Tree_Helper.Degree_Log_2 (Input)),
		Degree_Mask  => CXX_Tree_Degree_Type (Tree_Helper.Degree_Mask (Input)));

	function CXX_Tree_Helper_To_SPARK (Input : CXX_Tree_Helper_Type)
	return Tree_Helper.Object_Type
	is (
		Tree_Helper.Initialized_Object (
			Tree_Degree_Type (Input.Degree),
			Tree_Level_Type (Input.Height),
			Tree_Number_Of_Leafs_Type (Input.Leafs)));

	type CXX_Type_1_Node_Info_Type is record
		PBA  : CXX_Physical_Block_Address_Type;
		Gen  : CXX_Generation_Type;
		Hash : Hash_Type;
	end record;

	type CXX_Type_1_Node_Infos_Type is
		array (0..5) of CXX_Type_1_Node_Info_Type;

	--
	-- Bits_To_Bytes
	--
	function Number_Of_Bits_To_Bytes(Bits : Number_Of_Bits_Type)
	return Number_Of_Bytes_Type;

	--
	-- CXX_Type_1_Node_Info_From_SPARK
	--
	function CXX_Type_1_Node_Info_From_SPARK(Input : Type_1_Node_Info_Type)
	return CXX_Type_1_Node_Info_Type;

	--
	-- CXX_Type_1_Node_Info_To_SPARK
	--
	function CXX_Type_1_Node_Info_To_SPARK(Input : CXX_Type_1_Node_Info_Type)
	return Type_1_Node_Info_Type;

	--
	-- CXX_Type_1_Node_Infos_From_SPARK
	--
	function CXX_Type_1_Node_Infos_From_SPARK(Input : Type_1_Node_Infos_Type)
	return CXX_Type_1_Node_Infos_Type;

	--
	-- CXX_Type_1_Node_Infos_To_SPARK
	--
	function CXX_Type_1_Node_Infos_To_SPARK(Input : CXX_Type_1_Node_Infos_Type)
	return Type_1_Node_Infos_Type;

	--
	-- CXX_Bool_From_SPARK
	--
	function CXX_Bool_From_SPARK(Value : Boolean)
	return CXX_Bool_Type;

	--
	-- CXX_Bool_To_SPARK
	--
	function CXX_Bool_To_SPARK(Value : CXX_Bool_Type)
	return Boolean;

	--
	-- CXX_Index_From_SPARK
	--
	function CXX_Index_From_SPARK(Value : Index_Type)
	return CXX_Index_Type;

	--
	-- CXX_Physical_Block_Address_From_SPARK
	--
	function CXX_Physical_Block_Address_From_SPARK(
		Value : Physical_Block_Address_Type)
	return CXX_Physical_Block_Address_Type;

	--
	-- CXX_Superblock_Index_From_SPARK
	--
	function CXX_Superblock_Index_From_SPARK(Value : Superblock_Index_Type)
	return CXX_Superblock_Index_Type;

	--
	-- CXX_Generation_From_SPARK
	--
	function CXX_Generation_From_SPARK(Value : Generation_Type)
	return CXX_Generation_Type;

	--
	-- Correct_Object_Size
	--
	function Correct_Object_Size(
		Spark_Sz : Number_Of_Bits_Type;
		CXX_Sz   : CXX_Size_Type)
	return Boolean
	is (Number_Of_Bytes_Type(CXX_Sz) = Number_Of_Bits_To_Bytes(Spark_Sz));

	--
	-- Enough_Storage_Space
	--
	function Enough_Storage_Space(
		Spark_Sz : Number_Of_Bits_Type;
		CXX_Sz   : CXX_Size_Type)
	return Boolean
	is (Number_Of_Bytes_Type(CXX_Sz) >= Number_Of_Bits_To_Bytes(Spark_Sz));

end CBE.CXX;
