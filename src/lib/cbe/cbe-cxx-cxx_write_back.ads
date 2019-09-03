--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Write_Back;
with CBE.CXX.CXX_Primitive;

package CBE.CXX.CXX_Write_Back
with Spark_Mode
is
	function Object_Size (Obj : Write_Back.Object_Type)
	return CXX_Object_Size_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11object_sizeERKNS0_10Write_backE";

	procedure Initialize_Object(Obj : out Write_Back.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_backC1Ev";

	procedure Update (
		Obj         : in out Write_Back.Object_Type;
		PBA         :        Physical_Block_Address_Type;
		Tree        :        Tree_Helper.Object_Type;
		Data        :        Block_Data_Type;
		Update_Data : in out Block_Data_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back6updateEyRKNS_11Tree_helperERKNS_10Block_dataERS5_";

	function Primitive_Acceptable(Obj : Write_Back.Object_Type) return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module10Write_back20primitive_acceptableEv";

	procedure Submit_Primitive (
		Obj                   : in out Write_Back.Object_Type;
		Prim                  :        CXX_Primitive.Object_Type;
		Gen                   :        Generation_Type;
		VBA                   :        Virtual_Block_Address_Type;
		New_PBAs              :        Write_Back.New_PBAs_Type;
		Old_PBAs              :        CXX_Type_1_Node_Infos_Type;
		N                     :        Tree_Level_Index_Type;
		Data                  :        Block_Data_Type;
		WB_Data               :    out Write_Back.Data_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back16submit_primitiveERKNS_9PrimitiveEyyPKyPKNS_16Type_1_node_infoEjRKNS_10Block_dataERNS0_15Write_back_dataE";

	function Peek_Completed_Primitive (Obj : Write_Back.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back24peek_completed_primitiveEv";

	function Peek_Completed_Root (
		Obj  : Write_Back.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Physical_Block_Address_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module10Write_back19peek_completed_rootERKNS_9PrimitiveE";

	procedure Peek_Completed_Root_Hash (
		Obj  : Write_Back.Object_Type;
		Prim : CXX_Primitive.Object_Type;
		Hash : out Hash_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module10Write_back23peek_competed_root_hashERKNS_9PrimitiveERNS_4HashE";

	procedure Drop_Completed_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back24drop_completed_primitiveERKNS_9PrimitiveE";

	function Peek_Generated_Crypto_Primitive(Obj : Write_Back.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module10Write_back31peek_generated_crypto_primitiveEv";

	function Peek_Generated_Crypto_Data (
		Obj  : Write_Back.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Write_Back.Data_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back26peek_generated_crypto_dataERKNS_9PrimitiveE";

	procedure Drop_Generated_Crypto_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back31drop_generated_crypto_primitiveERKNS_9PrimitiveE";

	procedure Mark_Completed_Crypto_Primitive (
		Obj         : in out Write_Back.Object_Type;
		Prim        :        CXX_Primitive.Object_Type;
		Crypto_Data :        Block_Data_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back31mark_completed_crypto_primitiveERKNS_9PrimitiveERKNS_10Block_dataE";

	function Peek_Generated_IO_Primitive (Obj : Write_Back.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module10Write_back27peek_generated_io_primitiveEv";

	function Peek_Generated_IO_Data (
		Obj  : Write_Back.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Write_Back.Data_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back22peek_generated_io_dataERKNS_9PrimitiveE";

	procedure Drop_Generated_IO_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back27drop_generated_io_primitiveERKNS_9PrimitiveE";

	procedure Mark_Completed_IO_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back27mark_completed_io_primitiveERKNS_9PrimitiveE";

	function Peek_Generated_Cache_Primitive (Obj : Write_Back.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module10Write_back30peek_generated_cache_primitiveEv";

	function Peek_Generated_Cache_Update_PBA (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	return Physical_Block_Address_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module10Write_back31peek_generated_cache_update_pbaERKNS_9PrimitiveE";

	procedure Drop_Generated_Cache_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module10Write_back30drop_generated_cache_primitiveERKNS_9PrimitiveE";

end CBE.CXX.CXX_Write_Back;
