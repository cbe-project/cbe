--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.CXX;
with CBE.CXX.CXX_Primitive;
with CBE.Cache;

package CBE.CXX.CXX_Cache
with Spark_Mode
is
	pragma Pure;

	--
	-- Object_Size
	--
	function Object_Size (Obj : Cache.Object_Type) return CXX_Object_Size_Type
		with Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11object_sizeERKNS0_5CacheE";

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(Obj : out Cache.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5CacheC1Ev";

	--
	-- Data_Available
	--
	function Data_Available(
		Obj : Cache.Object_Type;
		Pba : CXX_Physical_Block_Address_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module5Cache14data_availableEy";

	--
	-- Data_Index
	--
	function Data_Index(
		Obj : in out Cache.Object_Type;
		Pba :        CXX_Physical_Block_Address_Type;
		Ts  :        Timestamp_Type)
	return CXX_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5Cache10data_indexEyy";

	--
	-- Invalidate
	--
	procedure Invalidate(
		Obj : in out Cache.Object_Type;
		Pba :        CXX_Physical_Block_Address_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5Cache10invalidateEy";

	--
	-- Request_Acceptable
	--
	function Request_Acceptable(
		Obj : Cache.Object_Type;
		Pba : CXX_Physical_Block_Address_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module5Cache18request_acceptableEy";

	--
	-- Submit_Primitive
	--
	procedure Submit_Request(
		Obj : in out Cache.Object_Type;
		Pba :        CXX_Physical_Block_Address_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5Cache14submit_requestEy";

	--
	-- Fill_Cache
	--
	procedure Fill_Cache(
		Obj      : in out Cache.Object_Type;
		Data     : in out Cache.Cache_Data_Type;
		Job_Data : in     Cache.Cache_Job_Data_Type;
		Time     :        Timestamp_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5Cache10fill_cacheERNS0_10Cache_DataERKNS0_14Cache_Job_DataEy";

	--
	-- Execute_Progress
	--
	function Execute_Progress(Obj : Cache.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module5Cache8progressEv";

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Cache.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5Cache24peek_generated_primitiveEv";

	--
	-- Peek_Generated_Data_Index
	--
	function Peek_Generated_Data_Index(
		Obj  : Cache.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return CXX_Index_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5Cache25peek_generated_data_indexERKNS_9PrimitiveE";

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Cache.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5Cache24drop_generated_primitiveERKNS_9PrimitiveE";

	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Completed_Primitive(
		Obj  : in out Cache.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module5Cache24mark_completed_primitiveERKNS_9PrimitiveE";

end CBE.CXX.CXX_Cache;
