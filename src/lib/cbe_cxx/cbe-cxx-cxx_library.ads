--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Library;
with CBE.Crypto;

package CBE.CXX.CXX_Library
with Spark_Mode
is
--	FIXME cannot be pure yet because of CBE.Library
--	pragma Pure;


	function Object_Size (Obj : Library.Object_Type)
	return CXX_Object_Size_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe11object_sizeERKNS_7LibraryE";


	procedure Initialize_Object (
		Obj     : out Library.Object_Type;
		SBs     :     Super_Blocks_Type;
		Curr_SB :     Super_Blocks_Index_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7LibraryC2ERKNS_12Super_blocksENS_17Super_block_indexE";


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
		Obj              : in out Library.Object_Type;
		Now              :        Timestamp_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library7executeEy";


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


	procedure Drop_Completed_Request (
		Obj : in out Library.Object_Type;
		Req :        CXX_Request_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library29drop_completed_client_requestERKNS_7RequestE";


	procedure Io_Data_Required (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library17_io_data_requiredERNS_7RequestE";


	procedure Io_Data_Read_In_Progress (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Progress :    out CXX_Bool_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library25_io_data_read_in_progressERKNS_7RequestERb";


	procedure Supply_Io_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Data     :        Block_Data_Type;
		Progress :    out CXX_Bool_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library15_supply_io_dataERKNS_7RequestERKNS_10Block_dataERb";


	procedure Has_Io_Data_To_Write (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library21_has_io_data_to_writeERNS_7RequestE";


	procedure Obtain_Io_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Data     :    out Block_Data_Type;
		Progress :    out CXX_Bool_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library15_obtain_io_dataERKNS_7RequestERNS_10Block_dataERb";


	procedure Ack_Io_Data_To_Write (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Progress :    out CXX_Bool_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library21_ack_io_data_to_writeERKNS_7RequestERb";


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
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Data     :    out Crypto.Plain_Data_Type;
		Progress :    out CXX_Bool_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library19_obtain_client_dataERKNS_7RequestERNS_10Block_dataERb";


	procedure Client_Data_Required (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library21_client_data_requiredERNS_7RequestE";


	function Supply_Client_Data (
		Obj     : in out Library.Object_Type;
		Now     :        Timestamp_Type;
		Req     :        CXX_Request_Type;
		Data    :        Block_Data_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe7Library19_supply_client_dataEyRKNS_7RequestERKNS_10Block_dataERb";


	function Execute_Progress (Obj : Library.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe7Library16execute_progressEv";

end CBE.CXX.CXX_Library;
