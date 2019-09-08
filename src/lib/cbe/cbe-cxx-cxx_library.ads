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
with CBE.CXX.CXX_Request;

package CBE.CXX.CXX_Library
with Spark_Mode
is
--	FIXME cannot be pure yet because of CBE.Library
--	pragma Pure;

	procedure Initialize_Object (
		Obj     : out Library.Object_Type;
		Now     :     Timestamp_Type;
		Sync    :     Timestamp_Type;
		Secure  :     Timestamp_Type;
		SBs     :     Super_Blocks_Type;
		Curr_SB :     Super_Blocks_Index_Type);


	function Peek_Sync_Timeout_Request (Obj : Library.Object_Type)
	return CXX_Timeout_Request_Type;


	function Peek_Secure_Timeout_Request (Obj : Library.Object_Type)
	return CXX_Timeout_Request_Type;


	procedure Ack_Sync_Timeout_Request (Obj : in out Library.Object_Type);


	procedure Ack_Secure_Timeout_Request (Obj : in out Library.Object_Type);


	function Max_VBA (Obj : Library.Object_Type)
	return Virtual_Block_Address_Type;


	procedure Execute (
		Obj              : in out Library.Object_Type;
		Now              :        Timestamp_Type);
--		Show_Progress    :        CXX_Bool_Type;
--		Show_If_Progress :        CXX_Bool_Type);


	function Request_Acceptable (Obj : Library.Object_Type)
	return CXX_Bool_Type;


	procedure Submit_Request (
		Obj : in out Library.Object_Type;
		Req :        CXX_Request.Object_Type);


	function Peek_Completed_Request (Obj : Library.Object_Type)
	return CXX_Request.Object_Type;


	procedure Drop_Completed_Request (
		Obj : in out Library.Object_Type;
		Req :        CXX_Request.Object_Type);


	procedure Need_Data (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request.Object_Type);


	procedure Take_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Progress :    out CXX_Bool_Type);


	procedure Ack_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Data     :        Block_Data_Type;
		Progress :    out CXX_Bool_Type);


	procedure Take_Write_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Data     :    out Block_Data_Type;
		Progress :    out CXX_Bool_Type);


	procedure Ack_Write_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Progress :    out CXX_Bool_Type);


	procedure Have_Data (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request.Object_Type);


	function Give_Data_Index (
		Obj : Library.Object_Type;
		Req : CXX_Request.Object_Type)
	return UInt64_Type;


	procedure Give_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Data     :    out Crypto.Plain_Data_Type;
		Progress :    out CXX_Bool_Type);


	function Give_Write_Data (
		Obj     : in out Library.Object_Type;
		Now     :        Timestamp_Type;
		Req     :        CXX_Request.Object_Type;
		Data    :        Block_Data_Type)
	return CXX_Bool_Type;


	function Execute_Progress(Obj : Library.Object_Type)
	return CXX_Bool_Type;

end CBE.CXX.CXX_Library;
