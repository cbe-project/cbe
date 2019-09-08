--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Request;

package body CBE.CXX.CXX_Library
with Spark_Mode
is
	procedure Initialize_Object (
		Obj     : out Library.Object_Type;
		Now     :     Timestamp_Type;
		Sync    :     Timestamp_Type;
		Secure  :     Timestamp_Type;
		SBs     :     Super_Blocks_Type;
		Curr_SB :     Super_Blocks_Index_Type)
	is
	begin
		Library.Initialize_Object (Obj, Now, Sync, Secure, SBs, Curr_SB);
	end Initialize_Object;


	function Peek_Sync_Timeout_Request (Obj : Library.Object_Type)
	return CXX_Timeout_Request_Type
	is (CXX_Timeout_Request_From_SPARK (Library.Peek_Sync_Timeout_Request (Obj)));


	function Peek_Secure_Timeout_Request (Obj : Library.Object_Type)
	return CXX_Timeout_Request_Type
	is (CXX_Timeout_Request_From_SPARK (Library.Peek_Secure_Timeout_Request (Obj)));


	procedure Ack_Sync_Timeout_Request (Obj : in out Library.Object_Type)
	is
	begin
		Library.Ack_Sync_Timeout_Request (Obj);
	end Ack_Sync_Timeout_Request;


	procedure Ack_Secure_Timeout_Request (Obj : in out Library.Object_Type)
	is
	begin
		Library.Ack_Secure_Timeout_Request (Obj);
	end Ack_Secure_Timeout_Request;


	function Max_VBA (Obj : Library.Object_Type)
	return Virtual_Block_Address_Type
	is
	begin
		return Library.Max_VBA (Obj);
	end Max_VBA;


	procedure Execute (
		Obj              : in out Library.Object_Type;
		Now              :        Timestamp_Type)
--		Show_Progress    :        CXX_Bool_Type;
--		Show_If_Progress :        CXX_Bool_Type)
	is
	begin
		Library.Execute (Obj, Now);
	end Execute;


	function Request_Acceptable (Obj : Library.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK (Library.Request_Acceptable (Obj)));


	procedure Submit_Request (
		Obj : in out Library.Object_Type;
		Req :        CXX_Request.Object_Type)
	is
	begin
		Library.Submit_Request (Obj, CXX_Request.To_Spark (Req));
	end Submit_Request;


	function Peek_Completed_Request (Obj : Library.Object_Type)
	return CXX_Request.Object_Type
	is (CXX_Request.From_Spark (Library.Peek_Completed_Request (Obj)));


	procedure Drop_Completed_Request (
		Obj : in out Library.Object_Type;
		Req :        CXX_Request.Object_Type)
	is
	begin
		Library.Drop_Completed_Request (Obj, CXX_Request.To_Spark (Req));
	end Drop_Completed_Request;


	procedure Need_Data (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request.Object_Type)
	is
		SPARK_Req : Request.Object_Type;
	begin
		Library.Need_Data (Obj, SPARK_Req);
		Req := CXX_Request.From_Spark (SPARK_Req);
	end Need_Data;


	procedure Take_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Take_Read_Data (Obj, CXX_Request.To_Spark (Req), SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Take_Read_Data;


	procedure Ack_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Data     :        Block_Data_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Ack_Read_Data (Obj, CXX_Request.To_Spark (Req), Data, SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Ack_Read_Data;


	procedure Take_Write_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Data     :    out Block_Data_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Take_Write_Data (Obj, CXX_Request.To_Spark (Req), Data, SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Take_Write_Data;


	procedure Ack_Write_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Ack_Write_Data (Obj, CXX_Request.To_Spark (Req), SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Ack_Write_Data;


	procedure Have_Data (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request.Object_Type)
	is
		SPARK_Req : Request.Object_Type;
	begin
		Library.Have_Data (Obj, SPARK_Req);
		Req := CXX_Request.From_Spark (SPARK_Req);
	end Have_Data;


	function Give_Data_Index (
		Obj : Library.Object_Type;
		Req : CXX_Request.Object_Type)
	return UInt64_Type
	is
	begin
		return UInt64_Type (Library.Give_Data_Index (Obj, CXX_Request.To_Spark (Req)));
	end Give_Data_Index;


	procedure Give_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request.Object_Type;
		Data     :    out Crypto.Plain_Data_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Give_Read_Data (Obj, CXX_Request.To_Spark (Req), Data, SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Give_Read_Data;


	function Give_Write_Data (
		Obj     : in out Library.Object_Type;
		Now     :        Timestamp_Type;
		Req     :        CXX_Request.Object_Type;
		Data    :        Block_Data_Type)
	return CXX_Bool_Type
	is
	begin
		return CXX_Bool_From_SPARK (Library.Give_Write_Data (Obj, Now, CXX_Request.To_Spark (Req), Data));
	end Give_Write_Data;


	function Execute_Progress(Obj : Library.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_Spark (Library.Execute_Progress (Obj)));

end CBE.CXX.CXX_Library;
