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
	function Object_Size (Obj : Library.Object_Type)
	return CXX_Object_Size_Type
	is (Obj'Size / 8);


	procedure Initialize_Object (
		Obj     : out Library.Object_Type;
		SBs     :     Super_Blocks_Type;
		Curr_SB :     Super_Blocks_Index_Type)
	is
	begin
		Library.Initialize_Object (Obj, SBs, Curr_SB);
	end Initialize_Object;


	function Cache_Dirty (Obj : Library.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK (Library.Cache_Dirty (Obj)));


	function Superblock_Dirty (Obj : Library.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK (Library.Superblock_Dirty (Obj)));


	function Is_Securing_Superblock (Obj : Library.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK (Library.Is_Securing_Superblock (Obj)));


	function Is_Sealing_Generation (Obj : Library.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK (Library.Is_Sealing_Generation (Obj)));


	procedure Start_Securing_Superblock (Obj : in out Library.Object_Type)
	is
	begin
		Library.Start_Securing_Superblock (Obj);
	end Start_Securing_Superblock;


	procedure Start_Sealing_Generation (Obj : in out Library.Object_Type)
	is
	begin
		Library.Start_Sealing_Generation (Obj);
	end Start_Sealing_Generation;


	function Max_VBA (Obj : Library.Object_Type)
	return Virtual_Block_Address_Type
	is
	begin
		return Library.Max_VBA (Obj);
	end Max_VBA;


	procedure Execute (
		Obj : in out Library.Object_Type;
		Now :        Timestamp_Type)
	is
	begin
		Library.Execute (Obj, Now);
	end Execute;


	function Request_Acceptable (Obj : Library.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_SPARK (Library.Request_Acceptable (Obj)));


	procedure Submit_Request (
		Obj : in out Library.Object_Type;
		Req :        CXX_Request_Type)
	is
	begin
		Library.Submit_Request (Obj, CXX_Request_To_SPARK (Req));
	end Submit_Request;


	function Peek_Completed_Request (Obj : Library.Object_Type)
	return CXX_Request_Type
	is (CXX_Request_From_SPARK (Library.Peek_Completed_Request (Obj)));


	procedure Drop_Completed_Request (
		Obj : in out Library.Object_Type;
		Req :        CXX_Request_Type)
	is
	begin
		Library.Drop_Completed_Request (Obj, CXX_Request_To_SPARK (Req));
	end Drop_Completed_Request;


	procedure Need_Data (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request_Type)
	is
		SPARK_Req : Request.Object_Type;
	begin
		Library.Need_Data (Obj, SPARK_Req);
		Req := CXX_Request_From_SPARK (SPARK_Req);
	end Need_Data;


	procedure Take_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Take_Read_Data (
			Obj, CXX_Request_To_SPARK (Req), SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Take_Read_Data;


	procedure Ack_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Data     :        Block_Data_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Ack_Read_Data (
			Obj, CXX_Request_To_SPARK (Req), Data, SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Ack_Read_Data;


	procedure Take_Write_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Data     :    out Block_Data_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Take_Write_Data (
			Obj, CXX_Request_To_SPARK (Req), Data, SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Take_Write_Data;


	procedure Ack_Write_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Ack_Write_Data (
			Obj, CXX_Request_To_SPARK (Req), SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Ack_Write_Data;


	procedure Have_Data (
		Obj : in out Library.Object_Type;
		Req :    out CXX_Request_Type)
	is
		SPARK_Req : Request.Object_Type;
	begin
		Library.Have_Data (Obj, SPARK_Req);
		Req := CXX_Request_From_SPARK (SPARK_Req);
	end Have_Data;


	function Give_Data_Index (
		Obj : Library.Object_Type;
		Req : CXX_Request_Type)
	return CXX_Primitive_Index_Type
	is
	begin
		return CXX_Primitive_Index_Type (
			Library.Give_Data_Index (Obj, CXX_Request_To_SPARK (Req)));
	end Give_Data_Index;


	procedure Give_Read_Data (
		Obj      : in out Library.Object_Type;
		Req      :        CXX_Request_Type;
		Data     :    out Crypto.Plain_Data_Type;
		Progress :    out CXX_Bool_Type)
	is
		SPARK_Progress : Boolean;
	begin
		Library.Give_Read_Data (
			Obj, CXX_Request_To_SPARK (Req), Data, SPARK_Progress);
		Progress := CXX_Bool_From_SPARK (SPARK_Progress);
	end Give_Read_Data;


	function Give_Write_Data (
		Obj     : in out Library.Object_Type;
		Now     :        Timestamp_Type;
		Req     :        CXX_Request_Type;
		Data    :        Block_Data_Type)
	return CXX_Bool_Type
	is
	begin
		return CXX_Bool_From_SPARK (
			Library.Give_Write_Data (
				Obj, Now, CXX_Request_To_SPARK (Req), Data));
	end Give_Write_Data;


	function Execute_Progress (Obj : Library.Object_Type)
	return CXX_Bool_Type
	is (CXX_Bool_From_Spark (Library.Execute_Progress (Obj)));

end CBE.CXX.CXX_Library;
