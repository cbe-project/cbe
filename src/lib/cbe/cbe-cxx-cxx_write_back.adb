--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.CXX.CXX_Write_Back
with Spark_Mode
is
	function Object_Size (Obj : Write_Back.Object_Type)
	return CXX_Object_Size_Type
	is (Obj'Size / 8);

	procedure Initialize_Object(Obj : out Write_Back.Object_Type)
	is
	begin
		Write_Back.Initialize_Object(Obj);
	end;

	procedure Update (
		Obj         : in out Write_Back.Object_Type;
		PBA         :        Physical_Block_Address_Type;
		Tree        :        Tree_Helper.Object_Type;
		Data        :        Block_Data_Type;
		Update_Data : in out Block_Data_Type)
	is
	begin
		Write_Back.Update(Obj, PBA, Tree, Data, Update_Data);
	end Update;

	function Primitive_Acceptable(Obj : Write_Back.Object_Type) return CXX_Bool_Type
	is (CXX_Bool_From_SPARK(Write_Back.Primitive_Acceptable(Obj)));

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
	is
	begin
		Write_Back.Submit_Primitive(Obj, CXX_Primitive.To_SPARK(Prim),
		                            Gen, VBA, New_PBAs,
		                            CXX_Type_1_Node_Infos_To_SPARK(Old_PBAs),
		                            N, Data, WB_Data);
	end Submit_Primitive;

	function Peek_Completed_Primitive (Obj : Write_Back.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return CXX_Primitive.From_SPARK(Write_Back.Peek_Completed_Primitive(Obj));
	end Peek_Completed_Primitive;

	function Peek_Completed_Root (
		Obj  : Write_Back.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Physical_Block_Address_Type
	is
	begin
		return Write_Back.Peek_Completed_Root(Obj, CXX_Primitive.To_SPARK(Prim));
	end Peek_Completed_Root;

	procedure Peek_Completed_Root_Hash (
		Obj  : Write_Back.Object_Type;
		Prim : CXX_Primitive.Object_Type;
		Hash : out Hash_Type)
	is
	begin
		Write_Back.Peek_Completed_Root_Hash(Obj, CXX_Primitive.To_SPARK(Prim), Hash);
	end Peek_Completed_Root_Hash;

	procedure Drop_Completed_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Write_Back.Drop_Completed_Primitive(Obj, CXX_Primitive.To_SPARK(Prim));
	end Drop_Completed_Primitive;

	function Peek_Generated_Crypto_Primitive(Obj : Write_Back.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return CXX_Primitive.From_SPARK(Write_Back.Peek_Generated_Crypto_Primitive(Obj));
	end Peek_Generated_Crypto_Primitive;

	function Peek_Generated_Crypto_Data (
		Obj  : Write_Back.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Write_Back.Data_Index_Type
	is
	begin
		return Write_Back.Peek_Generated_Crypto_Data(Obj, CXX_Primitive.To_SPARK(Prim));
	end Peek_Generated_Crypto_Data;

	procedure Drop_Generated_Crypto_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Write_Back.Drop_Generated_Crypto_Primitive(Obj, CXX_Primitive.To_SPARK(Prim));
	end Drop_Generated_Crypto_Primitive;

	procedure Mark_Completed_Crypto_Primitive (
		Obj         : in out Write_Back.Object_Type;
		Prim        :        CXX_Primitive.Object_Type;
		Crypto_Data :        Block_Data_Type)
	is
	begin
		Write_Back.Mark_Completed_Crypto_Primitive(Obj, CXX_Primitive.To_SPARK(Prim), Crypto_Data);
	end Mark_Completed_Crypto_Primitive;

	function Peek_Generated_IO_Primitive (Obj : Write_Back.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return CXX_Primitive.From_SPARK(Write_Back.Peek_Generated_IO_Primitive(Obj));
	end Peek_Generated_IO_Primitive;

	function Peek_Generated_IO_Data (
		Obj  : Write_Back.Object_Type;
		Prim : CXX_Primitive.Object_Type)
	return Write_Back.Data_Index_Type
	is
	begin
		return Write_Back.Peek_Generated_IO_Data(Obj, CXX_Primitive.To_SPARK(Prim));
	end Peek_Generated_IO_Data;

	procedure Drop_Generated_IO_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Write_Back.Drop_Generated_IO_Primitive(Obj, CXX_Primitive.To_SPARK(Prim));
	end Drop_Generated_IO_Primitive;

	procedure Mark_Completed_IO_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Write_Back.Mark_Completed_IO_Primitive(Obj, CXX_Primitive.To_SPARK(Prim));
	end Mark_Completed_IO_Primitive;

	function Peek_Generated_Cache_Primitive (Obj : Write_Back.Object_Type)
	return CXX_Primitive.Object_Type
	is
	begin
		return CXX_Primitive.From_SPARK(Write_Back.Peek_Generated_Cache_Primitive(Obj));
	end Peek_Generated_Cache_Primitive;

	function Peek_Generated_Cache_Update_PBA (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	return Physical_Block_Address_Type
	is
	begin
		return Write_Back.Peek_Generated_Cache_Update_PBA(Obj, CXX_Primitive.To_SPARK(Prim));
	end Peek_Generated_Cache_Update_PBA;

	procedure Drop_Generated_Cache_Primitive (
		Obj  : in out Write_Back.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	is
	begin
		Write_Back.Drop_Generated_Cache_Primitive(Obj, CXX_Primitive.To_SPARK(Prim));
	end Drop_Generated_Cache_Primitive;

end CBE.CXX.CXX_Write_Back;
