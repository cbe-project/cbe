--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.CXX.CXX_Primitive;
with CBE.Crypto;

package CBE.CXX.CXX_Crypto
with Spark_Mode
is
	--  Disable for now because of libsparkcrypto
	--  pragma Pure;

	--
	-- Object_Size
	--
	function Object_Size (Obj : Crypto.Object_Type) return CXX_Object_Size_Type
		with Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module11object_sizeERKNS0_6CryptoE";

	--
	-- Initialize_Object
	--
	procedure Initialize_Object(
		Obj       : out Crypto.Object_Type;
		Key       :     Crypto.Key_Type;
		Data_Size :     CXX_Size_Type;
		Prim_Size :     CXX_Size_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module6CryptoC1EPKcmm";

	--
	-- Primitive_Acceptable
	--
	function Primitive_Acceptable(Obj : Crypto.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module6Crypto20primitive_acceptableEv";

	--
	-- Submit_Primitive
	--
	procedure Submit_Primitive(
		Obj         : in out Crypto.Object_Type;
		Prim        :        CXX_Primitive.Object_Type;
		Plain_Data  :        Crypto.Plain_Data_Type;
		Cipher_Data :        Crypto.Cipher_Data_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module6Crypto16submit_primitiveERKNS_9PrimitiveERNS_10Block_dataES6_";

	--
	-- Execute
	--
	procedure Execute(Obj  : in out Crypto.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module6Crypto7executeEv";

	--
	-- Peek_Generated_Primitive
	--
	function Peek_Generated_Primitive(Obj : Crypto.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module6Crypto24peek_generated_primitiveEv";

	--
	-- Drop_Generated_Primitive
	--
	procedure Drop_Generated_Primitive(
		Obj  : in out Crypto.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module6Crypto24drop_generated_primitiveERKNS_9PrimitiveE";

	--
	-- Peek_Completed_Primitive
	--
	function Peek_Completed_Primitive(Obj : Crypto.Object_Type)
	return CXX_Primitive.Object_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZNK3Cbe6Module6Crypto24peek_completed_primitiveEv";

	--
	-- Drop_Completed_Primitive
	--
	procedure Drop_Completed_Primitive(
		Obj  : in out Crypto.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module6Crypto24drop_completed_primitiveERKNS_9PrimitiveE";

	--
	-- Mark_Completed_Primitive
	--
	procedure Mark_Completed_Primitive(
		Obj  : in out Crypto.Object_Type;
		Prim :        CXX_Primitive.Object_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module6Crypto24mark_completed_primitiveERKNS_9PrimitiveE";

	--
	-- Mark_Completed_Primitive
	--
	procedure Copy_Completed_Data(
		Obj        : in out Crypto.Object_Type;
		Prim       :        CXX_Primitive.Object_Type;
		Plain_Data : in out Crypto.Plain_Data_Type)
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module6Crypto19copy_completed_dataERKNS_9PrimitiveERNS_10Block_dataE";


	---------------
	-- Accessors --
	---------------

	function Execute_Progress(Obj : Crypto.Object_Type)
	return CXX_Bool_Type
	with
		Export,
		Convention    => C,
		External_Name => "_ZN3Cbe6Module6Crypto16execute_progressEv";

end CBE.CXX.CXX_Crypto;
