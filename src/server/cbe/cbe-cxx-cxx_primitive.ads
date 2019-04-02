--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.CXX.CXX_Request;

package CBE.CXX.CXX_Primitive
with Spark_Mode
is
	pragma Pure;

	--
	-- Object_Type
	--
	type Object_Type is record
		Tag          : UInt32_Type;
		Operation    : CXX_Request.Operation_Type;
		Success      : CXX_Request.Success_Type;
		Block_Number : UInt64_Type;
		Index        : UInt64_Type;
	end record;
	pragma Pack(Object_Type);

	--
	-- From_spark
	--
	function From_Spark(Obj : in Primitive.Object_Type)
	return Object_Type;

	--
	-- To_Spark
	--
	function To_Spark(Obj : in Object_Type)
	return Primitive.Object_Type;

end CBE.CXX.CXX_Primitive;
