--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package CBE
with Spark_Mode
is
	pragma Pure;

	type Number_Of_Primitives_Type is mod 2**64;

	type Byte_Type is range 0..255 with Size => 8;

	type Block_Data_Type is array (0..4095) of Byte_Type;

end CBE;
