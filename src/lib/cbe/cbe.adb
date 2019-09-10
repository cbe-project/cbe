--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE
with Spark_Mode
is
	procedure Snapshot_Discard (Snap : in out Snapshot_Type)
	is
	begin
		Snap.ID := Snapshot_ID_Invalid;
	end Snapshot_Discard;

	procedure Print_String (S : String)
	is
	begin
		Print_Cstring(S, S'Length);
	end;

end CBE;
