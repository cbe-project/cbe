--
-- Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
-- This file is part of the Consistent Block Encrypter project, which is
-- distributed under the terms of the GNU Affero General Public License
-- version 3.
--

pragma Ada_2012;

package body CBE.Tree_Helper
with Spark_Mode
is

	--
	-- Initialized_Object
	--
	function Initialized_Object (
		Degr : Tree_Degree_Type;
		Hght : Tree_Level_Type;
		Lfs  : Tree_Number_Of_Leafs_Type)
	return Object_Type
	is
		Degr_Log_2 : constant Tree_Degree_Log_2_Type :=
			Tree_Degree_Log_2_Type (Log_2 (Unsigned_32 (Degr)));
	begin
		return (
			Degree       => Degr,
			Height       => Hght,
			Leafs        => Lfs,
			Degree_Log_2 => Degr_Log_2,
			Degree_Mask  =>
				Tree_Degree_Mask_Type (
					Shift_Left (Unsigned_32(1), Natural (Degr_Log_2))) - 1);

	end Initialized_Object;

	--
	-- Index
	--
	function Index (
		Obj   : Object_Type;
		VBA   : Virtual_Block_Address_Type;
		Level : Tree_Level_Type)
	return Tree_Child_Index_Type
	is (
		Tree_Child_Index_Type (
			Shift_Right (Unsigned_64 (VBA), Natural (Unsigned_32 (Obj.Degree_Log_2) * (Unsigned_32 (Level) - 1)))
				and Unsigned_64(Obj.Degree_Mask)));

	--
	-- Log_2
	--
	function  Log_2(Value : Unsigned_32)
	return Unsigned_32
	is
		type Bit_index_Type is range 0..31;
	begin
		if Value = 0 then
			raise Program_Error;
		end if;
		for Bit_Index in reverse Bit_Index_Type'Range loop
			if  (Value and Shift_Left (Unsigned_32 (1), Natural (Bit_Index))) /= 0 then
				return Unsigned_32 (Bit_Index);
			end if;
		end loop;
		raise Program_Error;
	end;


	---------------
	-- Accessors --
	---------------

	function Height (Obj : Object_Type) return Tree_Level_Type is (Obj.Height);
	function Degree (Obj : Object_Type) return Tree_Degree_Type is (Obj.Degree);
	function Degree_Log_2 (Obj : Object_Type) return Tree_Degree_Log_2_Type is (Obj.Degree_Log_2);
	function Degree_Mask (Obj : Object_Type) return Tree_Degree_Mask_Type is (Obj.Degree_Mask);
	function Leafs (Obj : Object_Type) return Tree_Number_Of_Leafs_Type is (Obj.Leafs);

end CBE.Tree_Helper;
