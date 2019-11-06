--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with Interfaces;
use  Interfaces;

package CBE.Tree_Helper
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   --
   --  Initialized_Object
   --
   function Initialized_Object (
      Degr : Tree_Degree_Type;
      Hght : Tree_Level_Type;
      Lfs  : Tree_Number_Of_Leafs_Type)
   return Object_Type;

   --
   --  Index
   --
   function Index (
      Obj   : Object_Type;
      VBA   : Virtual_Block_Address_Type;
      Level : Tree_Level_Type)
   return Tree_Child_Index_Type;

   -----------------
   --  Accessors  --
   -----------------

   function Height (Obj : Object_Type) return Tree_Level_Type;
   function Degree (Obj : Object_Type) return Tree_Degree_Type;
   function Leafs (Obj : Object_Type) return Tree_Number_Of_Leafs_Type;

private

   type Degree_Mask_Type
   is range 2**Tree_Min_Degree_Log_2 - 1 .. 2**Tree_Max_Degree_Log_2 - 1;

   type Degree_Log_2_Type
   is range Tree_Min_Degree_Log_2 .. Tree_Max_Degree_Log_2;

   --
   --  Log_2
   --
   function Log_2 (Value : Unsigned_32) return Unsigned_32;

   --
   --  Object_Type
   --
   type Object_Type is record
      Degree       : Tree_Degree_Type;
      Height       : Tree_Level_Type;
      Leafs        : Tree_Number_Of_Leafs_Type;
      Degree_Log_2 : Degree_Log_2_Type;
      Degree_Mask  : Degree_Mask_Type;
   end record;

end CBE.Tree_Helper;
