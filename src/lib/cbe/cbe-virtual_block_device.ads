--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.Tree_Helper;
with CBE.Cache;
with CBE.Translation;

package CBE.Virtual_Block_Device
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   --
   --  Initialize_Object
   --
   --  FIXME will not be used anymore when the library module is in spark
   --
   procedure Initialize_Object (
      Obj    : out Object_Type;
      Height :     Tree_Level_Type;
      Degree :     Tree_Degree_Type;
      Leafs  :     Tree_Number_Of_Leafs_Type);

   --
   --  Initialized_Object
   --
   function Initialized_Object (
      Height :     Tree_Level_Type;
      Degree :     Tree_Degree_Type;
      Leafs  :     Tree_Number_Of_Leafs_Type)
   return Object_Type;

   --
   --  Trans_Inhibit_Translation
   --
   procedure Trans_Inhibit_Translation (Obj : in out Object_Type);

   --
   --  Trans_Resume_Translation
   --
   procedure Trans_Resume_Translation (Obj : in out Object_Type);

   --
   --  Trans_Get_Virtual_Block_Address
   --
   function Trans_Get_Virtual_Block_Address (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Block_Number_Type;

   --
   --  Trans_Can_Get_Type_1_Info
   --
   function Trans_Can_Get_Type_1_Info (
      Obj   : Object_Type;
      Prim  : Primitive.Object_Type)
   return Boolean;

   --
   --  Trans_Get_Type_1_Info
   --
   procedure Trans_Get_Type_1_Info (
      Obj   :        Object_Type;
      Infos : in out Type_1_Node_Infos_Type);

   --
   --  Tree_Height
   --
   function Tree_Height (Obj : Object_Type)
   return Tree_Level_Type;

   --
   --  Index_For_Level
   --
   function Index_For_Level (
      Obj   : Object_Type;
      VBA   : Virtual_Block_Address_Type;
      Level : Tree_Level_Type)
   return Tree_Child_Index_Type;

   --
   --  Get_Tree_Helper
   --
   function Get_Tree_Helper (Obj : Object_Type)
   return Tree_Helper.Object_Type;

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj  : in out Object_Type;
      PBA  :        Physical_Block_Address_Type;
      Gen  :        Generation_Type;
      Hash :        Hash_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type);

   --
   --  Execute
   --
   procedure Execute (
      Obj        : in out Object_Type;
      Trans_Data : in out Translation_Data_Type;
      Cach       : in out Cache.Object_Type;
      Cach_Data  :        Cache.Cache_Data_Type;
      Timestamp  :        Timestamp_Type);

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean;

   function To_String (Obj : Object_Type) return String;

private

   type Object_Type is record
      Trans_Helper     : Tree_Helper.Object_Type;
      Trans            : Translation.Object_Type;
      Execute_Progress : Boolean;
   end record;

end CBE.Virtual_Block_Device;
