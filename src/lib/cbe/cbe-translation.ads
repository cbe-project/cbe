--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Tree_Helper;
with CBE.Primitive;

package CBE.Translation
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   --
   --  Initialize an object of the translation module
   --
   --  \param Obj      resulting object
   --  \param Helpr    tree helper object used by the translation object
   --  \param Free_Tr  wether the object is used by the free tree or the VBD
   --
   procedure Initialize_Object (
      Obj     : out Object_Type;
      Helpr   :     Tree_Helper.Object_Type;
      Free_Tr :     Boolean);

   --
   --  Return an initialized object of the translation module
   --
   function Initialized_Object (
      Helpr   : Tree_Helper.Object_Type;
      Free_Tr : Boolean)
   return Object_Type;

   --
   --  Return height of the tree
   --
   --  \param Obj  translation-module object
   --
   function Height (Obj : Object_Type)
   return Tree_Level_Type;

   --
   --  Return child index of given level for a virtual address
   --
   function Index (
      Obj   : Object_Type;
      VBA   : Virtual_Block_Address_Type;
      Level : Tree_Level_Type)
   return Tree_Child_Index_Type;

   --
   --  Return whether the pool can accept a new request
   --
   function Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Suspend
   --
   procedure Suspend (Obj : in out Object_Type);

   --
   --  Resume
   --
   procedure Resume (Obj : in out Object_Type);

   --
   --  Submit a new translation request
   --
   --  \param Root_PBA  physical block address of the root of the tree
   --  \param Prim      the primitive for the request
   --
   procedure Submit_Primitive (
      Obj       : in out Object_Type;
      Root_PBA  :        Physical_Block_Address_Type;
      Root_Gen  :        Generation_Type;
      Root_Hash :        Hash_Type;
      Prim      :        Primitive.Object_Type);

   --
   --  Execute
   --
   procedure Execute (
      Obj        : in out Object_Type;
      Trans_Data :        Translation_Data_Type);

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
   --  Get_Virtual_Block_Address
   --
   function Get_Virtual_Block_Address (Obj : Object_Type)
   return Block_Number_Type;

   --
   --  Can_Get_Type_1_Info
   --
   function Can_Get_Type_1_Info (
      Obj   : Object_Type;
      Prim  : Primitive.Object_Type)
   return Boolean;

   --
   --  Get_Type_1_Info
   --
   procedure Get_Type_1_Info (
      Obj   :        Object_Type;
      Infos : in out Type_1_Node_Infos_Type);

   --
   --  Return the generated primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Discard the generated primitive
   --
   procedure Discard_Generated_Primitive (Obj : in out Object_Type);

   --
   --  Mark the generated primitive as completed
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj        : in out Object_Type;
      Data       :        Block_Data_Type;
      Trans_Data : in out Translation_Data_Type);

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean;
   function Data_PBA (Obj : Object_Type) return Physical_Block_Address_Type;
   function Next_PBA (Obj : Object_Type) return Physical_Block_Address_Type;
   function Current (Obj : Object_Type) return Primitive.Object_Type;
   function Max_Levels (Obj : Object_Type) return Tree_Level_Type;

   function To_String (Obj : Object_Type) return String;

private

   type Data_Available_Type is mod 2**32;

   type Data_Type is record
      Avail : Data_Available_Type;
   end record;

   type Object_Type is record
      Walk             : Type_1_Node_Infos_Type;
      Data             : Data_Type;
      Current          : Primitive.Object_Type;
      Level            : Tree_Level_Type;
      Next_PBA         : Physical_Block_Address_Type;
      Data_PBA         : Physical_Block_Address_Type;
      Suspended        : Boolean;
      Free_Tree        : Boolean;
      Helper           : Tree_Helper.Object_Type;
      Execute_Progress : Boolean;
   end record;

   --
   --  Data_Invalid_Object
   --
   function Data_Initialized_Object
   return Data_Type;

   --
   --  Data_Available
   --
   function Data_Available (
      Data  : Data_Type;
      Level : Tree_Level_Type)
   return Boolean;

   --
   --  Data_Set_Available
   --
   procedure Data_Set_Available (
      Data  : in out Data_Type;
      Level :        Tree_Level_Type);

   --
   --  Get_Node
   --
   function Get_Node (
      Data  : Block_Data_Type;
      Index : Tree_Child_Index_Type)
   return Type_I_Node_Type;

end CBE.Translation;
