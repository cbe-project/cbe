--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;
with SHA256_4K;
with Interfaces;
use  Interfaces;

package body CBE.Translation
with SPARK_Mode
is
   --
   --  Initialize_Object
   --
   procedure Initialize_Object (
      Obj     : out Object_Type;
      Helpr   :     Tree_Helper.Object_Type;
      Free_Tr :     Boolean)
   is
   begin
      Obj := Initialized_Object (Helpr, Free_Tr);
   end Initialize_Object;

   --
   --  Initialized_Object
   --
   function Initialized_Object (
      Helpr   : Tree_Helper.Object_Type;
      Free_Tr : Boolean)
   return Object_Type
   is (
      Walk             => (others => Type_1_Node_Info_Invalid),
      Data             => Data_Initialized_Object,
      Current          => Primitive.Invalid_Object,
      Level            => Tree_Level_Invalid,
      Next_PBA         => PBA_Invalid,
      Data_PBA         => PBA_Invalid,
      Suspended        => False,
      Execute_Progress => False,
      Free_Tree        => Free_Tr,
      Helper           => Helpr);

   --
   --  Height
   --
   function Height (Obj : Object_Type)
   return Tree_Level_Type
   is (Tree_Helper.Height (Obj.Helper));

   --
   --  Index
   --
   function Index (
      Obj   : Object_Type;
      VBA   : Virtual_Block_Address_Type;
      Level : Tree_Level_Type)
   return Tree_Child_Index_Type
   is (Tree_Helper.Index (Obj.Helper, VBA, Level));

   --
   --  Acceptable
   --
   function Acceptable (Obj : Object_Type)
   return Boolean
   is (not Primitive.Valid (Obj.Current) and then not Obj.Suspended);

   --
   --  Suspend
   --
   procedure Suspend (Obj : in out Object_Type)
   is
   begin
      Obj.Suspended := True;
   end Suspend;

   --
   --  Resume
   --
   procedure Resume (Obj : in out Object_Type)
   is
   begin
      Obj.Suspended := False;
   end Resume;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj       : in out Object_Type;
      Root_PBA  :        Physical_Block_Address_Type;
      Root_Gen  :        Generation_Type;
      Root_Hash :        Hash_Type;
      Prim      :        Primitive.Object_Type)
   is
   begin
      Obj.Data  := Data_Initialized_Object;
      Obj.Level := Tree_Helper.Height (Obj.Helper);

      for Index in 0 .. Obj.Level loop
         Obj.Walk (Natural (Index)) := Type_1_Node_Info_Invalid;
      end loop;

      Obj.Walk (Natural (Obj.Level)) := (Root_PBA, Root_Gen, Root_Hash);

      Obj.Current := Prim;
      Obj.Data_PBA := PBA_Invalid;
      Obj.Next_PBA := Obj.Walk (Natural (Obj.Level)).PBA;

   end Submit_Primitive;

   --
   --  Execute
   --
   procedure Execute (
      Obj        : in out Object_Type;
      Trans_Data :        Translation_Data_Type)
   is
   begin
      Obj.Execute_Progress := False;

      --  no active translation request or request already translated
      if
         not Primitive.Valid (Obj.Current) or else
         Obj.Data_PBA /= PBA_Invalid
      then
         return;
      end if;

      if not Data_Available (Obj.Data, Obj.Level) then

         --  data request already pending
         if Obj.Next_PBA /= PBA_Invalid then
            return;
         end if;

         --  or use previous level data to get next level
         declare
            Node : constant Type_I_Node_Type :=
               Get_Node (
                  Trans_Data (0),
                  Tree_Helper.Index (
                     Obj.Helper,
                     Virtual_Block_Address_Type (
                        Primitive.Block_Number (Obj.Current)),
                     Obj.Level + 1));
         begin
            Obj.Walk (Natural (Obj.Level)) := (
               Node.PBA, Node.Gen, Node.Hash);

            Obj.Next_PBA := Obj.Walk (Natural (Obj.Level)).PBA;
            Obj.Execute_Progress := True;
            return;
         end;
      end if;

      --  check hash
      Declare_SHA_Args :
      declare
         SHA_Data : SHA256_4K.Data_Type with Address => Trans_Data (0)'Address;
         SHA_Hash : SHA256_4K.Hash_Type;
         CBE_Hash : Hash_Type with Address => SHA_Hash'Address;
      begin
         SHA256_4K.Hash (SHA_Data, SHA_Hash);
         if CBE_Hash /= Obj.Walk (Natural (Obj.Level)).Hash then
            raise Program_Error;
         end if;
      end Declare_SHA_Args;

      --
      --  We query the next level and should it already be the last,
      --  we have found the pba for the data node.
      --
      --  For the VBD we are interested in the pba of the leaf
      --  but for the FT we look for the pba of the type 2 node.
      --
      --  XXX currently the free-tree MUST HAVE a height at least of 2,
      --     otherwise the entry in the type 2 node is treated as the
      --     type 2 node itself.
      --
      declare
         Data_Level : constant Tree_Level_Type :=
            (if Obj.Free_Tree then 1 else 0);
      begin
         Obj.Level := Obj.Level - 1;
         if Obj.Level = Data_Level then
            declare
               Parent_Level : constant Tree_Level_Type := Data_Level + 1;
               Node : constant Type_I_Node_Type :=
                  Get_Node (
                     Trans_Data (0),
                     Tree_Helper.Index (
                        Obj.Helper,
                        Virtual_Block_Address_Type (
                           Primitive.Block_Number (Obj.Current)),
                        Parent_Level));
            begin
               Obj.Walk (Natural (Obj.Level)) := (
                  Node.PBA, Node.Gen, Node.Hash);

               Obj.Data_PBA := Obj.Walk (Natural (Obj.Level)).PBA;
            end;
         end if;
         Obj.Execute_Progress := True;
      end;
   end Execute;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (
      if Obj.Data_PBA /= PBA_Invalid then
         Primitive.Valid_Object (
            Op     => Primitive.Operation (Obj.Current),
            Succ   => False,
            Tg     => Primitive.Tag (Obj.Current),
            Blk_Nr => Block_Number_Type (Obj.Data_PBA),
            Idx    => Primitive.Index (Obj.Current))
      else
         Primitive.Invalid_Object);

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type)
   is
   begin
      --  allow further translation requests
      Obj.Current := Primitive.Invalid_Object;

      --  reset completed primitive
      Obj.Data_PBA := PBA_Invalid;

   end Drop_Completed_Primitive;

   --
   --  Get_Virtual_Block_Address
   --
   function Get_Virtual_Block_Address (Obj : Object_Type)
   return Block_Number_Type
   is (Primitive.Block_Number (Obj.Current));

   --
   --  Can_Get_Type_1_Info
   --
   function Can_Get_Type_1_Info (
      Obj   : Object_Type;
      Prim  : Primitive.Object_Type)
   return Boolean
   is (
      Obj.Data_PBA /= PBA_Invalid and then
      Physical_Block_Address_Type (Primitive.Block_Number (Prim)) =
         Obj.Data_PBA);

   --
   --  Get_Type_1_Info
   --
   procedure Get_Type_1_Info (
      Obj   :        Object_Type;
      Infos : in out Type_1_Node_Infos_Type)
   is
   begin
      for Level in 0 .. Tree_Helper.Height (Obj.Helper) loop
         Infos (Natural (Level)) := Obj.Walk (Natural (Level));
      end loop;
   end Get_Type_1_Info;

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (
      if Obj.Next_PBA /= PBA_Invalid then
         Primitive.Valid_Object (
            Op     => Read,
            Succ   => False,
            Tg     => Tag_Translation,
            Blk_Nr => Block_Number_Type (Obj.Next_PBA),
            Idx    => 0)
      else
         Primitive.Invalid_Object);

   --
   --  Discard_Generated_Primitive
   --
   procedure Discard_Generated_Primitive (Obj : in out Object_Type)
   is
   begin
      Obj.Next_PBA := PBA_Invalid;
   end Discard_Generated_Primitive;

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj        : in out Object_Type;
      Data       :        Block_Data_Type;
      Trans_Data : in out Translation_Data_Type)
   is
   begin
      Data_Set_Available (Obj.Data, Obj.Level);
      Trans_Data (0) := Data;
   end Mark_Generated_Primitive_Complete;

   --
   --  Get_Node
   --
   function Get_Node (
      Data  : Block_Data_Type;
      Index : Tree_Child_Index_Type)
   return Type_I_Node_Type
   is
      Node_Block : Type_I_Node_Block_Type with Address => Data'Address;
   begin
      return Node_Block (Natural (Index));
   end Get_Node;

   --
   --  Data_Initialized_Object
   --
   function Data_Initialized_Object
   return Data_Type
   is (Avail => 0);

   --
   --  Data_Available
   --
   function Data_Available (
      Data  : Data_Type;
      Level : Tree_Level_Type)
   return Boolean
   is (
      (Unsigned_32 (Data.Avail) and
       Shift_Left (Unsigned_32 (1), Natural (Level - 1))) /= 0);

   --
   --  Data_Set_Available
   --
   procedure Data_Set_Available (
      Data  : in out Data_Type;
      Level :        Tree_Level_Type)
   is
   begin
      Data.Avail := Data_Available_Type (
         Unsigned_32 (Data.Avail) or
         Shift_Left (Unsigned_32 (1), Natural (Level - 1)));
   end Data_Set_Available;

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean
   is (Obj.Execute_Progress);

   function Data_PBA (Obj : Object_Type) return Physical_Block_Address_Type
   is (Obj.Data_PBA);

   function Next_PBA (Obj : Object_Type) return Physical_Block_Address_Type
   is (Obj.Next_PBA);

   function Current (Obj : Object_Type) return Primitive.Object_Type
   is (Obj.Current);

   function Max_Levels (Obj : Object_Type) return Tree_Level_Type
   is (6);

   function To_String (Obj : Object_Type) return String
   is (
      "Translation (Current=" & Primitive.To_String (Obj.Current) &
      ", Level=" & Debug.To_String (Debug.Uint64_Type (Obj.Level)) &
      ", Next_PBA=" & Debug.To_String (Obj.Next_PBA) &
      ", Data_PBA=" & Debug.To_String (Obj.Data_PBA) &
      ", Suspended=" & Debug.To_String (Obj.Suspended) &
      ", Free_Tree=" & Debug.To_String (Obj.Free_Tree) &
      ", Execute_Progress=" & Debug.To_String (Obj.Execute_Progress) &
      ")");

end CBE.Translation;
