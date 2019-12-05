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

package body CBE.Translation
with SPARK_Mode
is
   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type);

   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type);

   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type)
   is
      SHA_Idx : SHA256_4K.Hash_Index_Type := SHA256_4K.Hash_Index_Type'First;
   begin
      for CBE_Idx in CBE_Hash'Range loop
         CBE_Hash (CBE_Idx) := Byte_Type (SHA_Hash (SHA_Idx));
         if CBE_Idx < CBE_Hash'Last then
            SHA_Idx := SHA_Idx + 1;
         end if;
      end loop;
   end CBE_Hash_From_SHA256_4K_Hash;

   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type)
   is
      CBE_Idx : Block_Data_Index_Type := Block_Data_Index_Type'First;
   begin
      for SHA_Idx in SHA_Data'Range loop
         SHA_Data (SHA_Idx) := SHA256_4K.Byte (CBE_Data (CBE_Idx));
         if SHA_Idx < SHA_Data'Last then
            CBE_Idx := CBE_Idx + 1;
         end if;
      end loop;
   end SHA256_4K_Data_From_CBE_Data;

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
      Walk             => (others => Type_1_Node_Invalid),
      Data             => Data_Initialized_Object,
      Current          => Primitive.Invalid_Object,
      Level            => Tree_Level_Index_Type'Last,
      Next_PBA         => PBA_Invalid,
      Data_PBA         => PBA_Invalid,
      Suspended        => False,
      Execute_Progress => False,
      Free_Tree        => Free_Tr,
      Helper           => Helpr);

   --
   --  Max_Level
   --
   function Max_Level (Obj : Object_Type)
   return Tree_Level_Index_Type
   is (Tree_Helper.Max_Level (Obj.Helper));

   --
   --  Index
   --
   function Index (
      Obj   : Object_Type;
      VBA   : Virtual_Block_Address_Type;
      Level : Tree_Level_Index_Type)
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
      Obj.Level := Tree_Helper.Max_Level (Obj.Helper);

      for Index in Obj.Walk'Range loop
         Obj.Walk (Index) := Type_1_Node_Invalid;
      end loop;

      Obj.Walk (Obj.Level) := (Root_PBA, Root_Gen, Root_Hash);
      Obj.Current := Prim;
      Obj.Data_PBA := PBA_Invalid;
      Obj.Next_PBA := Obj.Walk (Obj.Level).PBA;

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

      if not Obj.Data.Avail (Obj.Level - 1) then

         --  data request already pending
         if Obj.Next_PBA /= PBA_Invalid then
            return;
         end if;

         --  or use previous level data to get next level
         declare
            Node : constant Type_1_Node_Type :=
               Get_Node (
                  Trans_Data (0),
                  Tree_Helper.Index (
                     Obj.Helper,
                     Virtual_Block_Address_Type (
                        Primitive.Block_Number (Obj.Current)),
                     Obj.Level + 1));
         begin
            Obj.Walk (Obj.Level) := (
               Node.PBA, Node.Gen, Node.Hash);

            Obj.Next_PBA := Obj.Walk (Obj.Level).PBA;
            Obj.Execute_Progress := True;
            return;
         end;
      end if;

      --  check hash
      Declare_SHA_Args :
      declare
         SHA_Data : SHA256_4K.Data_Type;
         SHA_Hash : SHA256_4K.Hash_Type;
         CBE_Hash : Hash_Type;
      begin
         SHA256_4K_Data_From_CBE_Data (SHA_Data, Trans_Data (0));
         SHA256_4K.Hash (SHA_Data, SHA_Hash);
         CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
         if CBE_Hash /= Obj.Walk (Obj.Level).Hash then
            pragma Debug (Debug.Print_String ("Translation: " & "Level: "
               & Debug.To_String (Debug.Uint64_Type (Obj.Level)) & " "
               & "PBA: "
               & Debug.To_String (Debug.Uint64_Type (
                  Obj.Walk (Obj.Level).PBA)) & " "
               & "GOT: " & Debug.To_String (CBE_Hash) & " "
               & "EXP: " & Debug.To_String (
                  Obj.Walk (Obj.Level).Hash)));
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
      declare
         Data_Level : constant Tree_Level_Index_Type :=
            (if Obj.Free_Tree then 1 else 0);
      begin
         Obj.Level := Obj.Level - 1;
         if Obj.Level = Data_Level then
            declare
               Parent_Level : constant Tree_Level_Index_Type := Data_Level + 1;
               Node : constant Type_1_Node_Type :=
                  Get_Node (
                     Trans_Data (0),
                     Tree_Helper.Index (
                        Obj.Helper,
                        Virtual_Block_Address_Type (
                           Primitive.Block_Number (Obj.Current)),
                        Parent_Level));
            begin
               Obj.Walk (Obj.Level) := (Node.PBA, Node.Gen, Node.Hash);
               Obj.Data_PBA := Obj.Walk (Obj.Level).PBA;
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
         Primitive.Copy_Valid_Object_New_Succ_Blk_Nr (
            Obj.Current, False, Block_Number_Type (Obj.Data_PBA))
      else
         Primitive.Invalid_Object);

   --
   --  Peek_Completed_Hash
   --
   function Peek_Completed_Hash (Obj : Object_Type)
   return Hash_Type
   is
   begin
      if Obj.Data_PBA /= PBA_Invalid then
         return Obj.Walk (Obj.Level).Hash;
      else
         raise Program_Error;
      end if;
   end Peek_Completed_Hash;

   --
   --  Peek_Completed_Generation
   --
   function Peek_Completed_Generation (Obj : Object_Type)
   return Generation_Type
   is
   begin
      if Obj.Data_PBA /= PBA_Invalid then
         return Obj.Walk (Obj.Level).Gen;
      else
         raise Program_Error;
      end if;
   end Peek_Completed_Generation;

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
   --  Can_Get_Type_1_Node_Walk
   --
   function Can_Get_Type_1_Node_Walk (
      Obj   : Object_Type;
      Prim  : Primitive.Object_Type)
   return Boolean
   is (
      Obj.Data_PBA /= PBA_Invalid and then
      Physical_Block_Address_Type (Primitive.Block_Number (Prim)) =
         Obj.Data_PBA);

   --
   --  Get_Type_1_Node_Walk
   --
   procedure Get_Type_1_Node_Walk (
      Obj  :        Object_Type;
      Walk : in out Type_1_Node_Walk_Type)
   is
   begin
      for Level in 0 .. Tree_Helper.Max_Level (Obj.Helper) loop
         Walk (Level) := Obj.Walk (Level);
      end loop;
   end Get_Type_1_Node_Walk;

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (
      if Obj.Next_PBA /= PBA_Invalid then
         Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     => Primitive.Tag_Translation,
            Blk_Nr => Block_Number_Type (Obj.Next_PBA),
            Idx    => 0)
      else
         Primitive.Invalid_Object);

   function Peek_Generated_Level (Obj : Object_Type)
   return Tree_Level_Index_Type is (Obj.Level);

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
      Obj.Data.Avail (Obj.Level - 1) := True;
      Trans_Data (0) := Data;
   end Mark_Generated_Primitive_Complete;

   --
   --  Get_Node
   --
   function Get_Node (
      Data  : Block_Data_Type;
      Index : Tree_Child_Index_Type)
   return Type_1_Node_Type
   is
      Nodes : Type_1_Node_Block_Type;
   begin
      Type_1_Node_Block_From_Block_Data (Nodes, Data);
      return Nodes (Natural (Index));
   end Get_Node;

   --
   --  Data_Initialized_Object
   --
   function Data_Initialized_Object
   return Data_Type
   is (
      Avail => (others => False));

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
