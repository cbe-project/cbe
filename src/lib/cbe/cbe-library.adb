--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Tree_Helper;
with CBE.Debug;

package body CBE.Library
with SPARK_Mode
is
   procedure Try_Discard_Snapshot (
      Snaps     : in out Snapshots_Type;
      Keep_Snap :        Snapshots_Index_Type;
      Success   :    out Boolean)
   is
      Discard_Idx       : Snapshots_Index_Type := Snapshots_Index_Type'First;
      Discard_Idx_Valid : Boolean              := False;
   begin
      For_Snapshots :
      for Idx in Snapshots_Index_Type loop
         if
            Idx /= Keep_Snap and then
            Snapshot_Valid (Snaps (Idx)) and then
            not Snapshot_Keep (Snaps (Idx)) and then (
               not Discard_Idx_Valid or else
               Snaps (Idx).ID < Snaps (Discard_Idx).ID)
         then
            Discard_Idx       := Idx;
            Discard_Idx_Valid := True;
         end if;
      end loop For_Snapshots;
      if Discard_Idx_Valid then
         Snapshot_Valid (Snaps (Discard_Idx), False);
      end if;
      Success := Discard_Idx_Valid;
   end Try_Discard_Snapshot;

   procedure Discard_Snapshot (
      Obj     : in out Object_Type;
      Snap_ID :        Generation_Type;
      Success :    out Boolean)
   is
   begin
      Success := False;

      Loop_Discard_Snapshot :
      for I in Snapshots_Index_Type loop
         if Snapshot_Valid (Obj.Superblocks (Obj.Cur_SB).Snapshots (
               I)) and then
            Snapshot_Keep (Obj.Superblocks (Obj.Cur_SB).Snapshots (
               I)) and then
            Obj.Superblocks (Obj.Cur_SB).Snapshots (I).Gen = Snap_ID
         then
            Snapshot_Valid (Obj.Superblocks (Obj.Cur_SB).Snapshots (I), False);
            Success := True;
            Obj.Secure_Superblock := True;
            exit Loop_Discard_Snapshot;
         end if;
      end loop Loop_Discard_Snapshot;
   end Discard_Snapshot;

   function Cache_Dirty (Obj : Object_Type)
   return Boolean
   is
      Result : Boolean := False;
   begin
      For_Cache_Data :
      for Cache_Index in Cache.Cache_Index_Type loop
         if Cache.Dirty (Obj.Cache_Obj, Cache_Index) then
            Result := True;
            exit For_Cache_Data;
         end if;
      end loop For_Cache_Data;
      return Result;
   end Cache_Dirty;

   procedure Create_Snapshot (
      Obj     : in out Object_Type;
      Quara   :        Boolean;
      Snap_ID :    out Snapshot_ID_Type)
   is
   begin

      --
      --  As long as we are creating a snapshot or
      --  the superblock the new snapshot belongs to is being
      --  written we do not allow the creation of a new
      --  snapshot.
      --
      if not Obj.Creating_Snapshot or else
         not Obj.Secure_Superblock
      then
         if not Cache_Dirty (Obj) then
            Snap_ID := Snapshot_ID_Type (Obj.Last_Secured_Generation);

            pragma Debug (Debug.Print_String ("Creating_Snapshot: "
               & "cache not dirty - no new snapshot: "
               & Debug.To_String (Debug.Uint64_Type (Snap_ID))));
            return;
         end if;

         Obj.Creating_Snapshot := True;
         Obj.Creating_Quarantine_Snapshot := Quara;
      end if;
      Snap_ID := Snapshot_ID_Type (Obj.Cur_Gen);

      pragma Debug (Debug.Print_String ("Creating_Snapshot: id: "
         & Debug.To_String (Debug.Uint64_Type (Snap_ID))));
   end Create_Snapshot;

   procedure Create_Snapshot_Internal (
      Obj      : in out Object_Type;
      Progress :    out Boolean)
   is
   begin
      Delcare_Cache_Flusher_Active :
      declare
         Cache_Flusher_Active : constant Boolean :=
            Cache_Flusher.Active (Obj.Cache_Flusher_Obj);
      begin
         if Cache_Flusher_Active or else Obj.Secure_Superblock then
            pragma Debug (Debug.Print_String ("Create_Snapshot_Internal: "
                & "flusher active: "
                & Debug.To_String (Cache_Flusher_Active) & " "
                & "Secure_Superblock: "
                & Debug.To_String (Obj.Secure_Superblock)));
            Progress := False;
            return;
         end if;
      end Delcare_Cache_Flusher_Active;

      Declare_Cache_Dirty :
      declare
         Cache_Dirty : Boolean := False;
      begin

         Check_Cache_Dirty :
         for Cache_Index in Cache.Cache_Index_Type loop
            if Cache.Dirty (Obj.Cache_Obj, Cache_Index) then

               Cache_Dirty := True;

               Cache_Flusher.Submit_Request (
                  Obj.Cache_Flusher_Obj,
                  Cache.Flush (Obj.Cache_Obj, Cache_Index),
                  Cache_Index);
            end if;
         end loop Check_Cache_Dirty;

         --
         --  In case we have to flush the Cache, wait until we have
         --  finished doing that.
         --
         if not Cache_Dirty then
            pragma Debug (Debug.Print_String ("Create_Snapshot_Internal: "
               & "snapshot created: "
               & "gen: " & Debug.To_String (Debug.Uint64_Type (Obj.Cur_Gen))));

            Obj.Superblocks (Obj.Cur_SB).Snapshots (
               Curr_Snap (Obj)).Flags := (
                  if Obj.Creating_Quarantine_Snapshot then 1 else 0);
            --  Obj.Cur_Gen := Obj.Cur_Gen  + 1;
            --  trigger securing of suerblock
            --  Obj.Creating_Snapshot := False;
            Obj.Secure_Superblock := True;
         end if;
         Progress := True;
      end Declare_Cache_Dirty;
   end Create_Snapshot_Internal;

   function Snapshot_Creation_Complete (
      Obj     : Object_Type;
      Snap_ID : Snapshot_ID_Type)
   return Boolean
   is
      Result : constant Boolean := (Obj.Last_Secured_Generation =
         Generation_Type (Snap_ID));
   begin
      pragma Debug (Debug.Print_String ("Snapshot_Creation_Complete: " &
         Debug.To_String (Debug.Uint64_Type (Obj.Last_Secured_Generation))
         & " = "
         & Debug.To_String (Debug.Uint64_Type (Snap_ID)) & " "
         & " result: " & Debug.To_String (Result)));

      return Result;
   end Snapshot_Creation_Complete;

   procedure Active_Snapshot_IDs (
      Obj  :     Object_Type;
      List : out Active_Snapshot_IDs_Type)
   is
   begin
      For_Snapshots :
      for Snap_ID in Snapshots_Index_Type loop

         if Snapshot_Valid (
            Obj.Superblocks (Obj.Cur_SB).Snapshots (Snap_ID)) and then
            Snapshot_Keep (
               Obj.Superblocks (Obj.Cur_SB).Snapshots (Snap_ID))
         then
            List (Snap_ID) := Snapshot_ID_Storage_Type (Obj.Superblocks (
               Obj.Cur_SB).Snapshots (Snap_ID).Gen);
         else
            List (Snap_ID) := Snapshot_ID_Storage_Type (0);
         end if;
      end loop For_Snapshots;
   end Active_Snapshot_IDs;

   procedure Initialize_Object (
      Obj     : out Object_Type;
      SBs     :     Superblocks_Type;
      Curr_SB :     Superblocks_Index_Type)
   is
      Curr_Snap : constant Snapshots_Index_Type :=
         Snapshots_Index_Type (SBs (Curr_SB).Curr_Snap);

      Degree : constant Tree_Degree_Type := SBs (Curr_SB).Degree;
      Height : constant Tree_Level_Type  :=
         SBs (Curr_SB).Snapshots (Curr_Snap).Height;

      Leafs : constant Tree_Number_Of_Leafs_Type :=
         SBs (Curr_SB).Snapshots (Curr_Snap).Nr_Of_Leafs;
   begin
      --
      --  The Current implementation is limited with regard to the
      --  tree topology. Make sure it fits.
      --
      if Height > Tree_Max_Height or else Height < Tree_Min_Height then
         raise Program_Error;
      end if;

      Obj.Execute_Progress := False;
      Obj.Request_Pool_Obj        := Pool.Initialized_Object;
      Obj.Splitter_Obj            := Splitter.Initialized_Object;
      Obj.Crypto_Obj              := Crypto.Initialized_Object;

      Obj.IO_Obj                  := Block_IO.Initialized_Object;
      Obj.Cache_Obj               := Cache.Initialized_Object;
      Obj.Cache_Data              := (others => (others => 0));
      Obj.Cache_Job_Data          := (others => (others => 0));
      Obj.Cache_Flusher_Obj       := Cache_Flusher.Initialized_Object;
      Obj.Trans_Data              := (others => (others => 0));
      Obj.VBD                     :=
         Virtual_Block_Device.Initialized_Object (
            Height, Degree, Leafs);

      Obj.Write_Back_Obj          := Write_Back.Initialized_Object;
      Obj.Write_Back_Data         := (others => (others => 0));
      Obj.Sync_SB_Obj             := Sync_Superblock.Initialized_Object;
      Obj.Free_Tree_Obj           := Free_Tree.Initialized_Object (
         SBs (Curr_SB).Free_Number,
         SBs (Curr_SB).Free_Gen,
         SBs (Curr_SB).Free_Hash,
         SBs (Curr_SB).Free_Height,
         SBs (Curr_SB).Free_Degree,
         SBs (Curr_SB).Free_Leafs);

      Obj.Free_Tree_Retry_Count   := 0;
      Obj.Free_Tree_Trans_Data    := (others => (others => 0));
      Obj.Free_Tree_Query_Data    := (others => (others => 0));
      Obj.Superblocks             := SBs;
      Obj.Cur_SB                  := (
         if Curr_SB < Superblocks_Index_Type'Last then
            Curr_SB + 1
         else
            Superblocks_Index_Type'First);
      Obj.Superblocks (Obj.Cur_SB) := Obj.Superblocks (Curr_SB);

      Obj.Cur_Gen                 := SBs (Curr_SB).Last_Secured_Generation + 1;
      Obj.Last_Secured_Generation := SBs (Curr_SB).Last_Secured_Generation;

      Obj.Secure_Superblock       := False;
      Obj.Wait_For_Front_End      := Wait_For_Event_Invalid;
      Obj.Wait_For_Back_End       := Wait_For_Event_Invalid;

      Obj.Creating_Snapshot            := False;
      Obj.Creating_Quarantine_Snapshot := False;
      Obj.Next_Snapshot_ID             := 0;
      Obj.Stall_Snapshot_Creation      := False;

      declare
         Next_Snap : constant Snapshots_Index_Type :=
            Next_Snap_Slot (Obj);
      begin
         Obj.Superblocks (Obj.Cur_SB).Snapshots (Next_Snap) :=
            Obj.Superblocks (Obj.Cur_SB).Snapshots (
               Snapshots_Index_Type (Obj.Superblocks (Obj.Cur_SB).Curr_Snap));
         --
         --  Clear flags to prevent creating a quarantine snapshot
         --  unintentionally.
         --
         Obj.Superblocks (Obj.Cur_SB).Snapshots (Next_Snap).Flags := 0;

         Obj.Superblocks (Obj.Cur_SB).Curr_Snap :=
            Snapshots_Index_Storage_Type (Next_Snap);
      end;

      pragma Debug (Debug.Print_String ("Initial SB state: "));
      pragma Debug (
         Debug.Dump_Current_Superblock (Obj.Superblocks, Obj.Cur_SB));

   end Initialize_Object;

   function Curr_Snap (Obj : Object_Type)
   return Snapshots_Index_Type
   is (Snapshots_Index_Type (Obj.Superblocks (Obj.Cur_SB).Curr_Snap));

   function Snap_Slot_For_ID (
      Obj : Object_Type;
      ID  : Generation_Type)
   return Snapshots_Index_Type
   is
   begin
      for I in Snapshots_Index_Type loop
         if Snapshot_Valid (Obj.Superblocks (Obj.Cur_SB).
            Snapshots (I)) and then
            Obj.Superblocks (Obj.Cur_SB).Snapshots (I).Gen = ID
         then
            return I;
         end if;
      end loop;
      raise Program_Error;
   end Snap_Slot_For_ID;

   function Next_Snap_Slot (Obj : Object_Type)
   return Snapshots_Index_Type
   is
         Next_Snap : Snapshots_Index_Type := Curr_Snap (Obj);
   begin
      --  XXX make sure we end up at the same idx in case
      --  there is no free slot
      Loop_Snap_Slots :
      for Idx in Snapshots_Index_Type loop
         Next_Snap := (
            if Next_Snap < Snapshots_Index_Type'Last then
               Next_Snap + 1
            else
               Snapshots_Index_Type'First);

         exit Loop_Snap_Slots when
            not Snapshot_Valid (Obj.Superblocks (Obj.Cur_SB).
               Snapshots (Next_Snap)) or else
            not Snapshot_Keep (Obj.Superblocks (Obj.Cur_SB).
               Snapshots (Next_Snap));
      end loop Loop_Snap_Slots;
      return Next_Snap;
   end Next_Snap_Slot;

   function Max_VBA (Obj : Object_Type)
   return Virtual_Block_Address_Type
   is
   begin
      return
         Virtual_Block_Address_Type (
            Obj.Superblocks (Obj.Cur_SB).
               Snapshots (Curr_Snap (Obj)).Nr_Of_Leafs - 1);
   end Max_VBA;

   procedure Update_Snapshot_Hash (
      WB       :        Write_Back.Object_Type;
      Curr_Gen :        Generation_Type;
      Snap     : in out Snapshot_Type;
      Prim     :        Primitive.Object_Type)
   is
      PBA : constant Physical_Block_Address_Type :=
         Write_Back.Peek_Completed_Root (WB, Prim);
   begin
      Snap.Gen := Curr_Gen;
      Snap.PBA := PBA;
      Write_Back.Peek_Completed_Root_Hash (WB, Prim, Snap.Hash);
   end Update_Snapshot_Hash;

   procedure Execute (
      Obj               : in out Object_Type;
      IO_Buf            : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Now               :        Timestamp_Type)
   is
      Progress : Boolean := False;
   begin

      pragma Debug (Debug.Print_String (To_String (Obj)));

      -------------------------
      --  Snapshot handling  --
      -------------------------

      if Obj.Creating_Snapshot and then
         not Obj.Secure_Superblock and then
         not Obj.Stall_Snapshot_Creation
      then
         Create_Snapshot_Internal (Obj, Progress);
      end if;

      --------------------------
      --  Free-tree handling  --
      --------------------------

      --
      --  The FT meta-module uses the Translation module internally and
      --  needs access to the Cache since it wants to use its data.
      --  Because accessing a Cache entry will update its LRU value, the
      --  Cache must be mutable (that is also the reason we need the
      --  time object).
      --
      --  Since it might need to reuse reserved blocks, we have to hand
      --  over all active snapshots as well as the last secured generation.
      --  Both are needed for doing the reuse check.
      --
      --
      --  (Rather than passing the Cache module itself to the FT it might
      --  be better to use a different interface for that purpose as I
      --  do not know how well the current solution works with SPARK...)
      --
      declare
      begin
         Free_Tree.Execute (
            Obj.Free_Tree_Obj,
            Obj.Superblocks (Obj.Cur_SB).Snapshots,
            Obj.Last_Secured_Generation,
            Obj.Free_Tree_Trans_Data,
            Obj.Cache_Obj,
            Obj.Cache_Data,
            Obj.Free_Tree_Query_Data,
            Now);

         if Free_Tree.Execute_Progress (Obj.Free_Tree_Obj) then
            Progress := True;
         end if;
      end;

      --
      --  A complete primitive was either successful or has failed.
      --
      --  In the former case we will instruct the Write_Back module to
      --  write all changed nodes of the VBD back to the block device
      --  and eventually will leadt to ACKing the block Request.
      --
      --  In the later case we will attempt to free reserved blocks in
      --  the FT by discarding snapshots. Briefly speaking all snapshots
      --  that were not specifically marked (see FLAG_KEEP) will be
      --  discarded. A finit number of retries will be performed. If we
      --  cannot free enough blocks, the write operation is marked as
      --  failed and will result in an I/O error at the Block session.
      --
      --
      Loop_Free_Tree_Completed_Prims :
      loop
         Declare_Prim_1 :
         declare
            Prim : constant Primitive.Object_Type :=
               Free_Tree.Peek_Completed_Primitive (Obj.Free_Tree_Obj);
         begin
            exit Loop_Free_Tree_Completed_Prims when
               not Primitive.Valid (Prim) or else
               Primitive.Success (Prim);

            if Obj.Free_Tree_Retry_Count < Free_Tree_Retry_Limit then
               Declare_Could_Discard_Snap :
               declare
                  Could_Discard_Snap : Boolean;
               begin
                  Try_Discard_Snapshot (
                     Obj.Superblocks (Obj.Cur_SB).Snapshots,
                     Curr_Snap (Obj), Could_Discard_Snap);
                  if Could_Discard_Snap then
                     Obj.Free_Tree_Retry_Count :=
                        Obj.Free_Tree_Retry_Count + 1;

                     --
                     --  Instructing the FT to retry the allocation will
                     --  lead to clearing its internal 'query branches'
                     --  state and executing the previously submitted
                     --  Request again.
                     --
                     --  (This retry attempt is a shortcut as we do not have
                     --  all information available at this point to call
                     --  'submit_Request' again - so we must not call
                     --  'drop_Completed_Primitive' as this will clear the
                     --  Request.)
                     --
                     Free_Tree.Retry_Allocation (Obj.Free_Tree_Obj);

                  end if;
               end Declare_Could_Discard_Snap;
               exit Loop_Free_Tree_Completed_Prims;
            end if;

            Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);

            --  FIXME
            Virtual_Block_Device.Trans_Resume_Translation (Obj.VBD);
            Obj.Stall_Snapshot_Creation := False;
            Free_Tree.Drop_Completed_Primitive (Obj.Free_Tree_Obj, Prim);

         end Declare_Prim_1;
         Progress := True;

      end loop Loop_Free_Tree_Completed_Prims;

      --
      --  There are two types of generated primitives by FT module,
      --  the traversing of the tree is done by the internal Translation
      --  module, which will access the nodes through the Cache - I/O
      --  primitives will therefor be generated as a side-effect of the
      --  querying attempt by the Cache module.
      --
      --  - IO_TAG primitives are only used for querying type 2 nodes, i.E.,
      --   inner nodes of the free-tree containg free or reserved blocks.
      --
      --  - WRITE_BACK_TAG primitve are only used for writing one changed
      --   branch back to the block device. Having the branch written
      --   will lead to a complete primitve.
      --
      Loop_Free_Tree_Generated_Prims :
      loop

         Declare_Prim_2 :
         declare
            Prim : constant Primitive.Object_Type :=
               Free_Tree.Peek_Generated_Primitive (Obj.Free_Tree_Obj);
         begin
            exit Loop_Free_Tree_Generated_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Declare_Index_1 :
            declare
               Index : constant Index_Type :=
                  Free_Tree.Peek_Generated_Data_Index (
                     Obj.Free_Tree_Obj, Prim);
               Data_Idx : Block_IO.Data_Index_Type;
            begin
               if Primitive.Has_Tag_Write_Back (Prim) then
                  --
                  --  FIXME Accessing the Cache in this way could be dangerous
                  --  because the Cache is shared by the VBD as well as the FT.
                  --  If we would not suspend the VBD while doing the
                  --  write-back, another Request could evict the entry
                  --  belonging to the Index value and replace it.
                  --
                  --  (Since the Prim contains the PBA we could check the
                  --  validity of the index beforehand - but not storing the
                  --  index in the first place would be the preferable
                  --  solution.)
                  --
                  Block_IO.Submit_Primitive (
                     Obj.IO_Obj, Primitive.Tag_Free_Tree_WB, Prim, Data_Idx);

                  if Primitive.Operation (Prim) = Write then
                     IO_Buf (Data_Idx) :=
                        Obj.Cache_Data (Cache.Cache_Index_Type (Index));
                  end if;

               elsif Primitive.Has_Tag_IO (Prim) then
                  Block_IO.Submit_Primitive (
                     Obj.IO_Obj, Primitive.Tag_Free_Tree_IO, Prim, Data_Idx);

                  if Primitive.Operation (Prim) = Write then
                     IO_Buf (Data_Idx) :=
                        Obj.Free_Tree_Query_Data (Natural (Index));
                  end if;
               end if;
            end Declare_Index_1;
            Free_Tree.Drop_Generated_Primitive (Obj.Free_Tree_Obj, Prim);

         end Declare_Prim_2;
         Progress := True;

      end loop Loop_Free_Tree_Generated_Prims;

      ---------------------------------
      --  Put Request into splitter  --
      ---------------------------------

      --
      --  An arbitrary sized Block Request will be cut into Block_Size-sized
      --  primitves by the Splitter module.
      --
      Loop_Pool_Pending_Requests :
      loop
         Declare_Pool_Idx_Slot :
         declare
            Pool_Idx_Slot : constant Pool_Index_Slot_Type :=
               Pool.Peek_Pending_Request (Obj.Request_Pool_Obj);
         begin
            exit Loop_Pool_Pending_Requests when
               not Pool_Idx_Slot_Valid (Pool_Idx_Slot) or else
               not Splitter.Request_Acceptable (Obj.Splitter_Obj);

            Declare_Pool_Idx :
            declare
               Pool_Idx : constant Pool_Index_Type :=
                  Pool_Idx_Slot_Content (Pool_Idx_Slot);

               Req : constant Request.Object_Type :=
                  Pool.Request_For_Index (Obj.Request_Pool_Obj, Pool_Idx);

               Snap_ID : constant Snapshot_ID_Type :=
                  Pool.Snap_ID_For_Request (Obj.Request_Pool_Obj, Req);
            begin
               Pool.Drop_Pending_Request (Obj.Request_Pool_Obj);
               Splitter.Submit_Request (
                  Obj.Splitter_Obj, Pool_Idx, Req, Snap_ID);

            end Declare_Pool_Idx;
         end Declare_Pool_Idx_Slot;
         Progress := True;
      end loop Loop_Pool_Pending_Requests;

      --
      --  Give primitive to the translation module
      --
      Loop_Splitter_Generated_Prims :
      loop
         Declare_Prim_3 :
         declare
            Snap_Slot_Index : Snapshots_Index_Type;
            Prim : constant Primitive.Object_Type :=
               Splitter.Peek_Generated_Primitive (Obj.Splitter_Obj);
            Snap_ID : constant Snapshot_ID_Type :=
               Splitter.Peek_Generated_Primitive_ID (Obj.Splitter_Obj);
         begin
            exit Loop_Splitter_Generated_Prims when
               not Primitive.Valid (Prim) or else
               not Virtual_Block_Device.Primitive_Acceptable (Obj.VBD);

            if Obj.Secure_Superblock or else Obj.Creating_Snapshot then
               pragma Debug (Debug.Print_String ("Execute: "
                  & "creating snapshot or securing superblock in progress, "
                  & "inhibit Splitter"));
               exit Loop_Splitter_Generated_Prims;
            end if;

            Splitter.Drop_Generated_Primitive (Obj.Splitter_Obj);

            if Snap_ID /= 0 then
               Snap_Slot_Index := Snap_Slot_For_ID (Obj,
                  Generation_Type (Snap_ID));
            else
               Snap_Slot_Index := Curr_Snap (Obj);
            end if;

            pragma Debug (Debug.Print_String ("Execute: "
               & "submit VBD: "
               & "Snap_ID: " & Debug.To_String (Debug.Uint64_Type (Snap_ID))
               & " Prim: " & Primitive.To_String (Prim)
               & " Cur_SB: " & Debug.To_String (Debug.Uint64_Type (Obj.Cur_SB))
               & " Curr_Snap: " & Debug.To_String (Debug.Uint64_Type (
                  Obj.Superblocks (Obj.Cur_SB).Curr_Snap))
               & " PBA: " & Debug.To_String (Debug.Uint64_Type (
                  Obj.Superblocks (Obj.Cur_SB).Snapshots (
                     Snap_Slot_Index).PBA))
               & " Gen: "
               & Debug.To_String (Debug.Uint64_Type (
                  Obj.Superblocks (Obj.Cur_SB).Snapshots (
                     Snap_Slot_Index).Gen))
               & " "
               & Debug.To_String (
                  Obj.Superblocks (Obj.Cur_SB).Snapshots (
                     Snap_Slot_Index).Hash)));

            --
            --  For every new Request, we have to use the currlently active
            --  snapshot as a previous Request may have changed the tree.
            --
            Virtual_Block_Device.Submit_Primitive (
               Obj.VBD,
               Obj.Superblocks (Obj.Cur_SB).Snapshots (Snap_Slot_Index).PBA,
               Obj.Superblocks (Obj.Cur_SB).Snapshots (Snap_Slot_Index).Gen,
               Obj.Superblocks (Obj.Cur_SB).Snapshots (Snap_Slot_Index).Hash,
               Prim);

         end Declare_Prim_3;
         Progress := True;
      end loop Loop_Splitter_Generated_Prims;

      --------------------
      --  VBD handling  --
      --------------------

      --
      --  The VBD meta-module uses the Translation module internally and
      --  needs access to the Cache since it wants to use its Data.
      --  Because accessing a Cache entry will update its LRU value, the
      --  Cache must be mutable (that is also the reason we need the
      --  time object).
      --
      --
      --  (Basically the same issue regarding SPARK as the FT module...)
      --
      Virtual_Block_Device.Execute (
         Obj.VBD, Obj.Trans_Data, Obj.Cache_Obj, Obj.Cache_Data, Now);

      if Virtual_Block_Device.Execute_Progress (Obj.VBD) then
         Progress := True;
      end if;

      ------------------------------
      --  Cache_Flusher handling  --
      ------------------------------

      --
      --  The Cache_Flusher module is used to flush all dirty Cache entries
      --  to the block device and mark them as clean again. While the flusher
      --  is doing its work, all Cache entries should be locked, i.E., do not
      --  Cache an entry while its flushed - otherwise the change might not
      --  end up on the block device.
      --
      --  (For better or worse it is just a glorified I/O manager. At some
      --  point it should be better merged into the Cache module later on.)
      --

      --
      --  Mark the corresponding Cache entry as clean. If it was
      --  evicted in the meantime it will be ignored.
      --
      Loop_Cache_Flusher_Completed_Prims :
      loop
         Declare_Prim_4 :
         declare
            Prim : constant Primitive.Object_Type :=
               Cache_Flusher.Peek_Completed_Primitive (
                  Obj.Cache_Flusher_Obj);
         begin
            exit Loop_Cache_Flusher_Completed_Prims when
               not Primitive.Valid (Prim);

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            Cache.Mark_Clean (
               Obj.Cache_Obj,
               Physical_Block_Address_Type (
                  Primitive.Block_Number (Prim)));

            Cache_Flusher.Drop_Completed_Primitive (
               Obj.Cache_Flusher_Obj, Prim);

         end Declare_Prim_4;
         Progress := True;

      end loop Loop_Cache_Flusher_Completed_Prims;

      --
      --  Just pass the primitive on to the I/O module.
      --
      Loop_Cache_Flusher_Generated_Prims :
      loop

         Declare_Prim_5 :
         declare
            Prim : constant Primitive.Object_Type :=
               Cache_Flusher.Peek_Generated_Primitive (
                  Obj.Cache_Flusher_Obj);

            Data_Idx : Block_IO.Data_Index_Type;
         begin
            exit Loop_Cache_Flusher_Generated_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Block_IO.Submit_Primitive (
               Obj.IO_Obj, Primitive.Tag_Cache_Flush, Prim, Data_Idx);

            if Primitive.Operation (Prim) = Write then
               IO_Buf (Data_Idx) :=
                  Obj.Cache_Data (Cache_Flusher.Peek_Generated_Data_Index (
                     Obj.Cache_Flusher_Obj, Prim));
            end if;

            Cache_Flusher.Drop_Generated_Primitive (
               Obj.Cache_Flusher_Obj, Prim);

         end Declare_Prim_5;
         Progress := True;

      end loop Loop_Cache_Flusher_Generated_Prims;

      ---------------------------
      --  Write-back handling  --
      ---------------------------

      --
      --  The Write_Back module will store a changed branch including its leaf
      --  node on the block device.
      --
      --  The way it currently operates is as follows:
      --    1. (CRYPTO)   it hands the leaf Data to the Crypto module for
      --                  encryption
      --    2. (IO)       it hands the encrypted leaf Data to I/O module to
      --                  write it to the block device
      --    3. (CACHE)    starting by the lowest inner node it will update the
      --                  node entry (PBA and Hash)
      --    4. (COMPLETE) update root PBA and root Hash
      --

      Loop_WB_Completed_Prims :
      loop
         Declare_Prim_6 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Completed_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_WB_Completed_Prims when
               not Primitive.Valid (Prim);

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            Update_Snapshot_Hash (
               Obj.Write_Back_Obj,
               Obj.Cur_Gen,
               Obj.Superblocks (Obj.Cur_SB).Snapshots (Curr_Snap (Obj)),
               Prim);

            --
            --  We touched the super-block, either by updating a snapshot or by
            --  creating a new one - make sure it gets secured within the next
            --  interval.
            --
            Write_Back.Drop_Completed_Primitive (Obj.Write_Back_Obj, Prim);

            --
            --  Since the write Request is finally finished, all nodes stored
            --  at some place "save" (leafs on the block device, inner nodes
            --  within the Cache, acknowledge the primitive.
            --
            Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);

         end Declare_Prim_6;
         Progress := True;

         --
         --  FIXME stalling translation as long as the write-back takes places
         --        is not a good idea
         --
         Virtual_Block_Device.Trans_Resume_Translation (Obj.VBD);
         Obj.Stall_Snapshot_Creation := False;

      end loop Loop_WB_Completed_Prims;

      --
      --  Give the leaf Data to the Crypto module.
      --
      Loop_WB_Generated_Crypto_Prims :
      loop

         Declare_Prim_7 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Generated_Crypto_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_WB_Generated_Crypto_Prims when
               not Primitive.Valid (Prim) or else
               not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);

            --
            --  The Data will be copied into the Crypto module's internal
            --  buffer
            --
            Declare_Crypto_Data :
            declare
               Plain_Data_Index : constant Write_Back.Data_Index_Type :=
                  Write_Back.Peek_Generated_Crypto_Data (
                     Obj.Write_Back_Obj, Prim);

               Data_Idx : Crypto.Item_Index_Type;
            begin
               Crypto.Submit_Primitive (Obj.Crypto_Obj, Prim, Data_Idx);
               Crypto_Plain_Buf (Data_Idx) :=
                  Obj.Write_Back_Data (Plain_Data_Index);

            end Declare_Crypto_Data;
            Write_Back.Drop_Generated_Crypto_Primitive (
               Obj.Write_Back_Obj, Prim);

         end Declare_Prim_7;
         Progress := True;

      end loop Loop_WB_Generated_Crypto_Prims;

      --
      --  Pass the encrypted leaf Data to the I/O module.
      --
      Loop_WB_Generated_IO_Prims :
      loop
         Declare_Prim_8 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Generated_IO_Primitive (Obj.Write_Back_Obj);

            Data_Idx : Block_IO.Data_Index_Type;
         begin
            exit Loop_WB_Generated_IO_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Block_IO.Submit_Primitive (
               Obj.IO_Obj, Primitive.Tag_Write_Back, Prim, Data_Idx);

            if Primitive.Operation (Prim) = Write then
               IO_Buf (Data_Idx) :=
                  Obj.Write_Back_Data (Write_Back.Peek_Generated_IO_Data (
                     Obj.Write_Back_Obj, Prim));
            end if;

            Write_Back.Drop_Generated_IO_Primitive (
               Obj.Write_Back_Obj, Prim);

         end Declare_Prim_8;
         Progress := True;

      end loop Loop_WB_Generated_IO_Prims;

      --
      --  Update the inner nodes of the tree. This is always done after the
      --  encrypted leaf node was stored by the I/O module.
      --
      Loop_WB_Generated_Cache_Prims :
      loop

         Declare_Prim_9 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Generated_Cache_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_WB_Generated_Cache_Prims when
               not Primitive.Valid (Prim);

            Declare_PBAs :
            declare
               PBA : constant Physical_Block_Address_Type :=
                  Physical_Block_Address_Type (
                     Primitive.Block_Number (Prim));

               Update_PBA : constant Physical_Block_Address_Type :=
                  Write_Back.Peek_Generated_Cache_Update_PBA (
                     Obj.Write_Back_Obj, Prim);

               Cache_Miss : Boolean := False;
            begin

               --
               --  Check if the Cache contains the needed entries. In case of
               --  the of the old node's block that is most likely. The new
               --  one, if there is one (that happens when the inner nodes are
               --  Obj.Not_ updated in place, might not be in the Cache -
               --  check and Request both.
               --
               if not Cache.Data_Available (Obj.Cache_Obj, PBA) then
                  if Cache.Request_Acceptable (Obj.Cache_Obj, PBA) then
                     Cache.Submit_Request (Obj.Cache_Obj, PBA);
                  end if;
                  Cache_Miss := True;
               end if;

               if PBA /= Update_PBA then
                  if not Cache.Data_Available (Obj.Cache_Obj, Update_PBA) then
                     if Cache.Request_Acceptable (Obj.Cache_Obj, Update_PBA)
                     then
                        Cache.Submit_Request (Obj.Cache_Obj, Update_PBA);
                     end if;
                     Cache_Miss := True;
                  end if;
               end if;

               --  read the needed blocks first
               if Cache_Miss then
                  exit Loop_WB_Generated_Cache_Prims;
               end if;

               Write_Back.Drop_Generated_Cache_Primitive (
                  Obj.Write_Back_Obj, Prim);

               --
               --  To keep it simply, always set both properly - even if
               --  the old and new node are the same.
               --
               Declare_Indices :
               declare
                  Index        : Cache.Cache_Index_Type;
                  Update_Index : Cache.Cache_Index_Type;
               begin
                  Cache.Data_Index (Obj.Cache_Obj, PBA, Now, Index);
                  Cache.Data_Index (
                     Obj.Cache_Obj, Update_PBA, Now, Update_Index);

                  --
                  --  FIXME We copy the data to the stack first and back to the
                  --  cache afterwards so gnatprove
                  --  does not complain that 'formal parameters "Data" and
                  --  "Update_Data" might be aliased (SPARK RM 6.4.2)' for the
                  --  call to Write_Back.Update. This might be avoided by using
                  --  assertions about the data indices.
                  --
                  Declare_Update_Data : declare
                     Data : constant Block_Data_Type :=
                        Obj.Cache_Data (Index);

                     Update_Data : Block_Data_Type :=
                        Obj.Cache_Data (Update_Index);
                  begin
                     --
                     --  (Later on we can remove the tree_Helper here as the
                     --  outer degree, which is used to calculate the entry in
                     --  the inner node from the VBA is set at compile-time.)
                     --
                     Write_Back.Update (
                        Obj.Write_Back_Obj,
                        PBA, Virtual_Block_Device.Get_Tree_Helper (Obj.VBD),
                        Data, Update_Data);

                     Obj.Cache_Data (Update_Index) := Update_Data;
                  end Declare_Update_Data;

                  --
                  --  Make the potentially new entry as dirty so it gets
                  --  flushed next time
                  --
                  Cache.Mark_Dirty (Obj.Cache_Obj, Update_PBA);

               end Declare_Indices;
            end Declare_PBAs;

         end Declare_Prim_9;
         Progress := True;

      end loop Loop_WB_Generated_Cache_Prims;

      ----------------------------
      --  Super-block handling  --
      ----------------------------

      --
      --  Store the current generation in the current
      --  super-block before it gets secured.
      --
      if
         Obj.Secure_Superblock and then
         Sync_Superblock.Request_Acceptable (Obj.Sync_SB_Obj)
      then
         Obj.Superblocks (Obj.Cur_SB).Last_Secured_Generation :=
            Obj.Cur_Gen;

         Sync_Superblock.Submit_Request (
            Obj.Sync_SB_Obj, Obj.Cur_SB, Obj.Cur_Gen);
      end if;

      --
      --  When the current super-block was secured, select the next one.
      --
      Loop_Sync_SB_Completed_Prims :
      loop
         Declare_Prim_10 :
         declare
            Prim : constant Primitive.Object_Type :=
               Sync_Superblock.Peek_Completed_Primitive (Obj.Sync_SB_Obj);
         begin
            exit Loop_Sync_SB_Completed_Prims when
               not Primitive.Valid (Prim);

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            Declare_Next_SB :
            declare
               Next_SB : constant Superblocks_Index_Type := (
                  if Obj.Cur_SB < Superblocks_Index_Type'Last then
                     Obj.Cur_SB + 1
                  else
                     Superblocks_Index_Type'First);
            begin
               Obj.Superblocks (Next_SB) :=
                  Obj.Superblocks (Obj.Cur_SB);

               --  handle state
               Obj.Cur_SB                  := Next_SB;
               Obj.Last_Secured_Generation :=
                  Sync_Superblock.Peek_Completed_Generation (
                     Obj.Sync_SB_Obj, Prim);

               --
               --  Look for a new snapshot slot. If we cannot find one
               --  we manual intervention b/c there are too many snapshots
               --  flagged as keep
               --
               Declare_Next_Snap_2 :
               declare
                  Next_Snap : constant Snapshots_Index_Type :=
                     Next_Snap_Slot (Obj);
               begin

                  --
                  --  Could not find free slots, we need to discard some
                  --  quarantine snapshots, user intervention needed.
                  --
                  if Next_Snap = Curr_Snap (Obj) then
                     raise Program_Error;
                  end if;

                  Declare_Tree :
                  declare
                     Tree : constant Tree_Helper.Object_Type :=
                        Virtual_Block_Device.Get_Tree_Helper (Obj.VBD);
                  begin
                     Obj.Superblocks (Obj.Cur_SB).Snapshots (
                        Next_Snap).Height := Tree_Helper.Height (Tree);
                     Obj.Superblocks (Obj.Cur_SB).Snapshots (
                        Next_Snap).Nr_Of_Leafs := Tree_Helper.Leafs (Tree);
                  end Declare_Tree;

                  Obj.Superblocks (Obj.Cur_SB).Snapshots (Next_Snap) :=
                  Obj.Superblocks (Obj.Cur_SB).Snapshots (Curr_Snap (Obj));
                  Obj.Superblocks (Obj.Cur_SB).Snapshots (
                     Next_Snap).Flags := 0;

                  --  Obj.Superblocks (Obj.Cur_SB).Snapshots (
                  --     Next_Snap).Gen := Obj.Cur_Gen;
                  Obj.Superblocks (Obj.Cur_SB).Snapshots (Next_Snap).ID :=
                     Snapshot_ID_Storage_Type (Obj.Next_Snapshot_ID);

                  Obj.Superblocks (Obj.Cur_SB).Snapshots (
                     Next_Snap).Valid := 1;
                  Obj.Superblocks (Obj.Cur_SB).Curr_Snap :=
                     Snapshots_Index_Storage_Type (Next_Snap);
               end Declare_Next_Snap_2;

               Obj.Cur_Gen := Obj.Cur_Gen  + 1;
               Obj.Creating_Snapshot := False;
               Obj.Secure_Superblock := False;

               pragma Debug (
                  Debug.Dump_Current_Superblock (Obj.Superblocks, Obj.Cur_SB));

            end Declare_Next_SB;
            Sync_Superblock.Drop_Completed_Primitive (Obj.Sync_SB_Obj, Prim);

         end Declare_Prim_10;
         Progress := True;

      end loop Loop_Sync_SB_Completed_Prims;

      --
      --  Use I/O module to write super-block to the block device.
      --
      Loop_Sync_SB_Generated_Prims :
      loop
         Declare_Prim_11 :
         declare
            Prim : constant Primitive.Object_Type :=
               Sync_Superblock.Peek_Generated_Primitive (Obj.Sync_SB_Obj);
         begin
            exit Loop_Sync_SB_Generated_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Declare_SB_Data :
            declare
               SB_Data : Block_Data_Type;
               Data_Idx : Block_IO.Data_Index_Type;
            begin
               Block_Data_From_Superblock (
                  SB_Data, Obj.Superblocks (
                     Sync_Superblock.Peek_Generated_Index (
                        Obj.Sync_SB_Obj, Prim)));

               Block_IO.Submit_Primitive (
                  Obj.IO_Obj, Primitive.Tag_Sync_SB, Prim, Data_Idx);

               if Primitive.Operation (Prim) = Write then
                  IO_Buf (Data_Idx) := SB_Data;
               end if;
            end Declare_SB_Data;
            Sync_Superblock.Drop_Generated_Primitive (
               Obj.Sync_SB_Obj, Prim);

         end Declare_Prim_11;
         Progress := True;

      end loop Loop_Sync_SB_Generated_Prims;

      -----------------------
      --  Crypto handling  --
      -----------------------

      --
      --  The Crypto module has its own internal buffer, Data has to be
      --  copied in and copied out.
      --

      --
      --  Only writes primitives (encrypted Data) are handled here,
      --  read primitives (decrypred Data) are handled in 'give_Read_Data'.
      --
      Loop_Crypto_Completed_Prims :
      loop
         Declare_Prim_12 :
         declare
            Prim : constant Primitive.Object_Type :=
               Crypto.Peek_Completed_Primitive (Obj.Crypto_Obj);
         begin
            exit Loop_Crypto_Completed_Prims when
               not Primitive.Valid (Prim) or else
               Primitive.Operation (Prim) = Read;

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            Declare_Index_2 :
            declare
               Index : constant Write_Back.Data_Index_Type :=
                  Write_Back.Peek_Generated_Crypto_Data (
                     Obj.Write_Back_Obj, Prim);
            begin
               --
               --  FIXME instead of copying the Data just ask the crypto
               --        module for the resulting Hash and omit further
               --        processing in case the operation failed
               --
               Obj.Write_Back_Data (Index) := Crypto_Cipher_Buf (
                  Crypto.Data_Index (Obj.Crypto_Obj, Prim));

               Write_Back.Mark_Completed_Crypto_Primitive (
                  Obj.Write_Back_Obj, Prim, Obj.Write_Back_Data (Index));

            end Declare_Index_2;
            Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj);

         end Declare_Prim_12;
         Progress := True;

      end loop Loop_Crypto_Completed_Prims;

      ----------------------
      --  Cache handling  --
      ----------------------

      --
      --  Pass the Data used by the module in by reference so that it
      --  can be shared by the other modules. The method will internally
      --  copy read job Data into the chosen entry. In doing so it might
      --  evict an already populated entry.
      --
      Cache.Fill_Cache (
         Obj.Cache_Obj, Obj.Cache_Data, Obj.Cache_Job_Data, Now);

      if Cache.Execute_Progress (Obj.Cache_Obj) then
         Progress := True;
      end if;

      --
      --  Read Data from the block device to fill the Cache.
      --
      --  (The Cache module has no 'peek_Completed_Primitive ()' method,
      --  all modules using the Cache have to poll and might be try to
      --  submit the same Request multiple times (see its acceptable
      --  method). It makes sense to change the Cache module so that it
      --  works the rest of modules. That would require restructing
      --  the modules, though.)
      --
      Loop_Cache_Generated_Prims :
      loop
         Declare_Prim_14 :
         declare
            Prim : constant Primitive.Object_Type :=
               Cache.Peek_Generated_Primitive (Obj.Cache_Obj);

            Data_Idx : Block_IO.Data_Index_Type;
         begin
            exit Loop_Cache_Generated_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Block_IO.Submit_Primitive (
               Obj.IO_Obj, Primitive.Tag_Cache, Prim, Data_Idx);

            if Primitive.Operation (Prim) = Write then
               IO_Buf (Data_Idx) := Obj.Cache_Job_Data (
                  Cache.Cache_Job_Index_Type (
                     Cache.Peek_Generated_Data_Index (Obj.Cache_Obj, Prim)));
            end if;

            Cache.Drop_Generated_Primitive (Obj.Cache_Obj, Prim);

         end Declare_Prim_14;
         Progress := True;

      end loop Loop_Cache_Generated_Prims;

      --------------------
      --  I/O handling  --
      --------------------

      --
      --  This module handles all the block backend I/O and has to
      --  work with all most all modules. IT uses the 'Tag' field
      --  to differentiate the modules.
      --

      Loop_IO_Completed_Prims :
      loop
         Declare_Prim_15 :
         declare
            Prim : constant Primitive.Object_Type :=
               Block_IO.Peek_Completed_Primitive (Obj.IO_Obj);
         begin
            exit Loop_IO_Completed_Prims when not Primitive.Valid (Prim);

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            Declare_Index_3 :
            declare
               Index : constant Block_IO.Data_Index_Type :=
                  Block_IO.Peek_Completed_Data_Index (Obj.IO_Obj);

               --
               --  Whenever we cannot hand a successful primitive over
               --  to the corresponding module, leave the loop but keep
               --  the completed primitive so that it might be processed
               --  next time.
               --
               Mod_Progress : Boolean := True;
            begin
               if Primitive.Has_Tag_Decrypt (Prim) then

                  if not Crypto.Primitive_Acceptable (Obj.Crypto_Obj) then
                     Mod_Progress := False;
                  else
                     Declare_Data :
                     declare
                        Data_Idx : Crypto.Item_Index_Type;
                     begin
                        --
                        --  Having to override the Tag is needed because of
                        --  the way the Crypto module is hooked up in the
                        --  overall Data flow. Since it is the one that
                        --  acknowledges the primitive to the pool in the read
                        --  case, we have to use the Tag the pool module uses.
                        --
                        Crypto.Submit_Primitive (
                           Obj.Crypto_Obj,
                           Primitive.Copy_Valid_Object_New_Tag (
                              Prim,
                              Block_IO.Peek_Completed_Tag (
                                 Obj.IO_Obj, Prim)),
                           Data_Idx);

                        Crypto_Cipher_Buf (Data_Idx) := IO_Buf (Index);

                     end Declare_Data;
                  end if;

               elsif Primitive.Has_Tag_Cache (Prim) then
                  --
                  --  FIXME we need a proper method for getting the right
                  --        Cache job Data index, for now rely on the
                  --        knowledge that there is only one item
                  --
                  Obj.Cache_Job_Data (0) := IO_Buf (Index);
                  Cache.Mark_Completed_Primitive (Obj.Cache_Obj, Prim);

               elsif Primitive.Has_Tag_Cache_Flush (Prim) then
                  Cache_Flusher.Mark_Generated_Primitive_Complete (
                     Obj.Cache_Flusher_Obj, Prim);

               elsif Primitive.Has_Tag_Write_Back (Prim) then
                  Write_Back.Mark_Completed_IO_Primitive (
                     Obj.Write_Back_Obj, Prim);

               elsif Primitive.Has_Tag_Sync_SB (Prim) then
                  Sync_Superblock.Mark_Generated_Primitive_Complete (
                     Obj.Sync_SB_Obj, Prim);

               elsif Primitive.Has_Tag_Free_Tree_WB (Prim) then
                  Free_Tree.Mark_Generated_Primitive_Complete (
                     Obj.Free_Tree_Obj,
                     Primitive.Copy_Valid_Object_New_Tag (
                        Prim, Primitive.Tag_Write_Back));

               elsif Primitive.Has_Tag_Free_Tree_IO (Prim) then

                  --
                  --  FIXME we need a proper method for getting the right query
                  --       Data index, for now rely on the knowledge that there
                  --       is only one item
                  --
                  Obj.Free_Tree_Query_Data (0) := IO_Buf (Index);
                  Free_Tree.Mark_Generated_Primitive_Complete (
                     Obj.Free_Tree_Obj,
                     Primitive.Copy_Valid_Object_New_Tag (
                        Prim, Primitive.Tag_IO));
               end if;
               exit Loop_IO_Completed_Prims when not Mod_Progress;

            end Declare_Index_3;
            Block_IO.Drop_Completed_Primitive (Obj.IO_Obj, Prim);

         end Declare_Prim_15;
         Progress := True;

      end loop Loop_IO_Completed_Prims;

      --
      --  Submit Block-IO read-primitives for completed primitives of the VBD
      --
      Loop_VBD_Completed_Prims :
      loop
         declare
            Prim : constant Primitive.Object_Type :=
               Virtual_Block_Device.Peek_Completed_Primitive (Obj.VBD);
         begin
            exit Loop_VBD_Completed_Prims when
               not Primitive.Valid (Prim) or else
               Primitive.Operation (Prim) /= Read or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Block_IO.Submit_Primitive_Dont_Return_Index (
               Obj.IO_Obj, Primitive.Tag_Decrypt, Prim);

            Virtual_Block_Device.Drop_Completed_Primitive (Obj.VBD);
            Progress := True;
         end;
      end loop Loop_VBD_Completed_Prims;
      Obj.Execute_Progress := Progress;

   end Execute;

   function Client_Request_Acceptable (Obj : Object_Type)
   return Boolean
   is (Pool.Request_Acceptable (Obj.Request_Pool_Obj));

   procedure Submit_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type;
      ID  :        Snapshot_ID_Type)
   is
   begin
      Pool.Submit_Request (
         Obj.Request_Pool_Obj,
         Req,
         ID,
         Splitter.Number_Of_Primitives (Req));
   end Submit_Client_Request;

   function Peek_Completed_Client_Request (Obj : Object_Type)
   return Request.Object_Type
   is (Pool.Peek_Completed_Request (Obj.Request_Pool_Obj));

   procedure Drop_Completed_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      Pool.Drop_Completed_Request (Obj.Request_Pool_Obj, Req);
   end Drop_Completed_Client_Request;

   --
   --  For now there can be only one Request pending.
   --
   function Front_End_Busy_With_Other_Request (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Boolean
   is (not Request.Equal (Obj.Wait_For_Front_End.Req, Req));

   procedure Has_IO_Request (
      Obj      : in out Object_Type;
      Req      :    out Request.Object_Type;
      Data_Idx :    out Block_IO.Data_Index_Type)
   is
   begin
      Req      := Request.Invalid_Object;
      Data_Idx := 0;

      if Primitive.Valid (Obj.Wait_For_Back_End.Prim) then
         return;
      end if;

      declare
         Prim : constant Primitive.Object_Type :=
            Block_IO.Peek_Generated_Primitive (Obj.IO_Obj);
      begin
         if Primitive.Valid (Prim) then
            Obj.Wait_For_Back_End := (
               Req => Request.Valid_Object (
                  Op     => Primitive.Operation (Prim),
                  Succ   => False,
                  Blk_Nr => Primitive.Block_Number (Prim),
                  Off    => 0,
                  Cnt    => 1,
                  Tg     => 0),
               Prim        => Prim,
               Event       => Event_IO_Request_Completed,
               In_Progress => False);

            Data_Idx := Block_IO.Peek_Generated_Data_Index (Obj.IO_Obj, Prim);
            Req      := Obj.Wait_For_Back_End.Req;
         end if;
      end;
   end Has_IO_Request;

   procedure IO_Request_In_Progress (
      Obj      : in out Object_Type;
      Data_Idx :        Block_IO.Data_Index_Type)
   is
   begin
      if Obj.Wait_For_Back_End.In_Progress or else
         Obj.Wait_For_Back_End.Event /= Event_IO_Request_Completed
      then
         raise Program_Error;
      end if;
      Block_IO.Drop_Generated_Primitive_2 (Obj.IO_Obj, Data_Idx);
      Obj.Wait_For_Back_End.In_Progress := True;
   end IO_Request_In_Progress;

   procedure IO_Request_Completed (
      Obj        : in out Object_Type;
      Data_Index :        Block_IO.Data_Index_Type;
      Success    :        Boolean)
   is
   begin
      if not Obj.Wait_For_Back_End.In_Progress or else
         Obj.Wait_For_Back_End.Event /= Event_IO_Request_Completed
      then
         raise Program_Error;
      end if;
      Block_IO.Mark_Generated_Primitive_Complete (
         Obj.IO_Obj, Data_Index, Success);

      Obj.Wait_For_Back_End := Wait_For_Event_Invalid;
   end IO_Request_Completed;

   procedure Client_Data_Ready (
      Obj : in out Object_Type;
      Req :    out Request.Object_Type)
   is
   begin
      Req := Request.Invalid_Object;

      if Primitive.Valid (Obj.Wait_For_Front_End.Prim) then
         return;
      end if;

      --
      --  When it was a read Request, we need the location to
      --  where the Crypto should copy the decrypted data.
      --
      declare
         Prim : constant Primitive.Object_Type :=
            Crypto.Peek_Completed_Primitive (Obj.Crypto_Obj);
      begin
         if
            Primitive.Valid (Prim) and then
            Primitive.Operation (Prim) = Read
         then
            Start_Waiting_For_Front_End (Obj, Prim, Event_Obtain_Client_Data);
            Req := Obj.Wait_For_Front_End.Req;
            return;
         end if;
      end;
   end Client_Data_Ready;

   function Client_Data_Index (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Primitive.Index_Type
   is
   begin
      if Front_End_Busy_With_Other_Request (Obj, Req) then
         return Primitive.Invalid_Index;
      end if;

      return Primitive.Index (Obj.Wait_For_Front_End.Prim);
   end Client_Data_Index;

   procedure Obtain_Client_Data (
      Obj              : in out Object_Type;
      Req              :        Request.Object_Type;
      Data_Index       :    out Crypto.Plain_Buffer_Index_Type;
      Data_Index_Valid :    out Boolean)
   is
      Prim  : constant Primitive.Object_Type := Obj.Wait_For_Front_End.Prim;
      Event : constant Event_Type            := Obj.Wait_For_Front_End.Event;
   begin
      Data_Index_Valid := False;
      Data_Index       := Crypto.Plain_Buffer_Index_Type'First;

      if Front_End_Busy_With_Other_Request (Obj, Req) or else
         Event /= Event_Obtain_Client_Data
      then
         return;
      end if;

      Data_Index := Crypto.Plain_Buffer_Index_Type (
         Crypto.Data_Index (Obj.Crypto_Obj, Prim));

      Data_Index_Valid := True;
      Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj);
      Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);
      Obj.Wait_For_Front_End := Wait_For_Event_Invalid;
   end Obtain_Client_Data;

   procedure Start_Waiting_For_Front_End (
      Obj   : in out Object_Type;
      Prim  :        Primitive.Object_Type;
      Event :        Event_Type)
   is
   begin
      Obj.Wait_For_Front_End := (
         Req         =>
            Pool.Request_For_Index (
               Obj.Request_Pool_Obj,
               Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim))),
         Prim        => Prim,
         Event       => Event,
         In_Progress => False);
   end Start_Waiting_For_Front_End;

   --
   --  FIXME move Wait_For_Front_End allocation into execute,
   --       turn procedure into function
   --
   procedure Client_Data_Required (
      Obj : in out Object_Type;
      Req :    out Request.Object_Type)
   is
   begin
      Req := Request.Invalid_Object;

      if Primitive.Valid (Obj.Wait_For_Front_End.Prim) then
         return;
      end if;

      --
      --  A write Request, we need the location from where to read the new
      --  leaf data.
      --
      declare
         Prim : constant Primitive.Object_Type :=
            Virtual_Block_Device.Peek_Completed_Primitive (Obj.VBD);
      begin
         if Primitive.Valid (Prim) and then Primitive.Operation (Prim) = Write
         then
            Start_Waiting_For_Front_End (
               Obj, Prim, Event_Supply_Client_Data_After_VBD);
            Req := Obj.Wait_For_Front_End.Req;
            return;
         end if;
      end;

      --
      --  The free-tree needs the data to give to the Write_Back module.
      --
      declare
         Prim : constant Primitive.Object_Type :=
            Free_Tree.Peek_Completed_Primitive (Obj.Free_Tree_Obj);
      begin
         if Primitive.Valid (Prim) and then Primitive.Success (Prim) then
            Start_Waiting_For_Front_End (
               Obj, Prim, Event_Supply_Client_Data_After_FT);
            Req := Obj.Wait_For_Front_End.Req;
            return;
         end if;
      end;
   end Client_Data_Required;

   procedure Supply_Client_Data (
      Obj      : in out Object_Type;
      Now      :        Timestamp_Type;
      Req      :        Request.Object_Type;
      Data     :        Block_Data_Type;
      Progress :    out Boolean)
   is
      Prim : constant Primitive.Object_Type := Obj.Wait_For_Front_End.Prim;
   begin
      Progress := False;

      --
      --  For now there is only one Request pending.
      --
      if not Request.Equal (Obj.Wait_For_Front_End.Req, Req) then
         return;
      end if;

      if Obj.Wait_For_Front_End.Event = Event_Supply_Client_Data_After_FT then

         if not Write_Back.Primitive_Acceptable (Obj.Write_Back_Obj) then
            return;
         end if;

         Obj.Free_Tree_Retry_Count := 0;

         --
         --  Accessing the write-back data in this manner is still a shortcut
         --  and probably will not work with SPARK - we have to get rid of
         --  the 'Block_Data' pointer.
         --
         declare
            WB : constant Free_Tree.Write_Back_Data_Type :=
               Free_Tree.Peek_Completed_WB_Data (Obj.Free_Tree_Obj, Prim);
         begin

            Write_Back.Submit_Primitive (
               Obj.Write_Back_Obj,
               WB.Prim, WB.Gen, WB.VBA, WB.New_PBAs, WB.Old_PBAs,
               Tree_Level_Index_Type (WB.Tree_Height),
               Data, Obj.Write_Back_Data);
         end;

         Free_Tree.Drop_Completed_Primitive (Obj.Free_Tree_Obj, Prim);

         --  XXX check if default constructor produces invalid object
         Obj.Wait_For_Front_End := Wait_For_Event_Invalid;
         Progress := True;
         return;

      --
      --  The VBD module translated a write Request, writing the data
      --  now to disk involves multiple steps:
      --
      --  1. Gathering of all nodes in the branch and looking up the
      --     volatile ones (those, which belong to theCurr generation
      --     and will be updated in place).
      --  2. Allocate new blocks if needed by consulting the FT
      --  3. Updating all entries in the nodes
      --  4. Writing the branch back to the block device.
      --
      --  Those steps are handled by different modules, depending on
      --  the allocation of new blocks.
      --
      elsif
         Obj.Wait_For_Front_End.Event = Event_Supply_Client_Data_After_VBD
      then

         --
         --  As usual check first we can submit new requests.
         --
         if not Free_Tree.Request_Acceptable (Obj.Free_Tree_Obj) or else
            not Virtual_Block_Device.Trans_Can_Get_Type_1_Info (Obj.VBD, Prim)
         then
            return;
         end if;

         --
         --  Then (ab-)use the Translation module and its still pending
         --  Request to get all old PBAs, whose generation we then check.
         --  The order of the array items corresponds to the level within
         --  the tree.
         --
         Declare_Old_PBAs : declare

            Old_PBAs : Type_1_Node_Infos_Type := (
               others => Type_1_Node_Info_Invalid);

            Trans_Height : constant Tree_Level_Type :=
               Virtual_Block_Device.Tree_Height (Obj.VBD) + 1;

            Snap : constant Snapshot_Type :=
               Obj.Superblocks (Obj.Cur_SB).Snapshots (Curr_Snap (Obj));

            --
            --  The array of new_PBA will either get populated from the Old_PBA
            --  content or from newly allocated blocks.
            --  The order of the array items corresponds to the level within
            --  the tree.
            --
            New_PBAs   : Write_Back.New_PBAs_Type := (others => 0);
            New_Blocks : Number_Of_Blocks_Type := 0;

            --
            --  This array contains all blocks that will get freed or rather
            --  marked as reserved in the FT as they are still referenced by
            --  an snapshot.
            --
            Free_PBAs   : Free_Tree.Free_PBAs_Type := (others => 0);
            Free_Blocks : Tree_Level_Index_Type := 0;

            --
            --  Get the corresponding VBA that we use to calculate the index
            --  for the edge in the node for a given level within the tree.
            --
            VBA : constant Virtual_Block_Address_Type :=
               Virtual_Block_Address_Type (
                  Virtual_Block_Device.Trans_Get_Virtual_Block_Address (
                     Obj.VBD, Prim));
         begin

            Virtual_Block_Device.Trans_Get_Type_1_Info (Obj.VBD, Old_PBAs);

            --
            --  Make sure we work with the proper snapshot.
            --
            --  (This check may be removed at some point.)
            --
            if Old_PBAs (Natural (Trans_Height - 1)).PBA /= Snap.PBA then
               raise Program_Error;
            end if;

            --
            --  Here only the inner nodes, i.E. all nodes excluding root and
            --  leaf, are considered. The root node is checked afterwards as
            --  we need the information of theCurr snapshot for that.
            --
            for I in 1 .. Trans_Height - 1 loop

               --
               --  Use the old PBA to get the node's data from the cache and
               --  use it check how we have to handle the node.
               --
               Declare_Nodes :
               declare
                  PBA : constant Physical_Block_Address_Type :=
                     Old_PBAs (Natural (I)).PBA;

                  Cache_Idx : Cache.Cache_Index_Type;
                  Nodes : Type_I_Node_Block_Type;
               begin
                  Cache.Data_Index (Obj.Cache_Obj, PBA, Now, Cache_Idx);
                  Type_I_Node_Block_From_Block_Data (
                     Nodes, Obj.Cache_Data (Cache_Idx));

                  Declare_Generation :
                  declare
                     Child_Idx : constant Tree_Child_Index_Type :=
                        Virtual_Block_Device.Index_For_Level (Obj.VBD, VBA, I);

                     Gen : constant Generation_Type :=
                        Nodes (Natural (Child_Idx)).Gen;
                  begin
                     --
                     --  In case the generation of the entry is the same as the
                     --  Curr generation OR if the generation is 0 (which means
                     --  it was never used before) the block is volatile and we
                     --  change it in place and store it directly in the
                     --  new_PBA array.
                     --
                     pragma Debug (Debug.Print_String ("PBA: "
                        & Debug.To_String (Debug.Uint64_Type (PBA)) & " "
                        & "Gen: "
                        & Debug.To_String (Debug.Uint64_Type (Gen)) & " "
                        & "Cur_Gen: "
                        & Debug.To_String (Debug.Uint64_Type (Obj.Cur_Gen))
                        & " Npba: "
                        & Debug.To_String (Debug.Uint64_Type (
                           Nodes (Natural (Child_Idx)).PBA))));
                     if Gen = Obj.Cur_Gen or else Gen = 0 then

                        New_PBAs (Tree_Level_Index_Type (I - 1)) :=
                           Old_PBAs (Natural (I - 1)).PBA;

                     --
                     --  Otherwise add the block to the free_PBA array so that
                     --  the FT will reserved it and note that we need another
                     --  new block.
                     --
                     else
                        Free_PBAs (Free_Blocks) :=
                           Old_PBAs (Natural (I - 1)).PBA;

                        Free_Blocks := Free_Blocks + 1;
                        New_Blocks  := New_Blocks  + 1;

                        pragma Debug (Debug.Print_String ("New_Blocks: "
                           & Debug.To_String (Debug.Uint64_Type (New_Blocks))
                           & " Free_PBA: "
                           & Debug.To_String (Debug.Uint64_Type (
                              Free_PBAs (Free_Blocks))) & " "
                           & Debug.To_String (Debug.Uint64_Type (
                              Old_PBAs (Natural (I - 1)).PBA))));
                     end if;
                  end Declare_Generation;
               end Declare_Nodes;
            end loop;

            pragma Debug (Debug.Print_String ("Snap.Gen: "
               & Debug.To_String (Debug.Uint64_Type (Snap.Gen)) & " "
               & "Cur_Gen: "
               & Debug.To_String (Debug.Uint64_Type (Obj.Cur_Gen))
               & " root PBA: "
               & Debug.To_String (Debug.Uint64_Type (
                  Old_PBAs (Natural (Trans_Height - 1)).PBA))
               & " Gen: "
               & Debug.To_String (Debug.Uint64_Type (
                  Old_PBAs (Natural (Trans_Height - 1)).Gen))));

            --  check root node
            if Old_PBAs (Natural (Trans_Height - 1)).Gen = Obj.Cur_Gen or else
               Snap.Gen = 0
            then
               pragma Debug (Debug.Print_String ("Change root PBA in place"));
               New_PBAs (Tree_Level_Index_Type (Trans_Height - 1)) :=
                  Old_PBAs (Natural (Trans_Height - 1)).PBA;
            else
               pragma Debug (Debug.Print_String ("New root PBA"));
               Free_PBAs (Free_Blocks) :=
                  Old_PBAs (Natural (Trans_Height - 1)).PBA;
               New_Blocks := New_Blocks  + 1;
            end if;

            --
            --  Since there are blocks we cannot change in place, use the
            --  FT module to allocate the blocks. As we have to reserve
            --  the blocks we implicitly will free (free_PBA items), pass
            --  on theCurr generation.
            --
            if New_Blocks > 0 then
               Free_Tree.Submit_Request (
                  Obj         => Obj.Free_Tree_Obj,
                  Curr_Gen    => Obj.Cur_Gen,
                  Nr_Of_Blks  => New_Blocks,
                  New_PBAs    => New_PBAs,
                  Old_PBAs    => Old_PBAs,
                  Tree_Height => Trans_Height,
                  Fr_PBAs     => Free_PBAs,
                  Req_Prim    => Prim,
                  VBA         => VBA);
            else
               --
               --  The complete branch is still part of theCurr generation,
               --  call the Write_Back module directly.
               --
               --  (We would have to check if the module can acutally accept
               --  the Request...)
               --
               Write_Back.Submit_Primitive (
                  Obj      => Obj.Write_Back_Obj,
                  Prim     => Prim,
                  Gen      => Obj.Cur_Gen,
                  VBA      => VBA,
                  New_PBAs => New_PBAs,
                  Old_PBAs => Old_PBAs,
                  N        => Tree_Level_Index_Type (Trans_Height),
                  Data     => Data,
                  WB_Data  => Obj.Write_Back_Data);
            end if;

            Virtual_Block_Device.Drop_Completed_Primitive (Obj.VBD);

            Obj.Wait_For_Front_End := Wait_For_Event_Invalid;

            --
            --  Inhibit translation which effectively will suspend the
            --  Translation modules operation and will stall all other
            --  pending requests to make sure all following Request will
            --  use the newest tree.
            --
            --  (It stands to reasons whether we can remove this check
            --  if we make sure that only the requests belonging to
            --  the same branch are serialized.)
            --
            Virtual_Block_Device.Trans_Inhibit_Translation (Obj.VBD);
            Obj.Stall_Snapshot_Creation := True;
            Progress := True;
            return;
         end Declare_Old_PBAs;
      end if;
   end Supply_Client_Data;

   procedure Crypto_Cipher_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Plain_Buffer_Index_Type)
   is
      Item_Index : Crypto.Item_Index_Type;
      Prim       : Primitive.Object_Type;
   begin
      Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj,  Item_Index, Prim);
      Data_Index := Crypto.Plain_Buffer_Index_Type (Item_Index);
      if not Primitive.Valid (Prim) or else
         Primitive.Operation (Prim) /= Write
      then
         Req := Request.Invalid_Object;
         return;
      end if;
      Req := Request.Valid_Object (
         Op     => CBE.Write,
         Succ   => False,
         Blk_Nr => Primitive.Block_Number (Prim),
         Off    => 0,
         Cnt    => 1,
         Tg     => 0);
   end Crypto_Cipher_Data_Required;

   procedure Crypto_Cipher_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive (
         Obj.Crypto_Obj, Crypto.Item_Index_Type (Data_Index));
   end Crypto_Cipher_Data_Requested;

   procedure Supply_Crypto_Cipher_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type;
      Data_Valid :        Boolean)
   is
   begin
      Crypto.Mark_Completed_Primitive (
         Obj.Crypto_Obj, Crypto.Item_Index_Type (Data_Index), Data_Valid);
   end Supply_Crypto_Cipher_Data;

   procedure Crypto_Plain_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Cipher_Buffer_Index_Type)
   is
      Item_Index : Crypto.Item_Index_Type;
      Prim       : Primitive.Object_Type;
   begin
      Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj,  Item_Index, Prim);
      Data_Index := Crypto.Cipher_Buffer_Index_Type (Item_Index);
      if not Primitive.Valid (Prim) or else
         Primitive.Operation (Prim) /= Read
      then
         Req := Request.Invalid_Object;
         return;
      end if;
      Req := Request.Valid_Object (
         Op     => CBE.Read,
         Succ   => False,
         Blk_Nr => Primitive.Block_Number (Prim),
         Off    => 0,
         Cnt    => 1,
         Tg     => 0);
   end Crypto_Plain_Data_Required;

   procedure Crypto_Plain_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive (
         Obj.Crypto_Obj, Crypto.Item_Index_Type (Data_Index));
   end Crypto_Plain_Data_Requested;

   procedure Supply_Crypto_Plain_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type;
      Data_Valid :        Boolean)
   is
   begin
      Crypto.Mark_Completed_Primitive (
         Obj.Crypto_Obj, Crypto.Item_Index_Type (Data_Index), Data_Valid);
   end Supply_Crypto_Plain_Data;

   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

   function To_String (WFE : Wait_For_Event_Type)
   return String
   is (
      "WFE (Req=" & Request.To_String (WFE.Req) &
      ", Prim="        & Primitive.To_String (WFE.Prim) &
      ", Event="       & To_String (WFE.Event) &
      ", In_Progress=" & Debug.To_String (WFE.In_Progress) & ")");

   function To_String (Obj : Object_Type)
   return String
   is (
      "CBE=(" &
      ", Wait_For_Back_End="  & To_String (Obj.Wait_For_Back_End) &
      ", Wait_For_Front_End=" & To_String (Obj.Wait_For_Front_End) &
      ", VBD="                & Virtual_Block_Device.To_String (Obj.VBD) &
      ", Secure_Superblock="  & Debug.To_String (Obj.Secure_Superblock) &
      ")");

end CBE.Library;
