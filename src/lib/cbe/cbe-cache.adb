--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;

package body CBE.Cache
with SPARK_Mode
is

   package body Cache_Item
   with SPARK_Mode
   is

      --
      --  Unused_Object
      --
      function Unused_Object
      return Cache_Item_Type
      is (
         PBA   => 0,
         Ts    => 0,
         State => Unused,
         Dirty => False);

      --
      --  Initialize_Object
      --
      procedure Initialize_Object (
         Obj : out Cache_Item_Type;
         PBA : Physical_Block_Address_Type;
         Ts  : Timestamp_Type)
      is
      begin
         Obj.State := Used;
         Obj.PBA   := PBA;
         Obj.Ts    := Ts;
      end Initialize_Object;

      -----------------
      --  Accessors  --
      -----------------

      function Unused (Obj : Cache_Item_Type) return Boolean
      is (Obj.State = Unused);

      function Used   (Obj : Cache_Item_Type) return Boolean
      is (Obj.State = Used);

      function Dirty  (Obj : Cache_Item_Type) return Boolean
      is (Obj.Dirty);

      function PBA (Obj : Cache_Item_Type) return Physical_Block_Address_Type
      is (Obj.PBA);

      function Ts  (Obj : Cache_Item_Type) return Timestamp_Type
      is (Obj.Ts);

      procedure State (
         Obj : in out Cache_Item_Type;
         Sta :        State_Type)
      is
      begin
         Obj.State := Sta;
      end State;

      procedure Set_Ts (
         Obj : in out Cache_Item_Type;
         Ts  :        Timestamp_Type)
      is
      begin
         Obj.Ts := Ts;
      end Set_Ts;

      procedure Invalidate (Obj : in out Cache_Item_Type)
      is
      begin
         Obj.State := Unused;
      end Invalidate;

      procedure Mark_Dirty (Obj : in out Cache_Item_Type)
      is
      begin
         Obj.Dirty := True;
      end Mark_Dirty;

      procedure Mark_Clean (Obj : in out Cache_Item_Type)
      is
      begin
         Obj.Dirty := False;
      end Mark_Clean;

   end Cache_Item;

   package body Job_Item
   with SPARK_Mode
   is

      --
      --  Pending_Object
      --
      function Pending_Object (PBA : Physical_Block_Address_Type)
      return Job_Item_Type
      is (
         State => Pending,
         Success => False,
         PBA => PBA);

      --
      --  Unused_Object
      --
      function Unused_Object
      return Job_Item_Type
      is (
         PBA     => 0,
         State   => Unused,
         Success => False);

      --
      --  Already_Pending
      --
      function Already_Pending (
         Obj : Job_Item_Type;
         PBA : Physical_Block_Address_Type)
      return Boolean
      is
      begin
         if Obj.State = Pending and then Obj.PBA = PBA then
            return True;
         end if;
         return False;
      end Already_Pending;

      -----------------
      --  Accessors  --
      -----------------

      function Unused (Obj : Job_Item_Type) return Boolean
      is (Obj.State = Unused);

      function Pending (Obj : Job_Item_Type) return Boolean
      is (Obj.State = Pending);

      function In_Progress (Obj : Job_Item_Type) return Boolean
      is (Obj.State = In_Progress);

      function Complete (Obj : Job_Item_Type) return Boolean
      is (Obj.State = Complete);

      function PBA (Obj : Job_Item_Type) return Physical_Block_Address_Type
      is (Obj.PBA);

      function Success (Obj : Job_Item_Type) return Boolean
      is (Obj.Success);

      procedure State (
         Obj : in out Job_Item_Type;
         Sta :        State_Type)
      is
      begin
         Obj.State := Sta;
      end State;

      procedure Set_Unused (Obj : in out Job_Item_Type)
      is
      begin
         Obj.State := Unused;
      end Set_Unused;

      procedure Set_Success (
         Obj : in out Job_Item_Type;
         Suc :        Boolean)
      is
      begin
         Obj.Success := Suc;
      end Set_Success;
   end Job_Item;

   --
   --  Get_Cache_Slot
   --
   function Get_Cache_Slot (Obj : Object_Type)
   return Cache_Index_Type
   is
      Cache_Id : Cache_Index_Type := Cache_Index_Type'Last;
      Min_Used : Timestamp_Type := Timestamp_Type'Last;
   begin
      Unused_Slot : for Cache_Item_Id in Obj.Cache_Items'Range loop
         if Cache_Item.Unused (Obj.Cache_Items (Cache_Item_Id)) then
            return Cache_Item_Id;
         end if;
      end loop Unused_Slot;

      Evict_Slot : for Cache_Item_Id in Obj.Cache_Items'Range loop
         if Cache_Item.Used (Obj.Cache_Items (Cache_Item_Id)) then
            if Min_Used > Cache_Item.Ts (Obj.Cache_Items (Cache_Item_Id))
            then
               Cache_Id := Cache_Item_Id;
               Min_Used := Cache_Item.Ts (Obj.Cache_Items (Cache_Item_Id));
            end if;
         end if;
      end loop Evict_Slot;

      return Cache_Id;
   end Get_Cache_Slot;

   --
   --  Initialize_Object
   --
   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin
      Obj := Initialized_Object;
   end Initialize_Object;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is (
      Cache_Items      => (others => Cache_Item.Unused_Object),
      Job_Items        => (others => Job_Item.Unused_Object),
      Active_Jobs      => 0,
      Execute_Progress => False);

   --
   --  Data_Available
   --
   function Data_Available (
      Obj : Object_Type;
      PBA : Physical_Block_Address_Type)
   return Boolean
   is
   begin
      Item_Loop : for Item of Obj.Cache_Items loop
         if Cache_Item.Used (Item) and then Cache_Item.PBA (Item) = PBA
         then
            return True;
         end if;
      end loop Item_Loop;

      return False;
   end Data_Available;

   --
   --  Data_Index
   --
   procedure Data_Index (
      Obj    : in out Object_Type;
      PBA    :        Physical_Block_Address_Type;
      Ts     :        Timestamp_Type;
      Result :    out Cache_Index_Type)
   is
   begin
      for Cache_Item_Id in Obj.Cache_Items'Range loop
         if Cache_Item.Used (Obj.Cache_Items (Cache_Item_Id)) and then
            Cache_Item.PBA (Obj.Cache_Items (Cache_Item_Id)) = PBA
         then
            Cache_Item.Set_Ts (Obj.Cache_Items (Cache_Item_Id), Ts);
            Result := Cache_Item_Id;
            return;
         end if;
      end loop;
      raise Program_Error;
   end Data_Index;

   --
   --  Invalidate
   --
   procedure Invalidate (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type)
   is
   begin
      for Cache_Item_Id in Obj.Cache_Items'Range loop
         if Cache_Item.Used (Obj.Cache_Items (Cache_Item_Id)) and then
            Cache_Item.PBA (Obj.Cache_Items (Cache_Item_Id)) = PBA
         then
            Cache_Item.Invalidate (Obj.Cache_Items (Cache_Item_Id));
         end if;
      end loop;
   end Invalidate;

   --
   --  Dirty
   --
   function Dirty (
      Obj : Object_Type;
      Idx : Cache_Index_Type)
   return Boolean
   is
   begin
      ---  XXX remove loop and access directly?
      for Cache_Item_Id in Obj.Cache_Items'Range loop
         if Cache_Item_Id = Idx
         then
            return Cache_Item.Dirty (Obj.Cache_Items (Cache_Item_Id));
         end if;
      end loop;
      return False;
   end Dirty;

   --
   --  Flush
   --
   function Flush (
      Obj : Object_Type;
      Idx : Cache_Index_Type)
   return Physical_Block_Address_Type
   is
   begin
      return Cache_Item.PBA (Obj.Cache_Items (Idx));
   end Flush;

   --
   --  Mark_Clean
   --
   procedure Mark_Clean (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type)
   is
   begin
      for Cache_Item_Id in Obj.Cache_Items'Range loop
         if Cache_Item.Used (Obj.Cache_Items (Cache_Item_Id)) and then
            Cache_Item.PBA (Obj.Cache_Items (Cache_Item_Id)) = PBA
         then
            Cache_Item.Mark_Clean (Obj.Cache_Items (Cache_Item_Id));
         end if;
      end loop;
   end Mark_Clean;

   --
   --  Mark_Dirty
   --
   procedure Mark_Dirty (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type)
   is
   begin
      for Cache_Item_Id in Obj.Cache_Items'Range loop
         if Cache_Item.Used (Obj.Cache_Items (Cache_Item_Id)) and then
            Cache_Item.PBA (Obj.Cache_Items (Cache_Item_Id)) = PBA
         then
            Cache_Item.Mark_Dirty (Obj.Cache_Items (Cache_Item_Id));
         end if;
      end loop;
   end Mark_Dirty;

   --
   --  Request_Acceptable_Logged
   --
   function Request_Acceptable_Logged (
      Obj : Object_Type;
      PBA : Physical_Block_Address_Type)
   return Boolean
   is
      Result : constant Boolean := Request_Acceptable (Obj, PBA);
   begin
      return Result;
   end Request_Acceptable_Logged;

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (
      Obj : Object_Type;
      PBA : Physical_Block_Address_Type)
   return Boolean
   is
   begin
      --  XXX replace cast, i.e. change Active_Jobs to Cache_Index_Type
      if Obj.Active_Jobs >= Standard.Natural (Cache_Job_Index_Type'Last) then
         return False;
      end if;

      for Item of Obj.Job_Items loop
         if Job_Item.Already_Pending (Item, PBA) then
            return False;
         end if;
      end loop;

      return True;
   end Request_Acceptable;

   --
   --  Submit_Request_Logged
   --
   procedure Submit_Request_Logged (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type)
   is
   begin
      Submit_Request (Obj, PBA);
   end Submit_Request_Logged;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj : in out Object_Type;
      PBA :        Physical_Block_Address_Type)
   is
   begin
      --
      --  There is already a Job pending, we do not need to do the work
      --  twice.
      --
      Already_Pending : for Item of Obj.Job_Items loop
         if Job_Item.Already_Pending (Item, PBA) then
            return;
         end if;
      end loop Already_Pending;

      --
      --  The data is already available, do not create a new Job.
      --  (XXX It stands to reason is this check is necessary.)
      --
      if Data_Available (Obj, PBA) then
         return;
      end if;

      New_Job : for Job_Item_Id in Obj.Job_Items'Range loop
         if Job_Item.Unused (Obj.Job_Items (Job_Item_Id))
         then
            Obj.Job_Items (Job_Item_Id) := Job_Item.Pending_Object (PBA);
            Job_Item.State (Obj.Job_Items (Job_Item_Id), Job_Item.Pending);
            Obj.Active_Jobs := Obj.Active_Jobs + 1;
            return;
         end if;
      end loop New_Job;
   end Submit_Request;

   --
   --  Fill_Cache
   --
   procedure Fill_Cache (
      Obj      : in out Object_Type;
      Data     : in out Cache_Data_Type;
      Job_Data : in     Cache_Job_Data_Type;
      Time     :        Timestamp_Type)
   is
      Cache_Id : Cache_Index_Type;
   begin
      Obj.Execute_Progress := False;

      Complete_Job : for Job_Item_Id in Obj.Job_Items'Range loop
         if Job_Item.Complete (Obj.Job_Items (Job_Item_Id)) and then
            Job_Item.Success (Obj.Job_Items (Job_Item_Id))
         then
            Cache_Id := Get_Cache_Slot (Obj);
            Cache_Item.Initialize_Object (
               Obj.Cache_Items (Cache_Id),
               Job_Item.PBA (Obj.Job_Items (Job_Item_Id)), Time);

            --  XXX will it work?
            Data (Cache_Id) := Job_Data (Job_Item_Id);

            Job_Item.Set_Unused (Obj.Job_Items (Job_Item_Id));

            Obj.Execute_Progress := True;
            Obj.Active_Jobs := Obj.Active_Jobs - 1;
         end if;
      end loop Complete_Job;
   end Fill_Cache;

   --
   --  Execute_Progress
   --
   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      Peek_Primitive : for Job_Item_Id in Obj.Job_Items'Range loop
         if Job_Item.Pending (Obj.Job_Items (Job_Item_Id))
         then
            return Primitive.Valid_Object (
               Op     => Read,
               Succ   => Request.Success_Type (False),
               Tg     => 16#20#,
               Blk_Nr => Block_Number_Type (
                  Job_Item.PBA (Obj.Job_Items (Job_Item_Id))),
               Idx    => 0);
         end if;
      end loop Peek_Primitive;

      return Primitive.Invalid_Object;
   end Peek_Generated_Primitive;

   --
   --  Peek_Generated_Data_Index
   --
   function Peek_Generated_Data_Index (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Cache_Index_Type
   is
   begin
      Pending_Data : for Job_Item_Id in Obj.Job_Items'Range loop
         if Job_Item.Pending (Obj.Job_Items (Job_Item_Id)) and then
            Job_Item.PBA (Obj.Job_Items (Job_Item_Id)) =
               Physical_Block_Address_Type (Primitive.Block_Number (Prim))
         then
            return Cache_Index_Type (Job_Item_Id);
         end if;
      end loop Pending_Data;

      return Cache_Index_Type'Last;
   end Peek_Generated_Data_Index;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Drop_Primitive : for Job_Item_Id in Obj.Job_Items'Range loop
         if Job_Item.Pending (Obj.Job_Items (Job_Item_Id)) and then
            Job_Item.PBA (Obj.Job_Items (Job_Item_Id)) =
               Physical_Block_Address_Type (Primitive.Block_Number (Prim))
         then
            Job_Item.State (Obj.Job_Items (Job_Item_Id), Job_Item.In_Progress);
            return;
         end if;
      end loop Drop_Primitive;
   end Drop_Generated_Primitive;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Complete_Primitive : for Job_Item_Id in Obj.Job_Items'Range loop
         if Job_Item.In_Progress (Obj.Job_Items (Job_Item_Id)) and then
            Job_Item.PBA (Obj.Job_Items (Job_Item_Id)) =
               Physical_Block_Address_Type (Primitive.Block_Number (Prim))
         then
            Job_Item.State (Obj.Job_Items (Job_Item_Id), Job_Item.Complete);
            Job_Item.Set_Success (
               Obj.Job_Items (Job_Item_Id),
               Boolean (Primitive.Success (Prim)));
            return;
         end if;
      end loop Complete_Primitive;
   end Mark_Completed_Primitive;
end CBE.Cache;
