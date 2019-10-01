with Ada.Unchecked_Conversion;
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
--  with Componolit.Gneiss.Strings;
with Componolit.Gneiss.Strings_Generic;
with CBE.Library;
with CBE.Request;
with CBE.Crypto;
with Conversion;

package body Component is

   use type Block.Size;
   use type CBE.Timestamp_Type;

   subtype Block_Buffer is Component.Buffer (1 .. 4096);
   type Superblock_Request_Cache is
      array (Request_Id range 0 .. 7) of Block_Client.Request;
   type Superblocks_Initialized is
      array (CBE.Super_Blocks_Index_Type) of Boolean;

   type Cache_Entry is record
      S : Block_Server.Request;
      V : Boolean := False;
      P : Boolean := False;
   end record;

   type Request_Cache is
      array (Request_Id range 1 .. 32) of Cache_Entry;

   Cbe_Block_Size      : constant Block.Size := 4096;
   Virtual_Block_Count : Block.Count         := 0;

   function Time_To_Ns is new Ada.Unchecked_Conversion (Gns.Timer.Time, CBE.Timestamp_Type);
   function Convert_Time (T : Gns.Timer.Time) return CBE.Timestamp_Type is
      (Time_To_Ns (T) / 1000);
   procedure Convert_Block is new Conversion.Convert (Block_Buffer, CBE.Super_Block_Type);

   function Image is new Gns.Strings_Generic.Image_Ranged (Block.Size);
   --  function Image is new Gns.Strings_Generic.Image_Modular (Block.Id);
   --  function Image is new Gns.Strings_Generic.Image_Ranged (CBE.Super_Blocks_Index_Type);

   Dispatcher  : Block.Dispatcher_Session;
   Client      : Block.Client_Session;
   Server      : Block.Server_Session;
   Log         : Gns.Log.Client_Session;
   Timer       : Gns.Timer.Client_Session;
   Capability  : Gns.Types.Capability;
   Ready       : Boolean  := False;
   Sb_Alloc    : Block.Id := 0;
   Cbe_Session : CBE.Library.Object_Type;
   Client_Act  : Boolean  := False;

   procedure Read_Superblock with
      Pre => Block.Initialized (Client)
      and then Gns.Log.Initialized (Log);

   procedure Handle_Server_Requests (Progress : out Boolean) with
      Pre => Block.Initialized (Client)
             and then Gns.Log.Initialized (Log);

   procedure Cbe_Server_Read;
   procedure Cbe_Server_Write;

   Sbsr : Superblock_Request_Cache;
   --  This should be a variable inside Read_Superblock. But since the Request type
   --  on contains the block itself and we have limited stack space there it resides in the package.
   Superblocks      : CBE.Super_Blocks_Type;
   Superblocks_Init : Superblocks_Initialized :=
      (others => False);
   Cache            : Request_Cache;

   procedure Read_Superblock is
      use type Block.Id;
      use type Block.Request_Status;
      use type CBE.Generation_Type;
      use type CBE.Virtual_Block_Address_Type;
      Result   : Block.Result;
      Max_Vba  : CBE.Virtual_Block_Address_Type;
      Progress : Boolean                     := True;
      Current  : CBE.Super_Blocks_Index_Type := 0;
      Last_Gen : CBE.Generation_Type         := 0;
      Valid    : Boolean                     := False;
   begin
      if Block.Block_Size (Client) = Cbe_Block_Size then
         loop
            for I in Sbsr'Range loop
               case Block_Client.Status (Sbsr (I)) is
                  when Block.Raw =>
                     if Sb_Alloc < 8 then
                        Block_Client.Allocate_Request
                           (Client, Sbsr (I), Block.Read, Sb_Alloc, 1,
                            I, Result);
                        case Result is
                           when Block.Success =>
                              Sb_Alloc := Sb_Alloc + Block.Id'(1);
                           when Block.Unsupported =>
                              Gns.Log.Client.Error
                                 (Log, "Unable to read superblock");
                              Main.Vacate (Capability, Main.Failure);
                              return;
                           when others =>
                              null;
                        end case;
                     end if;
                  when Block.Allocated =>
                     Block_Client.Enqueue (Client, Sbsr (I));
                  when Block.Pending =>
                     Block_Client.Update_Request (Client, Sbsr (I));
                     Progress :=
                        Block_Client.Status (Sbsr (I)) /= Block.Pending;
                  when Block.Ok =>
                     Block_Client.Read (Client, Sbsr (I));
                     Block_Client.Release (Client, Sbsr (I));
                  when Block.Error =>
                     Gns.Log.Client.Error
                        (Log, "Failed to read superblock");
                     Main.Vacate (Capability, Main.Failure);
                     return;
               end case;
            end loop;
            exit when not Progress or else
               (Sb_Alloc = 8
                and then
                (for all R of Sbsr =>
                    Block_Client.Status (R) = Block.Raw));
         end loop;
      else
         Gns.Log.Client.Error
            (Log,
             "Invalid block size " & Image (Block.Block_Size (Client)));
         Main.Vacate (Capability, Main.Failure);
      end if;
      if Sb_Alloc = 8 and then (for all T of Superblocks_Init => T) then
         for I in Superblocks'Range loop
            if Superblocks (I).Last_Secured_Generation /=
               CBE.Generation_Type'Last
               and then Superblocks (I).Last_Secured_Generation >=
                  Last_Gen
            then
               Valid    := True;
               Current  := I;
               Last_Gen := Superblocks (I).Last_Secured_Generation;
            end if;
         end loop;
         if Valid then
            CBE.Library.Initialize_Object
               (Cbe_Session, Superblocks, Current);
            Max_Vba := CBE.Library.Max_VBA (Cbe_Session);
            if Max_Vba < CBE.Virtual_Block_Address_Type'Last
               and then Max_Vba <
                  CBE.Virtual_Block_Address_Type (Block.Count'Last)
            then
               Virtual_Block_Count := Block.Count (Max_Vba + 1);
               Ready               := True;
               Gns.Log.Client.Info (Log, "CBE ready");
               Block_Dispatcher.Register (Dispatcher);
            else
               Gns.Log.Client.Error
                  (Log, "Virtual block address space is too large.");
               Main.Vacate (Capability, Main.Failure);
            end if;
         else
            Gns.Log.Client.Error (Log, "No valid superblock found");
            Main.Vacate (Capability, Main.Failure);
         end if;
      end if;
   end Read_Superblock;

   procedure Handle_Server_Requests (Progress : out Boolean) is
      use type Block.Request_Status;
      use type Block.Id;
      use type Block.Count;
      Start   : Block.Id;
      Length  : Block.Count;
      Request : CBE.Request.Object_Type;
      Op      : CBE.Operation_Type := CBE.Read;
   begin
      Progress := False;
      for I in Cache'Range loop
         if Block_Server.Status (Cache (I).S) = Block.Raw then
            Block_Server.Process (Server, Cache (I).S);
            if Block_Server.Status (Cache (I).S) = Block.Pending then
               Progress := Progress or True;
               Start  := Block_Server.Start (Cache (I).S);
               Length := Block_Server.Length (Cache (I).S);
               Cache (I).V :=
                  Length <= Block.Count (CBE.Request.Count_Type'Last) and then
                  Block.Id'Last - Length > Start and then
                  Start + Length - 1 <= Block.Count (CBE.Library.Max_VBA (Cbe_Session)) and then
                  Block_Server.Kind (Cache (I).S) in Block.Read | Block.Write | Block.Sync;
            end if;
         end if;
         if Block_Server.Status (Cache (I).S) = Block.Pending then
            if Cache (I).V then
               if CBE.Library.Request_Acceptable (Cbe_Session) then
                  case Block_Server.Kind (Cache (I).S) is
                     when Block.Read => Op := CBE.Read;
                     when Block.Write => Op := CBE.Write;
                     when Block.Sync => Op := CBE.Sync;
                     when others => null;
                  end case;
                  Start   := Block_Server.Start (Cache (I).S);
                  Length  := Block_Server.Length (Cache (I).S);
                  Request := CBE.Request.Valid_Object
                              (Op, True,
                               CBE.Block_Number_Type (Start),
                               0,
                               CBE.Request.Count_Type (Length),
                               CBE.Tag_Type (I));
                  Cache (I).P := True;
                  CBE.Library.Submit_Request (Cbe_Session, Request);
               end if;
            else
               Block_Server.Acknowledge (Server, Cache (I).S, Block.Error);
               Progress := Progress or else Block_Server.Status (Cache (I).S) = Block.Raw;
            end if;
         end if;
      end loop;
   end Handle_Server_Requests;

   procedure Construct (Cap : Gns.Types.Capability) is
   begin
      Capability := Cap;
      if not Gns.Log.Initialized (Log) then
         Gns.Log.Client.Initialize (Log, Capability, "cbe");
      end if;
      if Gns.Log.Initialized (Log) then
         Gns.Log.Client.Info (Log, "CBE Server");
         if not Block.Initialized (Dispatcher) then
            Block_Dispatcher.Initialize (Dispatcher, Cap, 42);
         end if;
         if not Block.Initialized (Dispatcher) then
            Gns.Log.Client.Error
               (Log, "Dispatcher initialization failed.");
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;
         if not Block.Initialized (Client) then
            Block_Client.Initialize (Client, Cap, "cbe", 42);
         end if;
         if not Block.Initialized (Client) then
            Gns.Log.Client.Error (Log, "Client initialization failed.");
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;
         if not Gns.Timer.Initialized (Timer) then
            Timer_Client.Initialize (Timer, Cap);
         end if;
         if not Gns.Timer.Initialized (Timer) then
            Gns.Log.Client.Error (Log, "Timer initialization failed");
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;
         Read_Superblock;
      else
         Main.Vacate (Cap, Main.Failure);
      end if;
      Gns.Log.Client.Info (Log, "/Construct");
   end Construct;

   procedure Destruct is
   begin
      if Gns.Log.Initialized (Log) then
         Gns.Log.Client.Warning (Log, "Stopping CBE");
         Gns.Log.Client.Finalize (Log);
      end if;
      if Block.Initialized (Client) then
         Block_Client.Finalize (Client);
      end if;
      if Block.Initialized (Dispatcher) then
         Block_Dispatcher.Finalize (Dispatcher);
      end if;
   end Destruct;

   procedure Write
      (C : in out Block.Client_Session;
       I :        Request_Id;
       D :    out Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (I);
   begin
      D :=
         (others => 0);
   end Write;

   procedure Read
      (C : in out Block.Client_Session;
       I :        Request_Id;
       D :        Buffer)
   is
      pragma Unreferenced (C);
      use type Block.Request_Status;
      S : CBE.Super_Blocks_Index_Type;
   begin
      if Ready then
         null;
      else --  reading superblocks
         if D'Length = 4096
            and then Block_Client.Status (Sbsr (I)) = Block.Ok
            and then Block_Client.Start (Sbsr (I)) in 0 .. 7
         then
            S := CBE.Super_Blocks_Index_Type
                  (Block_Client.Start (Sbsr (I)));
            Convert_Block (D, Superblocks (S));
            Superblocks_Init (S) := True;
         end if;
      end if;
   end Read;

   procedure Event is
      Progress : Boolean := True;
      Now      : Gns.Timer.Time;
   begin
      if Ready then
         while Progress loop
            Handle_Server_Requests (Progress);
            Now := Timer_Client.Clock (Timer);
            CBE.Library.Execute (Cbe_Session, Convert_Time (Now));
            Progress := Progress or else CBE.Library.Execute_Progress (Cbe_Session);
            Cbe_Server_Read;
            Cbe_Server_Write;
         end loop;
      else
         Read_Superblock;
      end if;
   end Event;

   procedure Dispatch
      (I : in out Block.Dispatcher_Session;
       C :        Block.Dispatcher_Capability)
   is
   begin
      if Block_Dispatcher.Valid_Session_Request (I, C)
         and then not Initialized (Server)
         and then not Block.Initialized (Server)
      then
         Block_Dispatcher.Session_Initialize (I, C, Server, 42);
         if Initialized (Server) and then Block.Initialized (Server)
         then
            Block_Dispatcher.Session_Accept (I, C, Server);
         end if;
      end if;
      Block_Dispatcher.Session_Cleanup (I, C, Server);
   end Dispatch;

   Server_Plain_Buffer : CBE.Crypto.Plain_Data_Type;

   procedure Cbe_Server_Read is
      Req : CBE.Request.Object_Type;
      Tag : Request_Id;
      P : Boolean;
   begin
      CBE.Library.Client_Data_Ready (Cbe_Session, Req);
      if CBE.Request.Valid (Req) then
         Tag := Request_Id (CBE.Request.Tag (Req));
         if Tag in Cache'Range then
            declare
               procedure Rd (B : Block_Buffer);
               procedure Rd (B : Block_Buffer) is
               begin
                  Block_Server.Read (Server, Cache (Tag).S, B);
               end Rd;
               procedure Crd is new Conversion.Pass_In
                  (CBE.Crypto.Plain_Data_Type,
                   Block_Buffer, Rd);
            begin
               CBE.Library.Obtain_Client_Data
                  (Cbe_Session, Req, Server_Plain_Buffer, P);
               Crd (Server_Plain_Buffer);
            end;
         end if;
      end if;
   end Cbe_Server_Read;

   Server_Block_Buffer : CBE.Block_Data_Type;

   procedure Cbe_Server_Write is
      Req : CBE.Request.Object_Type;
      Tag : Request_Id;
      Now : Gns.Timer.Time;
      P_DUMMY : Boolean;
   begin
      Now := Timer_Client.Clock (Timer);
      CBE.Library.Client_Data_Required (Cbe_Session, Req);
      if CBE.Request.Valid (Req) then
         Gns.Log.Client.Info (Log, "Write valid");
         Tag := Request_Id (CBE.Request.Tag (Req));
         if Tag in Cache'Range then
            declare
               procedure Wrt (B : out Block_Buffer);
               procedure Wrt (B : out Block_Buffer) is
               begin
                  Block_Server.Write (Server, Cache (Tag).S, B);
               end Wrt;
               procedure Cwrt is new Conversion.Pass_Out
                  (CBE.Block_Data_Type,
                   Block_Buffer, Wrt);
            begin
               Cwrt (Server_Block_Buffer);
               P_DUMMY := CBE.Library.Supply_Client_Data
                  (Cbe_Session, Convert_Time (Now), Req, Server_Block_Buffer);
            end;
         end if;
      end if;
   end Cbe_Server_Write;

   procedure Initialize_Server
      (S : in out Block.Server_Session;
       L :        String;
       B :        Block.Byte_Length)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (L);
      pragma Unreferenced (B);
   begin
      Client_Act := True;
   end Initialize_Server;

   procedure Finalize_Server (S : in out Block.Server_Session) is
      pragma Unreferenced (S);
   begin
      null;
   end Finalize_Server;

   function Block_Count
      (S : Block.Server_Session)
      return Block.Count
   is
      pragma Unreferenced (S);
   begin
      return Virtual_Block_Count;
   end Block_Count;

   function Block_Size
      (S : Block.Server_Session)
      return Block.Size
   is
      pragma Unreferenced (S);
   begin
      return Cbe_Block_Size;
   end Block_Size;

   function Writable
      (S : Block.Server_Session)
      return Boolean
   is
      pragma Unreferenced (S);
   begin
      return True;
   end Writable;

   function Initialized
      (S : Block.Server_Session)
      return Boolean is (Client_Act);

end Component;
