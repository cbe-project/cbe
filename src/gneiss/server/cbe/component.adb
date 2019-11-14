with Ada.Unchecked_Conversion;
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Strings;
with Componolit.Gneiss.Strings_Generic;
with CBE.Library;
with CBE.Request;
with CBE.Crypto;
with CBE.Block_IO;
with CBE.Primitive;
with External.Crypto;
with Conversion;

package body Component is

   use type Block.Size;
   use type CBE.Timestamp_Type;

   subtype Block_Buffer is Component.Buffer (1 .. 4096);
   type Superblock_Request_Cache is
      array (Request_Id range 0 .. 7) of Block_Client.Request;
   type Superblocks_Initialized is
      array (CBE.Superblocks_Index_Type) of Boolean;

   Cbe_Block_Size      : constant Block.Size := 4096;
   Virtual_Block_Count : Block.Count         := 0;

   type Request_Status is (Empty, Accepted, Pending, Ok, Error);
   type Crypto_Status is (Empty, Accepted, Submitted);

   type Server_Cache_Entry is record
      R : Block_Server.Request;
      S : Request_Status := Empty;
   end record;

   type Server_Cache is array (Request_Id range 1 .. 32) of Server_Cache_Entry;

   type Client_Cache_Entry is record
      R : Block_Client.Request;
      C : CBE.Request.Object_Type;
      I : CBE.Block_IO.Data_Index_Type;
   end record;

   type Client_Cache is array (Request_Id range 1 .. 8) of Client_Cache_Entry;

   type Crypto_Enc_Cache_Entry is record
      R : CBE.Request.Object_Type;
      I : CBE.Crypto.Plain_Buffer_Index_Type;
      S : Crypto_Status := Empty;
   end record;

   type Crypto_Enc_Cache is array (Request_Id range 1 .. 8) of
      Crypto_Enc_Cache_Entry;

   type Crypto_Dec_Cache_Entry is record
      R : CBE.Request.Object_Type;
      I : CBE.Crypto.Cipher_Buffer_Index_Type;
      S : Crypto_Status := Empty;
   end record;

   type Crypto_Dec_Cache is array (Request_Id range 1 .. 8) of
      Crypto_Dec_Cache_Entry;

   function Time_To_Ns is new Ada.Unchecked_Conversion (Gns.Timer.Time,
       CBE.Timestamp_Type);
   function Convert_Time
      (T : Gns.Timer.Time)
      return CBE.Timestamp_Type is (Time_To_Ns (T) / 1000);
   procedure Convert_Block is new Conversion.Convert
      (Block_Buffer, CBE.Superblock_Type);
   procedure Convert_Block is new Conversion.Convert
      (CBE.Block_Data_Type, Block_Buffer);
   procedure Convert_Block is new Conversion.Convert
      (Block_Buffer, CBE.Block_Data_Type);

   function Image is new Gns.Strings_Generic.Image_Ranged (Block.Size);
   --  function Image is new Gns.Strings_Generic.Image_Ranged (Request_Id);
   function Image is new Gns.Strings_Generic.Image_Modular (Block.Id);
   function Image is new Gns.Strings_Generic.Image_Ranged
      (CBE.Block_IO.Data_Index_Type);
   function Image is new Gns.Strings_Generic.Image_Ranged (Request_Id);
   --  function Image is new Gns.Strings_Generic.Image_Ranged
   --    (CBE.Superblocks_Index_Type);

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
   S_Cache     : Server_Cache;
   C_Cache     : Client_Cache;
   Crypto      : External.Crypto.Object_Type :=
      External.Crypto.Initialized_Object;
   Ce_Cache    : Crypto_Enc_Cache;
   Cd_Cache    : Crypto_Dec_Cache;

   function Create_Request (I : Request_Id)
      return CBE.Request.Object_Type with
      Pre => Block_Server.Kind (S_Cache (I).R) in
               Block.Read | Block.Write | Block.Sync;

   procedure Read_Superblock (Progress : out Boolean) with
      Pre => Block.Initialized (Client) and then Gns.Log.Initialized (Log);

   procedure Handle_Incoming_Requests (Progress : in out Boolean);
   procedure Handle_Read_Progress (Progress : in out Boolean);
   procedure Handle_Write_Progress (Progress  : in out Boolean;
                                    Timestamp :        CBE.Timestamp_Type);
   procedure Handle_Backend_Io (Progress : in out Boolean);
   procedure Initialize_Crypto;
   procedure Handle_Encryption (Progress : in out Boolean);
   procedure Handle_Decryption (Progress : in out Boolean);
   procedure Handle_Completed_Requests (Progress : in out Boolean);

   Sbsr : Superblock_Request_Cache;
   --  This should be a variable inside Read_Superblock.
   --  But since the Request type on contains the block
   --  itself and we have limited stack space there it resides in the package.
   Superblocks      : CBE.Superblocks_Type;
   Superblocks_Init : Superblocks_Initialized :=
      (others => False);
   Plain_Buffer : CBE.Crypto.Plain_Buffer_Type;
   Cipher_Buffer : CBE.Crypto.Cipher_Buffer_Type;
   Io_Buffer : CBE.Block_IO.Data_Type;

   function Create_Request (I : Request_Id)
      return CBE.Request.Object_Type
   is
      Cbe_Request : CBE.Request.Object_Type;
      Operation   : CBE.Operation_Type;
   begin
      Operation := (case Block_Server.Kind (S_Cache (I).R) is
                     when Block.Read => CBE.Read,
                     when Block.Write => CBE.Write,
                     when Block.Sync => CBE.Sync,
                     when others => CBE.Read);
      Cbe_Request := CBE.Request.Valid_Object
         (Operation,
          False,
          CBE.Block_Number_Type (Block_Server.Start (S_Cache (I).R)),
          0,
          CBE.Request.Count_Type (Block_Server.Length (S_Cache (I).R)),
          CBE.Request.Tag_Type (I));
      return Cbe_Request;
   end Create_Request;

   function Kind (R : CBE.Request.Object_Type) return Block.Request_Kind
   is
      (case CBE.Request.Operation (R) is
         when CBE.Read => Block.Read,
         when CBE.Write => Block.Write,
         when CBE.Sync => Block.Sync);

   procedure Read_Superblock (Progress : out Boolean) is
      use type Block.Id;
      use type Block.Count;
      use type Block.Request_Status;
      use type CBE.Generation_Type;
      Result   : Block.Result;
      Max_Vba  : CBE.Virtual_Block_Address_Type;
      Current  : CBE.Superblocks_Index_Type := 0;
      Last_Gen : CBE.Generation_Type         := 0;
      Valid    : Boolean                     := False;
   begin
      Progress := True;
      if Block.Block_Size (Client) = Cbe_Block_Size then
         loop
            for I in Sbsr'Range loop
               case Block_Client.Status (Sbsr (I)) is
                  when Block.Raw =>
                     if Sb_Alloc < 8 then
                        Block_Client.Allocate_Request
                           (Client, Sbsr (I), Block.Read, Sb_Alloc, 1, I,
                            Result);
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
                     Gns.Log.Client.Error (Log, "Failed to read superblock");
                     Main.Vacate (Capability, Main.Failure);
                     return;
               end case;
            end loop;
            exit when not Progress
               or else
               (Sb_Alloc = 8
                and then
                (for all R of Sbsr => Block_Client.Status (R) = Block.Raw));
         end loop;
      else
         Gns.Log.Client.Error
            (Log, "Invalid block size " & Image (Block.Block_Size (Client)));
         Main.Vacate (Capability, Main.Failure);
      end if;
      if Sb_Alloc = 8 and then (for all T of Superblocks_Init => T) then
         for I in Superblocks'Range loop
            if Superblocks (I).Last_Secured_Generation /=
               CBE.Generation_Type'Last
               and then Superblocks (I).Last_Secured_Generation >= Last_Gen
            then
               Valid    := True;
               Current  := I;
               Last_Gen := Superblocks (I).Last_Secured_Generation;
               exit;
            end if;
         end loop;
         if Valid then
            CBE.Library.Initialize_Object (Cbe_Session, Superblocks, Current);
            Max_Vba := CBE.Library.Max_VBA (Cbe_Session);
            Virtual_Block_Count := Block.Count (Max_Vba) + 1;
            Ready               := True;
            Gns.Log.Client.Info (Log, "CBE ready");
            Block_Dispatcher.Register (Dispatcher);
         else
            Gns.Log.Client.Error (Log, "No valid superblock found");
            Main.Vacate (Capability, Main.Failure);
         end if;
         Progress := False;
      end if;
   end Read_Superblock;

   procedure Construct (Cap : Gns.Types.Capability)
   is
      Progress : Boolean;
   begin
      Capability := Cap;
      if not Gns.Log.Initialized (Log) then
         Gns.Log.Client.Initialize (Log, Capability, "CBE");
      end if;
      if Gns.Log.Initialized (Log) then
         Gns.Log.Client.Info (Log, "CBE Server");
         Gns.Log.Client.Info (Log, "Block buffer: " &
            Gns.Strings.Image (Long_Integer (Block_Buffer'Size / 8)));
         Gns.Log.Client.Info (Log, "Superblock: " &
            Gns.Strings.Image (Long_Integer (CBE.Superblock_Type'Size / 8)));
         if not Block.Initialized (Dispatcher) then
            Block_Dispatcher.Initialize (Dispatcher, Cap, 42);
         end if;
         if not Block.Initialized (Dispatcher) then
            Gns.Log.Client.Error (Log, "Dispatcher initialization failed.");
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;
         if not Block.Initialized (Client) then
            Block_Client.Initialize (Client, Cap, "CBE", 42);
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
         Read_Superblock (Progress);
         Initialize_Crypto;
         Timer_Client.Set_Timeout (Timer, 1.0);
         --  snapshot every 1.0s
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

   procedure Initialize_Crypto
   is
      Success : Boolean;
   begin
      External.Crypto.Set_Key
         (Crypto, 1, 0, External.Crypto.Key_Data_Type'(others => 42), Success);
      if Success then
         External.Crypto.Set_Key
            (Crypto, 2, 0, External.Crypto.Key_Data_Type'(others => 42),
             Success);
      end if;
      if not Success then
         Gns.Log.Client.Error (Log, "Failed to set crypto key");
         Main.Vacate (Capability, Main.Failure);
      end if;
   end Initialize_Crypto;

   procedure Write
      (C : in out Block.Client_Session;
       I :        Request_Id;
       D :    out Buffer)
   is
      pragma Unreferenced (C);
   begin
      Convert_Block (Io_Buffer (C_Cache (I).I), D);
   end Write;

   procedure Read
      (C : in out Block.Client_Session;
       I :        Request_Id;
       D :        Buffer)
   is
      pragma Unreferenced (C);
      use type Block.Request_Status;
      S : CBE.Superblocks_Index_Type;
   begin
      if Ready then
         Convert_Block (D, Io_Buffer (C_Cache (I).I));
      else --  reading superblocks
         if D'Length = 4096 and then Block_Client.Status (Sbsr (I)) = Block.Ok
            and then Block_Client.Start (Sbsr (I)) in 0 .. 7
         then
            S := CBE.Superblocks_Index_Type (Block_Client.Start (Sbsr (I)));
            Convert_Block (D, Superblocks (S));
            Superblocks_Init (S) := True;
         end if;
      end if;
   end Read;

   procedure Snapshot_Event
   is
      function Image is new Gns.Strings_Generic.Image_Modular
         (CBE.Generation_Type);
      Snapshot : CBE.Generation_Type;
      Success  : Boolean;
   begin
      CBE.Library.Create_Snapshot (Cbe_Session, False, Snapshot, Success);
      if not Success then
         Gns.Log.Client.Warning (Log, "Failed to create snapshot");
      end if;
      if CBE.Library.Snapshot_Creation_Complete (Cbe_Session, Snapshot) then
         Gns.Log.Client.Info (Log, "Snapshot finished: " & Image (Snapshot));
      end if;
   end Snapshot_Event;

   procedure Event is
      Progress : Boolean := True;
      C_Prog   : Boolean;
      Now      : Gns.Timer.Time;
   begin
      Gns.Log.Client.Info (Log, "Event");
      if Ready then
         if not Initialized (Server) then
            Gns.Log.Client.Warning (Log, "Block server not initialized");
            return;
         end if;
         while Progress loop
            --  Gns.Log.Client.Info (Log, "Ev");
            Progress := False;
            Handle_Incoming_Requests (Progress);
            Now := Timer_Client.Clock (Timer);
            CBE.Library.Execute (Cbe_Session, Io_Buffer,
                                 Plain_Buffer, Cipher_Buffer,
                                 Convert_Time (Now));
            Progress :=
               Progress or else CBE.Library.Execute_Progress (Cbe_Session);
            Handle_Read_Progress (Progress);
            Handle_Write_Progress (Progress, Convert_Time (Now));
            Handle_Backend_Io (Progress);
            Handle_Encryption (Progress);
            Handle_Decryption (Progress);
            External.Crypto.Execute (Crypto, C_Prog);
            Progress := Progress or else C_Prog;
            Handle_Completed_Requests (Progress);
            --  Gns.Log.Client.Info (Log, "/Ev");
         end loop;
         Block_Server.Unblock_Client (Server);
      else
         while Progress loop
            Read_Superblock (Progress);
         end loop;
      end if;
   end Event;

   procedure Handle_Incoming_Requests (Progress : in out Boolean)
   is
      use type Block.Request_Status;
      Cbe_Request : CBE.Request.Object_Type;
   begin
      --  Gns.Log.Client.Info (Log, "Handle_Incoming_Requests");
      for I in S_Cache'Range loop
         case Block_Server.Status (S_Cache (I).R) is
            when Block.Raw =>
               Block_Server.Process (Server, S_Cache (I).R);
               Progress := Progress or else
                           Block_Server.Status (S_Cache (I).R) = Block.Pending;
               S_Cache (I).S := Accepted;
            when Block.Pending =>
               if S_Cache (I).S = Accepted
                  and then CBE.Library.Client_Request_Acceptable (Cbe_Session)
               then
                  Gns.Log.Client.Info
                     (Log, "Accept request at "
                           & Image (Block_Server.Start (S_Cache (I).R)));
                  Cbe_Request := Create_Request (I);
                  CBE.Library.Submit_Client_Request (Cbe_Session,
                                                     Cbe_Request, 0);
                  Progress := True;
                  S_Cache (I).S := Pending;
               end if;
            when others =>
               null;
         end case;
      end loop;
   end Handle_Incoming_Requests;

   Read_Offset : Unsigned_Long := 0;
   Read_Index  : CBE.Crypto.Item_Index_Type := 0;

   procedure Handle_Read_Progress (Progress : in out Boolean)
   is
      Cbe_Request : CBE.Request.Object_Type;
      Prim_Index  : CBE.Primitive.Index_Type;
      Data_Index  : CBE.Crypto.Plain_Buffer_Index_Type;
      Req_Index   : Request_Id;
      Valid       : Boolean;
   begin
      --  Gns.Log.Client.Info (Log, "Handle_Read_Progress");
      CBE.Library.Client_Data_Ready (Cbe_Session, Cbe_Request);
      if CBE.Request.Valid (Cbe_Request) then
         Prim_Index := CBE.Library.Client_Data_Index
            (Cbe_Session, Cbe_Request);
         CBE.Library.Obtain_Client_Data
            (Cbe_Session, Cbe_Request, Data_Index, Valid);
         if Valid then
            Progress := True;
            Req_Index := Request_Id (CBE.Request.Tag (Cbe_Request));
            Read_Offset := Unsigned_Long (Prim_Index) * 4096;
            Read_Index := CBE.Crypto.Item_Index_Type (Data_Index);
            Block_Server.Read (Server, S_Cache (Req_Index).R, Req_Index);
            Gns.Log.Client.Info
               (Log, "Read at "
                     & Image (Block_Server.Start (S_Cache (Req_Index).R)));
         end if;
      end if;
   end Handle_Read_Progress;

   Write_Offset : Unsigned_Long := 0;
   Write_Timestamp : CBE.Timestamp_Type := 0;
   Write_Request : CBE.Request.Object_Type;
   Write_Progress : Boolean;

   procedure Handle_Write_Progress (Progress  : in out Boolean;
                                    Timestamp :        CBE.Timestamp_Type)
   is
      Cbe_Request : CBE.Request.Object_Type;
      Prim_Index  : CBE.Primitive.Index_Type;
      Req_Index   : Request_Id;
   begin
      --  Gns.Log.Client.Info (Log, "Handle_Write_Progress");
      CBE.Library.Client_Data_Required (Cbe_Session, Cbe_Request);
      if CBE.Request.Valid (Cbe_Request) then
         Prim_Index := CBE.Library.Client_Data_Index
            (Cbe_Session, Cbe_Request);
         Req_Index := Request_Id (CBE.Request.Tag (Cbe_Request));
         Write_Offset := Unsigned_Long (Prim_Index) * 4096;
         Write_Timestamp := Timestamp;
         Write_Request := Cbe_Request;
         Write_Progress := False;
         Block_Server.Write (Server, S_Cache (Req_Index).R, Req_Index);
         Progress := Write_Progress;
         Gns.Log.Client.Info
            (Log, "Write at "
                  & Image (Block_Server.Start (S_Cache (Req_Index).R)));
      end if;
   end Handle_Write_Progress;

   procedure Handle_Backend_Io (Progress : in out Boolean)
   is
      use type Block.Request_Status;
      use type Block.Request_Kind;
      use type Block.Result;
      Cbe_Request : CBE.Request.Object_Type;
      Data_Index  : CBE.Block_IO.Data_Index_Type;
      Result      : Block.Result;
   begin
      --  Gns.Log.Client.Info (Log, "Handle_Backend_Io");
      for I in C_Cache'Range loop
         case Block_Client.Status (C_Cache (I).R) is
            when Block.Raw =>
               CBE.Library.Has_IO_Request
                  (Cbe_Session, Cbe_Request, Data_Index);
--  Gns.Log.Client.Info
--     (Log, "CBE Req: "
--           & Gns.Strings.Image (CBE.Request.Valid (Cbe_Request))
--           & " at "
--           & Image (Block.Id (CBE.Request.Block_Number
--              (Cbe_Request)))
--           & " cache: "
--           & Gns.Strings.Image
--              (not CBE.Request.Valid (C_Cache (I).C))
--           & " "
--           & Gns.Strings.Image
--              (for all E of C_Cache =>
--               not CBE.Request.Equal (Cbe_Request, E.C)));
               if
                  CBE.Request.Valid (Cbe_Request)
                  and then not CBE.Request.Valid (C_Cache (I).C)
                  and then (for all E of C_Cache =>
                            not CBE.Request.Equal (Cbe_Request, E.C))
               then
                  Block_Client.Allocate_Request
                     (Client,
                      C_Cache (I).R,
                      Kind (Cbe_Request),
                      Block.Id (CBE.Request.Block_Number (Cbe_Request)),
                      Block.Count (CBE.Request.Count (Cbe_Request)),
                      I, Result);
                  case Result is
                     when Block.Success =>
                        C_Cache (I).C := Cbe_Request;
                        C_Cache (I).I := Data_Index;
                        if Block_Client.Kind (C_Cache (I).R) = Block.Write then
                           Block_Client.Write (Client, C_Cache (I).R);
                        end if;
                        Gns.Log.Client.Info
                           (Log, "Employ backend request at "
                                 & Image (Block_Client.Start (C_Cache (I).R)));
                        CBE.Library.IO_Request_In_Progress (Cbe_Session,
                                                            C_Cache (I).I);
                        Progress := True;
                     when Block.Unsupported =>
                        CBE.Library.IO_Request_In_Progress
                           (Cbe_Session, Data_Index);
                        CBE.Library.IO_Request_Completed
                           (Cbe_Session, Data_Index, False);
                     when others =>
                        null;
                  end case;
               end if;
            when Block.Allocated =>
               Block_Client.Enqueue (Client, C_Cache (I).R);
               Progress := Progress or else
                           Block_Client.Status (C_Cache (I).R) = Block.Pending;
               --  if Block_Client.Status (C_Cache (I).R) = Block.Pending then
               --     CBE.Library.IO_Request_In_Progress (Cbe_Session,
               --                                         C_Cache (I).I);
               --     Progress := True;
               --  end if;
            when Block.Pending =>
               Block_Client.Update_Request (Client, C_Cache (I).R);
               Progress := Progress or else Block_Client.Status
                  (C_Cache (I).R) in Block.Ok | Block.Error;
            when Block.Ok | Block.Error =>
               if
                  Block_Client.Status (C_Cache (I).R) = Block.Ok
                  and then Block_Client.Kind (C_Cache (I).R) = Block.Read
               then
                  Block_Client.Read (Client, C_Cache (I).R);
               end if;
               Gns.Log.Client.Info
                  (Log, "Received backend request at "
                        & Image (Block_Client.Start (C_Cache (I).R)));
               CBE.Library.IO_Request_Completed
                  (Cbe_Session, C_Cache (I).I,
                   Block_Client.Status (C_Cache (I).R) = Block.Ok);
               Block_Client.Release (Client, C_Cache (I).R);
               C_Cache (I).C := CBE.Request.Invalid_Object;
               Progress := True;
         end case;
      end loop;
   end Handle_Backend_Io;

   procedure Handle_Encryption (Progress : in out Boolean)
   is
      function Convert is new Ada.Unchecked_Conversion
         (CBE.Block_Data_Type, External.Crypto.Plain_Data_Type);
      Cbe_Request : CBE.Request.Object_Type;
      Index       : Request_Id;
      Success     : Boolean;
   begin
      --  Gns.Log.Client.Info (Log, "Handle_Encryption");
      for I in Ce_Cache'Range loop
         case Ce_Cache (I).S is
            when Empty =>
               CBE.Library.Crypto_Cipher_Data_Required
                  (Cbe_Session, Cbe_Request, Ce_Cache (I).I);
               if
                  CBE.Request.Valid (Cbe_Request)
                  and then
                  External.Crypto.Encryption_Request_Acceptable (Crypto)
               then
                  Progress := True;
                  Ce_Cache (I).S := Accepted;
                  Ce_Cache (I).R := CBE.Request.Valid_Object
                     (CBE.Request.Operation (Cbe_Request),
                      False,
                      CBE.Request.Block_Number (Cbe_Request),
                      CBE.Request.Offset (Cbe_Request),
                      CBE.Request.Count (Cbe_Request),
                      CBE.Request.Tag_Type (I));
                  Gns.Log.Client.Info
                     (Log, "Encrypt req: " & Image (C_Cache (I).I)
                           & " at " & Image (I));
                  External.Crypto.Submit_Encryption_Request
                     (Crypto, Ce_Cache (I).R,
                      Convert (Plain_Buffer
                         (CBE.Crypto.Item_Index_Type (Ce_Cache (I).I))));
                  CBE.Library.Crypto_Cipher_Data_Requested
                     (Cbe_Session, Ce_Cache (I).I);
                  Ce_Cache (I).S := Submitted;
               end if;
            when Accepted =>
               null;
            when Submitted =>
               null;
         end case;
      end loop;
      Cbe_Request :=
         External.Crypto.Peek_Completed_Encryption_Request (Crypto);
      if CBE.Request.Valid (Cbe_Request) then
         Gns.Log.Client.Info (Log, "Enc completed");
         Progress := True;
         Index := Request_Id (CBE.Request.Tag (Cbe_Request));
         declare
            procedure Collect (B : out External.Crypto.Cipher_Data_Type);
            procedure Collect (B : out External.Crypto.Cipher_Data_Type)
            is
            begin
               External.Crypto.Supply_Cipher_Data
                  (Crypto, Ce_Cache (Index).R, B, Success);
               CBE.Library.Supply_Crypto_Cipher_Data
                  (Cbe_Session,
                   CBE.Crypto.Cipher_Buffer_Index_Type (Ce_Cache (Index).I),
                   Success);
            end Collect;
            procedure Pass_Out is new Conversion.Pass_Out
               (CBE.Block_Data_Type,
                External.Crypto.Cipher_Data_Type,
                Collect);
         begin
            Pass_Out (Cipher_Buffer (CBE.Crypto.Item_Index_Type
                                       (Ce_Cache (Index).I)));
            Progress := Progress or else Success;
            Ce_Cache (Index).S := Empty;
         end;
      end if;
   end Handle_Encryption;

   procedure Handle_Decryption (Progress : in out Boolean)
   is
      function Convert is new Ada.Unchecked_Conversion
         (CBE.Block_Data_Type, External.Crypto.Cipher_Data_Type);
      Cbe_Request : CBE.Request.Object_Type;
      Index       : Request_Id;
      Success     : Boolean;
   begin
      --  Gns.Log.Client.Info (Log, "Handle_Decryption");
      for I in Cd_Cache'Range loop
         case Cd_Cache (I).S is
            when Empty =>
               CBE.Library.Crypto_Plain_Data_Required
                  (Cbe_Session, Cbe_Request, Cd_Cache (I).I);
               if
                  CBE.Request.Valid (Cbe_Request)
                  and then
                  External.Crypto.Decryption_Request_Acceptable (Crypto)
               then
                  Progress := True;
                  Cd_Cache (I).S := Accepted;
                  Cd_Cache (I).R := CBE.Request.Valid_Object
                     (CBE.Request.Operation (Cbe_Request),
                      False,
                      CBE.Request.Block_Number (Cbe_Request),
                      CBE.Request.Offset (Cbe_Request),
                      CBE.Request.Count (Cbe_Request),
                      CBE.Request.Tag_Type (I));
                  Gns.Log.Client.Info
                     (Log, "Decrypt req: " & Image (C_Cache (I).I)
                           & " at " & Image (I));
                  External.Crypto.Submit_Decryption_Request
                     (Crypto, Cd_Cache (I).R,
                      Convert (Cipher_Buffer
                         (CBE.Crypto.Item_Index_Type (Cd_Cache (I).I))));
                  CBE.Library.Crypto_Plain_Data_Requested
                     (Cbe_Session, Cd_Cache (I).I);
                  Cd_Cache (I).S := Submitted;
               end if;
            when Accepted =>
               null;
            when Submitted =>
               null;
         end case;
      end loop;
      Cbe_Request :=
         External.Crypto.Peek_Completed_Decryption_Request (Crypto);
      if CBE.Request.Valid (Cbe_Request) then
         Gns.Log.Client.Info (Log, "Dec completed");
         Progress := True;
         Index := Request_Id (CBE.Request.Tag (Cbe_Request));
         declare
            procedure Collect (B : out External.Crypto.Plain_Data_Type);
            procedure Collect (B : out External.Crypto.Plain_Data_Type)
            is
            begin
               Gns.Log.Client.Info
                  (Log, "Supply plain data");
               External.Crypto.Supply_Plain_Data
                  (Crypto, Cd_Cache (Index).R, B, Success);
               CBE.Library.Supply_Crypto_Plain_Data
                  (Cbe_Session,
                   CBE.Crypto.Plain_Buffer_Index_Type (Cd_Cache (Index).I),
                   Success);
            end Collect;
            procedure Pass_Out is new Conversion.Pass_Out
               (CBE.Block_Data_Type,
                External.Crypto.Plain_Data_Type,
                Collect);
         begin
            Pass_Out (Plain_Buffer (CBE.Crypto.Item_Index_Type
                                       (Cd_Cache (Index).I)));
            Progress := Progress or else Success;
            Cd_Cache (Index).S := Empty;
         end;
      end if;
   end Handle_Decryption;

   procedure Handle_Completed_Requests (Progress : in out Boolean)
   is
      use type Block.Request_Status;
      Cbe_Request : constant CBE.Request.Object_Type :=
         CBE.Library.Peek_Completed_Client_Request (Cbe_Session);
      Index       : Request_Id;
   begin
      if CBE.Request.Valid (Cbe_Request) then
         Progress := True;
         Index := Request_Id (CBE.Request.Tag (Cbe_Request));
         S_Cache (Index).S :=
            (if CBE.Request.Success (Cbe_Request) then Ok else Error);
         CBE.Library.Drop_Completed_Client_Request (Cbe_Session, Cbe_Request);
         Gns.Log.Client.Info (Log, "Dropped completed request");
      end if;
      for I in S_Cache'Range loop
         if
            Block_Server.Status (S_Cache (I).R) = Block.Pending
            and then S_Cache (I).S in Ok | Error
         then
            Block_Server.Acknowledge
               (Server,
                S_Cache (I).R,
                (if S_Cache (I).S = Ok then Block.Ok else Block.Error));
            if Block_Server.Status (S_Cache (I).R) = Block.Raw then
               Gns.Log.Client.Info
                  (Log, "Acknowledged request at "
                        & Image (Block_Server.Start (S_Cache (I).R)));
               Progress := True;
               S_Cache (I).S := Empty;
            end if;
         end if;
      end loop;
   end Handle_Completed_Requests;

   procedure Dispatch
      (I : in out Block.Dispatcher_Session;
       C :        Block.Dispatcher_Capability)
   is
   begin
      if Block_Dispatcher.Valid_Session_Request (I, C)
         and then not Initialized (Server)
         and then not Block.Initialized (Server)
      then
         Gns.Log.Client.Info (Log, "Connection request");
         Block_Dispatcher.Session_Initialize (I, C, Server, 42);
         if Initialized (Server) and then Block.Initialized (Server) then
            Gns.Log.Client.Info (Log, "Accepting connection request");
            Block_Dispatcher.Session_Accept (I, C, Server);
         end if;
      end if;
      Block_Dispatcher.Session_Cleanup (I, C, Server);
   end Dispatch;

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

   procedure Read (S : in out Block.Server_Session;
                   I :        Request_Id;
                   B :    out Buffer)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (I);
   begin
      Convert_Block
         (Plain_Buffer (Read_Index),
          B (B'First + Read_Offset .. B'First + Read_Offset + 4095));
   end Read;

   procedure Write (S : in out Block.Server_Session;
                    I :        Request_Id;
                    B :        Buffer)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (I);
      procedure Cbe_Write (Buf : CBE.Block_Data_Type);
      procedure Cbe_Write (Buf : CBE.Block_Data_Type)
      is
      begin
         CBE.Library.Supply_Client_Data (Cbe_Session,
                                         Write_Timestamp,
                                         Write_Request,
                                         Buf,
                                         Write_Progress);
      end Cbe_Write;
      procedure Pass is new Conversion.Pass_In
         (Block_Buffer, CBE.Block_Data_Type, Cbe_Write);
   begin
      Pass (B (B'First + Write_Offset .. B'First + Write_Offset + 4095));
   end Write;

end Component;
