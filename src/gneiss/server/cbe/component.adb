with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
--  with Componolit.Gneiss.Strings;
with Componolit.Gneiss.Strings_Generic;
with Cbe.Library;

package body Component is

   use type Block.Size;

   subtype Block_Buffer is Buffer (1 .. 4096);
   type Superblock_Request_Cache is
      array (Request_Id range 0 .. 7) of Block_Client.Request;
   type Superblocks_Initialized is
      array (Cbe.Super_Blocks_Index_Type) of Boolean;

   Cbe_Block_Size      : constant Block.Size := 4096;
   Virtual_Block_Count : Block.Count         := 0;

   function Image is new Gns.Strings_Generic.Image_Ranged (Block.Size);
   --  function Image is new Gns.Strings_Generic.Image_Modular (Block.Id);
   --  function Image is new Gns.Strings_Generic.Image_Ranged (Cbe.Super_Blocks_Index_Type);

   Dispatcher  : Block.Dispatcher_Session;
   Client      : Block.Client_Session;
   Server      : Block.Server_Session;
   Log         : Gns.Log.Client_Session;
   Capability  : Gns.Types.Capability;
   Ready       : Boolean  := False;
   Sb_Alloc    : Block.Id := 0;
   Cbe_Session : Cbe.Library.Object_Type;
   Client_Act  : Boolean  := False;

   procedure Read_Superblock with
      Pre => Block.Initialized (Client)
      and then Gns.Log.Initialized (Log);

   Sbsr : Superblock_Request_Cache;
   --  This should be a variable inside Read_Superblock. But since the Request type
   --  on contains the block itself and we have limited stack space there it resides in the package.
   Superblocks      : Cbe.Super_Blocks_Type;
   Superblocks_Init : Superblocks_Initialized :=
      (others => False);

   procedure Read_Superblock is
      use type Block.Id;
      use type Block.Request_Status;
      use type Cbe.Generation_Type;
      use type Cbe.Virtual_Block_Address_Type;
      Result   : Block.Result;
      Max_Vba  : Cbe.Virtual_Block_Address_Type;
      Progress : Boolean                     := True;
      Current  : Cbe.Super_Blocks_Index_Type := 0;
      Last_Gen : Cbe.Generation_Type         := 0;
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
            exit when not Progress or
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
               Cbe.Generation_Type'Last
               and then Superblocks (I).Last_Secured_Generation >=
                  Last_Gen
            then
               Valid    := True;
               Current  := I;
               Last_Gen := Superblocks (I).Last_Secured_Generation;
            end if;
         end loop;
         if Valid then
            Cbe.Library.Initialize_Object
               (Cbe_Session, Superblocks, Current);
            Max_Vba := Cbe.Library.Max_Vba (Cbe_Session);
            if Max_Vba < Cbe.Virtual_Block_Address_Type'Last
               and then Max_Vba <
                  Cbe.Virtual_Block_Address_Type (Block.Count'Last)
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
   begin
      if Ready then
         null;
      else --  reading superblocks
         if D'Length = 4096
            and then Block_Client.Status (Sbsr (I)) = Block.Ok
            and then Block_Client.Start (Sbsr (I)) in 0 .. 7
         then
            declare
               S : constant Cbe.Super_Blocks_Index_Type :=
                  Cbe.Super_Blocks_Index_Type
                     (Block_Client.Start (Sbsr (I)));
               B : Block_Buffer with
                  Address => Superblocks (S)'Address;
            begin
               B                    := D;
               Superblocks_Init (S) := True;
            end;
         end if;
      end if;
   end Read;

   procedure Event is
      Progress : Boolean := True;
   begin
      Gns.Log.Client.Info (Log, "Event");
      while Progress loop
         if Ready then
            Progress := False;
         else
            Read_Superblock;
            Progress := Ready;
         end if;
      end loop;
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
