with Componolit.Gneiss.Types;
with Componolit.Gneiss.Component;
with Componolit.Gneiss.Block;
with Componolit.Gneiss.Block.Client;
with Componolit.Gneiss.Block.Dispatcher;
with Componolit.Gneiss.Block.Server;
with Componolit.Gneiss.Timer;
with Componolit.Gneiss.Timer.Client;
with CBE;

package Component is

   package Gns renames Componolit.Gneiss;

   procedure Construct (Cap : Gns.Types.Capability);
   procedure Destruct;

   package Main is new Gns.Component (Construct, Destruct);

   type Unsigned_Long is new Long_Integer range 0 .. Long_Integer'Last;

   type Buffer is array (Unsigned_Long range <>) of CBE.Byte_Type;

   type Request_Id is new Long_Integer range 0 .. 2**32 - 1;

   package Block is new Gns.Block (CBE.Byte_Type, Unsigned_Long, Buffer,
       Integer, Request_Id);

   procedure Event;

   procedure Write
      (C : in out Block.Client_Session;
       I :        Request_Id;
       D :    out Buffer) with
      Pre => Block.Initialized (C);

   procedure Read
      (C : in out Block.Client_Session;
       I :        Request_Id;
       D :        Buffer) with
      Pre => Block.Initialized (C);

   package Timer_Client is new Gns.Timer.Client (Event);
   package Block_Client is new Block.Client (Event, Read, Write);

   procedure Dispatch
      (I : in out Block.Dispatcher_Session;
       C :        Block.Dispatcher_Capability) with
      Pre => Block.Initialized (I) and then not Block.Accepted (I);
   function Initialized
      (S : Block.Server_Session)
      return Boolean;
   procedure Initialize_Server
      (S : in out Block.Server_Session;
       L :        String;
       B :        Block.Byte_Length) with
      Pre => not Initialized (S);
   procedure Finalize_Server (S : in out Block.Server_Session) with
      Pre => Initialized (S);
   function Block_Count
      (S : Block.Server_Session)
      return Block.Count with
      Pre => Initialized (S);
   function Block_Size
      (S : Block.Server_Session)
      return Block.Size with
      Pre => Initialized (S);
   function Writable
      (S : Block.Server_Session)
      return Boolean with
      Pre => Initialized (S);

   package Block_Server is new Block.Server (Event, Block_Count, Block_Size,
       Writable, Initialized, Initialize_Server, Finalize_Server);
   package Block_Dispatcher is new Block.Dispatcher (Block_Server, Dispatch);

end Component;
