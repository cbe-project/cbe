--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Splitter
with SPARK_Mode
is
   --
   --  Curr_Primitive
   --
   function Curr_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (
      Primitive.Valid_Object (
         Request.Operation (Obj.Curr_Req),
         Request.Success (Obj.Curr_Req),
         Request.Tag (Obj.Curr_Req),
         Obj.Curr_Blk_Nr,
         Obj.Curr_Idx));

   --
   --  Reset_Curr_Req
   --
   procedure Reset_Curr_Req (Obj : out Object_Type)
   is
   begin
      Obj.Curr_Req    := Request.Invalid_Object;
      Obj.Curr_Blk_Nr := 0;
      Obj.Curr_Idx    := 0;
      Obj.Nr_Of_Prims := 0;
   end Reset_Curr_Req;

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
      Curr_Req    => Request.Invalid_Object,
      Curr_Blk_Nr => 0,
      Curr_Idx    => 0,
      Nr_Of_Prims => 0);

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   is
   begin

      Obj := (
         Curr_Req    => Req,
         Curr_Blk_Nr => Request.Block_Number (Req),
         Curr_Idx    => 0,
         Nr_Of_Prims => Number_Of_Primitives_Type (Request.Count (Req)));

   end Submit_Request;

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean
   is (not Request.Valid (Obj.Curr_Req));

   --
   --  Primitive_Taken
   --
   procedure Next_Primitive (Obj : in out Object_Type)
   is
      use type Primitive.Index_Type;
   begin
      Obj.Curr_Blk_Nr := Obj.Curr_Blk_Nr + 1;
      Obj.Curr_Idx    := Obj.Curr_Idx    + 1;

      if Number_Of_Primitives_Type (Obj.Curr_Idx) = Obj.Nr_Of_Prims then
         Reset_Curr_Req (Obj);
      end if;
   end Next_Primitive;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (Obj : in out Object_Type)
   is
   begin
      Next_Primitive (Obj);
   end Drop_Generated_Primitive;

   --
   --  Number_Of_Primitives
   --
   function Number_Of_Primitives (Req : Request.Object_Type)
   return Number_Of_Primitives_Type
   is (
      Number_Of_Primitives_Type (Request.Count (Req)));

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Splitter.Object_Type)
   return Primitive.Object_Type
   is (
      case Request.Valid (Obj.Curr_Req) is
      when False => Primitive.Invalid_Object,
      when True  => Curr_Primitive (Obj));

   -----------------
   --  Accessors  --
   -----------------

   function Curr_Request (Obj : Object_Type) return Request.Object_Type
   is (Obj.Curr_Req);

end CBE.Splitter;
