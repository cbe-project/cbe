--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;
with CBE.Primitive;

package CBE.Splitter
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   --
   --  Curr_Primitive
   --
   function Curr_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   with
      Pre => (Request.Valid (Curr_Request (Obj)));

   --
   --  Initialize_Object
   --
   --  FIXME will not be used anymore when the library module is in spark
   --
   procedure Initialize_Object (Obj : out Object_Type)
   with
      Post => (Request_Acceptable (Obj));

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Number_Of_Primitives
   --
   function Number_Of_Primitives (Req : Request.Object_Type)
   return Number_Of_Primitives_Type
   with
      Pre => (Request.Valid (Req));

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   with
      Pre  => (Request_Acceptable (Obj) and then Request.Valid (Req)),
      Post => (not Request_Acceptable (Obj));

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Splitter.Object_Type)
   return Primitive.Object_Type;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (Obj : in out Object_Type);

   -----------------
   --  Accessors  --
   -----------------

   function Curr_Request (Obj : Object_Type) return Request.Object_Type;

private

   --
   --  Object_Type
   --
   type Object_Type is record
      Curr_Req    : Request.Object_Type;
      Curr_Blk_Nr : Block_Number_Type;
      Curr_Idx    : Primitive.Index_Type;
      Nr_Of_Prims : Number_Of_Primitives_Type;
   end record;

   --
   --  Reset_Curr_Req
   --
   procedure Reset_Curr_Req (Obj : out Object_Type);

   --
   --  Primitive_Taken
   --
   procedure Next_Primitive (Obj : in out Object_Type);

end CBE.Splitter;
