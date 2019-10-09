--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

--
--  The Write_back module is used to update the virtual-block-device in
--  a CoW fashion whenever new data is written to the device.
--
--  It operates in the following steps:
--    1. (CRYPTO)   it hands the leaf data to the Crypto module for encryption
--    2. (IO)       it hands the encrypted leaf data to I/O module to write it
--                  to the block device
--    3. (CACHE)    starting by the lowest inner node it will update the node
--                  entry (pba and hash)
--    4. (COMPLETE) it returns the new root pba and root hash
--
--  To reduce the tag handling in the arbiter this module contains methods
--  necessary for each step so that the arbiter simply polls these ones.
--

pragma Ada_2012;

with CBE.Tree_Helper;
with CBE.Primitive;

package CBE.Write_Back
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   type Data_Index_Type is range 0 .. 0;

   type Data_Type
   is array (Data_Index_Type) of Block_Data_Type
   with Size => Block_Size * 8 * 1;

   --
   --  Initialize_Object
   --
   --  FIXME will not be used anymore when the library module is in spark
   --
   procedure Initialize_Object (Obj : out Object_Type);

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Update inner node
   --
   --  \param  PBA          physical-block-address of the inner node in
   --                       question
   --  \param  Tree         tree-helper which is needed to calculate the the
   --                       entry index
   --  \param  Data         reference to the data of the old block which we
   --                       might need to copy
   --  \param  Update_Data  reference to the current data we are going to
   --                       update
   --
   procedure Update (
      Obj         : in out Object_Type;
      PBA         :        Physical_Block_Address_Type;
      Tree        :        Tree_Helper.Object_Type;
      Data        :        Block_Data_Type;
      Update_Data : in out Block_Data_Type);

   --
   --  Check if the module can accept a primitive
   --
   --  \return true if a primitive can be accepted, otherwise false
   --
   function Primitive_Acceptable (Obj : Object_Type) return Boolean;

   type New_PBAs_Type
   is array (Tree_Level_Index_Type'Range) of Physical_Block_Address_Type;

   --
   --  Submit a new primitive
   --
   --  This method must only be called after executing 'Primitive_Acceptable'
   --  returned true.
   --
   --  \param Prim     referencet to the primitive
   --  \param Gen      new generation that is used for the updated branch in
   --                  the VBD
   --  \param VBA      virtual-block-address the leaf node is referenced by
   --  \param New_PBAs list of new PBA
   --  \param Old_PBAs list of old PBA
   --  \param N        number of valid items in the new and old PBA lists
   --  \param Data     reference to the data that should be written to the new
   --                  leaf node
   --  \param WB_Data  reference to the location where the new data is stored
   --
   procedure Submit_Primitive (
      Obj                   : in out Object_Type;
      Prim                  :        Primitive.Object_Type;
      Gen                   :        Generation_Type;
      VBA                   :        Virtual_Block_Address_Type;
      New_PBAs              :        New_PBAs_Type;
      Old_PBAs              :        Type_1_Node_Infos_Type;
      N                     :        Tree_Level_Index_Type;
      Data                  :        Block_Data_Type;
      WB_Data               :    out Data_Type);

   --
   --  Check for any completed primitive
   --
   --  The method will always a return a primitive and the caller
   --  always has to check if the returned primitive is in fact a
   --  valid one.
   --
   --  \return a valid Primitive will be returned if there is an
   --         completed primitive, otherwise an invalid one
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Get root physical-block-address of completed primitive
   --
   --  This method must only be called after 'peek_completed_primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to the completed primitive
   --
   --  \return root physical-block-address
   --
   function Peek_Completed_Root (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Physical_Block_Address_Type;

   --
   --  Get root physical-block-address of completed primitive
   --
   --  This method must only be called after 'peek_completed_primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to the completed primitive
   --  \param Hash  reference to location where to store the hash
   --
   procedure Peek_Completed_Root_Hash (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type;
      Hash : out Hash_Type);

   --
   --  Discard given completed primitive
   --
   --  This method must only be called after 'peek_completed_primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to primitive
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Crypto
   --

   --
   --  Check for a generated crypto primitive
   --
   --  The method will always a return a primitive and the caller
   --  always has to check if the returned primitive is in fact a
   --  valid one.
   --
   --  \return a valid Primitive will be returned if there is an
   --         generated primitive pending, otherwise an invalid one
   --
   function Peek_Generated_Crypto_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Get index for the data block of the crypto primitive
   --
   --  This method must only be called after 'Peek_Generated_Crypto_Primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to the completed primitive
   --
   --  \return index for data block
   --
   function Peek_Generated_Crypto_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Data_Index_Type;

   --
   --  Discard given generated primitive
   --
   --  This method must only be called after 'Peek_Generated_Crypto_Primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to primitive
   --
   procedure Drop_Generated_Crypto_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark given generated crypto primitive as complete
   --
   --  \param  Prim         reference to primitive
   --  \param  Crypto_Data  reference to encrypted for creating the hash
   --
   procedure Mark_Completed_Crypto_Primitive (
      Obj         : in out Object_Type;
      Prim        :        Primitive.Object_Type;
      Crypto_Data :        Block_Data_Type);

   --
   --  I/O
   --

   --
   --  Check for a generated I/O primitive
   --
   --  The method will always a return a primitive and the caller
   --  always has to check if the returned primitive is in fact a
   --  valid one.
   --
   --  \return a valid Primitive will be returned if there is an
   --         generated primitive pending, otherwise an invalid one
   --
   function Peek_Generated_IO_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Get index for the data block of the crypto primitive
   --
   --  This method must only be called after 'peek_generated_io_primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to the completed primitive
   --
   --  \return index for data block
   --
   function Peek_Generated_IO_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Data_Index_Type;

   --
   --  Discard given generated I/O primitive
   --
   --  This method must only be called after 'peek_generated_io_primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to primitive
   --
   procedure Drop_Generated_IO_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark given generated I/O primitive as complete
   --
   --  \param Prim  reference to primitive
   --
   procedure Mark_Completed_IO_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Cache
   --

   --
   --  Check for a generated cache primitive
   --
   --  The method will always a return a primitive and the caller
   --  always has to check if the returned primitive is in fact a
   --  valid one.
   --
   --  \return a valid Primitive will be returned if there is an
   --         generated primitive pending, otherwise an invalid one
   --
   function Peek_Generated_Cache_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Get physical-block-address for the update node
   --
   --  This method must only be called after 'Peek_Generated_Cache_Primitive'
   --  returned a valid primitive.
   --
   --  The old PBA simply is the 'block_number' of the generated cache
   --  primitive, the potentially new one used for updating later on must be
   --  requested directly.
   --
   --  \param Prim  reference to the completed primitive
   --
   --  \return physical-block-address
   --
   function Peek_Generated_Cache_Update_PBA (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   return Physical_Block_Address_Type;

   --
   --  Discard given generated cache primitive
   --
   --  This method must only be called after 'peek_generated_cache_primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to primitive
   --
   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

private

   type Entry_State_Type is (Invalid, Pending, In_Progress, Complete);

   type Entry_Type is record
      PBA        : Physical_Block_Address_Type;
      Update_PBA : Physical_Block_Address_Type;
      State      : Entry_State_Type;
      Tag        : Tag_Type;
   end record;

   --
   --  entry 0               -> leaf node
   --  entry 1 ... entry N-1 -> inner node
   --  entry N-1             -> root node
   --
   type Entries_Type is array (Tree_Level_Index_Type'Range) of Entry_Type;
   type Hashes_Type  is array (Tree_Level_Index_Type'Range) of Hash_Type;

   type State_Type is (Invalid, Crypto, IO, Cache, Complete);

   --
   --  Object_Type
   --
   type Object_Type is record
      Entries           : Entries_Type;
      Hashes            : Hashes_Type;
      Levels            : Tree_Level_Index_Type;
      State             : State_Type;
      Pending_Primitive : Primitive.Object_Type;
      Pending_Failure   : Boolean;
      VBA               : Virtual_Block_Address_Type;
      New_Generation    : Generation_Type;
   end record;

   procedure Fail_If_Pending_Primitive_Not_Complete (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type);

   --
   --  The method is called for all entries by the specific
   --  'Peek_Generated_*_Primitve' method, which contains the
   --  state guard.
   --
   function Peek_Generated_Leaf_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  The method is called for all entries by the specific
   --  'Peek_Generated_*_Data' method, which contains the
   --  state guard.
   --
   function Peek_Generated_Leaf_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Data_Index_Type;

   --
   --  The method is called for all entries by the specific
   --  'drop_generated_*_primitive' method, which contains the
   --  state guard.
   --
   procedure Drop_Generated_Leaf_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   procedure Mark_Entry_As_In_Progress (
      Prim :        Primitive.Object_Type;
      E    : in out Entry_Type);

end CBE.Write_Back;
