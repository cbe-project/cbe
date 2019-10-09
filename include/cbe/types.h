/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_TYPES_H_
#define _CBE_TYPES_H_

/* Genode includes */
#include <base/stdint.h>
#include <base/output.h>

namespace Cbe {

	using namespace Genode;

	/*
	 * Type definitions
	 *
	 * (Tp prevent implicit conversions better use a structured
	 *  type.)
	 */

	static constexpr uint32_t BLOCK_SIZE = 4096;

	using Number_of_primitives = size_t;

	enum {
		INVALID_GEN = 18446744073709551615ULL,
		INVALID_PBA = 18446744073709551615ULL,
		INVALID_VBA = 18446744073709551615ULL,
	};

	using Physical_block_address = uint64_t;
	using Virtual_block_address  = uint64_t;
	using Generation             = uint64_t;
	using Height                 = uint32_t;
	using Number_of_leaves       = uint64_t;
	using Number_of_leafs        = uint64_t;
	using Number_of_blocks       = uint64_t;
	using Degree                 = uint32_t;
	using Timestamp              = uint64_t;
	using Snapshot_index         = uint32_t;
	using Number_of_superblocks  = uint64_t;

	struct Index {
		enum { INVALID = 18446744073709551615ULL, };
		uint64_t value;
	};


	struct Timeout_request {
		bool      valid   { false };
		Timestamp timeout { 0 };
	};


	/*
	 * List of special tags used throughout the CBE.
	 *
	 * (The type is currently specified by
	 *  - bits [23:16] contain the meta-type (affiliation to meta-module)
	 *  - bits [15: 8] contain the type (affiliation to a module)
	 *  - bits [ 7: 0] contain the sub-type
	 *  but is more adhoc than really designed.)
	 */

	enum class Tag : Genode::uint32_t {
		INVALID_TAG        = 0x00,
		IO_TAG             = 0x10,
		CACHE_TAG          = 0x20,
		CACHE_FLUSH_TAG    = CACHE_TAG | 0x1,
		CRYPTO_TAG         = 0x30,
		CRYPTO_TAG_DECRYPT = CRYPTO_TAG | 0x1,
		CRYPTO_TAG_ENCRYPT = CRYPTO_TAG | 0x2,
		POOL_TAG           = 0x40,
		SPLITTER_TAG       = 0x50,
		TRANSLATION_TAG    = 0x60,
		WRITE_BACK_TAG     = 0x70,
		SYNC_SB_TAG        = 0x80,
		RECLAIM_TAG        = 0x90,

		VBD_TAG             = 0x100,
		VBD_CACHE_TAG       = VBD_TAG | CACHE_TAG,
		FREE_TREE_TAG       = 0x200,
		FREE_TREE_TAG_IO    = FREE_TREE_TAG | IO_TAG,
		FREE_TREE_TAG_CACHE = FREE_TREE_TAG | CACHE_TAG,
		FREE_TREE_TAG_WB    = FREE_TREE_TAG | WRITE_BACK_TAG,
	};


	/*
	 * The Request is a loose reimagination of the Block::Request
	 * type and is used to seperate the C++ and SPARK even more, e.g.,
	 * it is packed whereas the Block::Request is not.
	 *
	 * (It stands to reason if this type is strictly necessary by now as
	 *  it also lacks certain operations like TRIM.)
	 */
	struct Request
	{
		enum class Operation : uint32_t { INVALID = 0, READ = 1, WRITE = 2, SYNC = 3 };
		enum class Success   : uint32_t { FALSE = 0, TRUE = 1 };

		Operation operation;
		Success   success;
		uint64_t  block_number;
		uint64_t  offset;
		uint32_t  count;
		uint32_t  tag;

		bool read()  const { return operation == Operation::READ; }
		bool write() const { return operation == Operation::WRITE; }
		bool sync()  const { return operation == Operation::SYNC; }

		bool valid() const
		{
			return operation == Operation::READ
			    || operation == Operation::WRITE
			    || operation == Operation::SYNC;
		}

		bool equal(Request const &rhs) const
		{
			return tag          == rhs.tag
			    && block_number == rhs.block_number
			    && operation    == rhs.operation;
		}

		/* debug */
		void print(Genode::Output &out) const
		{
			if (!valid()) {
				Genode::print(out, "<invalid>");
				return;
			}
			Genode::print(out, "tag: ", tag);

			Genode::print(out, " block_number: ", block_number);
			Genode::print(out, " count: ", count);
			Genode::print(out, " offset: ", offset);
			Genode::print(out, " op: ");
			switch (operation) {
			case Operation::READ:  Genode::print(out, "READ"); break;
			case Operation::WRITE: Genode::print(out, "WRITE"); break;
			case Operation::SYNC:  Genode::print(out, "SYNC"); break;
			case Operation::INVALID: [[fallthrough]]
			default: Genode::print(out, "INVALID"); break;
			}
			Genode::print(out, " success: ");
			switch (success) {
			case Success::FALSE: Genode::print(out, "no"); break;
			case Success::TRUE:  Genode::print(out, "yes"); break;
			}
		}
	} __attribute__((packed));


	/*
	 * The Primitive is the primary data structure within the CBE
	 * and encapsulates a CBE operation.
	 */
	struct Primitive
	{
		using Number = uint64_t;
		using Index  = uint64_t;

		enum class Operation : uint32_t { INVALID, READ, WRITE, SYNC };
		enum class Success   : uint32_t { FALSE, TRUE };

		Tag tag;

		Operation operation;
		Success   success;

		Number block_number;
		Index  index;

		bool read()  const { return operation == Operation::READ; }
		bool write() const { return operation == Operation::WRITE; }
		bool sync()  const { return operation == Operation::SYNC; }

		bool valid() const
		{
			return operation == Operation::READ
			    || operation == Operation::WRITE
			    || operation == Operation::SYNC;
		}

		bool equal(Primitive const &rhs) const
		{
			return tag          == rhs.tag
			    && block_number == rhs.block_number
			    && operation    == rhs.operation;
		}

		/* debug */
		void print(Genode::Output &out) const
		{
			if (!valid()) {
				Genode::print(out, "<invalid>");
				return;
			}

			auto tag_string = [](Tag const tag) {
				switch (tag) {
				case Tag::INVALID_TAG: return "INVALID_TAG";
				case Tag::IO_TAG: return "IO_TAG";
				case Tag::CACHE_TAG: return "CACHE_TAG";
				case Tag::CACHE_FLUSH_TAG: return "CACHE_FLUSH_TAG";
				case Tag::CRYPTO_TAG: return "CRYPTO_TAG";
				case Tag::CRYPTO_TAG_DECRYPT: return "CRYPTO_TAG_DECRYPT";
				case Tag::CRYPTO_TAG_ENCRYPT: return "CRYPTO_TAG_ENCRYPT";
				case Tag::POOL_TAG: return "POOL_TAG";
				case Tag::SPLITTER_TAG: return "SPLITTER_TAG";
				case Tag::TRANSLATION_TAG: return "TRANSLATION_TAG";
				case Tag::WRITE_BACK_TAG: return "WRITE_BACK_TAG";
				case Tag::SYNC_SB_TAG: return "SYNC_SB_TAG";
				case Tag::RECLAIM_TAG: return "RECLAIM_TAG";

				case Tag::VBD_TAG: return "VBD_TAG";
				case Tag::VBD_CACHE_TAG: return "VBD_CACHE_TAG";
				case Tag::FREE_TREE_TAG: return "FREE_TREE_TAG";
				case Tag::FREE_TREE_TAG_IO: return "FREE_TREE_TAG_IO";
				case Tag::FREE_TREE_TAG_CACHE: return "FREE_TREE_TAG_CACHE";
				case Tag::FREE_TREE_TAG_WB: return "FREE_TREE_TAG_WB";
				}
				return "<invalid>";
			};
			Genode::print(out, "tag: ", tag_string(tag));

			Genode::print(out, " block_number: ", block_number);
			Genode::print(out, " index: ", index);
			Genode::print(out, " op: ");
			switch (operation) {
			case Operation::READ:  Genode::print(out, "READ"); break;
			case Operation::WRITE: Genode::print(out, "WRITE"); break;
			case Operation::SYNC:  Genode::print(out, "SYNC"); break;
			case Operation::INVALID: [[fallthrough]]
			default: break;
			}
			Genode::print(out, " success: ");
			switch (success) {
			case Success::FALSE: Genode::print(out, "no"); break;
			case Success::TRUE:  Genode::print(out, "yes"); break;
			}
		}
	} __attribute__((packed));

	/*
	 * The Tree_helper makes the information about used
	 * tree available.
	 */
	enum {
		TREE_MIN_DEGREE = 1,
		TREE_MIN_HEIGHT = 1,
		TREE_MAX_HEIGHT = 6,
	};
	struct Tree_helper
	{
		struct Object_size_mismatch { };

		static inline uint32_t _log2(uint32_t const value)
		{
			if (!value) { return -1; }

			for (int i = 8 * sizeof(value) - 1; i >= 0; --i) {
				if (((uint32_t)1 << i) & value) { return i; }
			}

			return -1; 
		}

		static constexpr Genode::uint32_t bytes() { return sizeof(Tree_helper); }

		Degree const _degree;
		Height const _height;
		Number_of_leaves const _leafs;

		Degree const _degree_log2 { _log2(_degree) };
		Degree const _degree_mask { (1u << _degree_log2) - 1 };

		Tree_helper(Degree           const degree,
		            Height           const height,
		            Number_of_leaves const leafs)
		: _degree(degree), _height(height), _leafs(leafs) { }

		uint32_t index(Virtual_block_address const vba,
		               uint32_t                   const level) const
		{
			return (vba >> (_degree_log2 * (level - 1)) & _degree_mask);
		}

		Height height()          const { return _height; }
		Degree degree()          const { return _degree; }
		Number_of_leaves leafs() const { return _leafs; }
	};


	/*
	 * The Block_data encapsulates the data of a complete on
	 * disk sector.
	 */
	struct Block_data
	{
		char values[BLOCK_SIZE];

		/* debug */
		void print(Genode::Output &out) const
		{
			using namespace Genode;
			for (char const c : values) {
				Genode::print(out, Hex(c, Hex::OMIT_PREFIX, Hex::PAD), " ");
			}
			Genode::print(out, "\n");
		}
	} __attribute__((packed));

	class Crypto_plain_buffer
	{
		private:

			Block_data items[1];

		public:

			struct Bad_index : Genode::Exception { };

			struct Index
			{
				uint32_t value;

				explicit Index(uint32_t value) : value(value) { }

			} __attribute__((packed));

			Block_data &item(Index const idx)
			{
				if (idx.value >= sizeof(items) / sizeof(items[0])) {
					throw Bad_index();
				}
				return items[idx.value];
			}

	} __attribute__((packed));


	class Crypto_cipher_buffer
	{
		private:

			Block_data items[1];

		public:

			struct Bad_index : Genode::Exception { };

			struct Index
			{
				uint32_t value;

				explicit Index(uint32_t value) : value(value) { }

			} __attribute__((packed));

			Block_data &item(Index const idx)
			{
				if (idx.value >= sizeof(items) / sizeof(items[0])) {
					throw Bad_index();
				}
				return items[idx.value];
			}
	} __attribute__((packed));


	/*
	 * The Hash contains the hash of a node.
	 */
	struct Hash
	{
		enum { MAX_LENGTH = 32, };
		char values[MAX_LENGTH];

		/* hash as hex value plus "0x" prefix and terminating null */
		using String = Genode::String<sizeof(values) * 2 + 3>;

		/* debug */
		void print(Genode::Output &out) const
		{
			using namespace Genode;
			Genode::print(out, "0x");
			bool leading_zero = true;
			for (char const c : values) {
				if (leading_zero) {
					if (c) {
						leading_zero = false;
						Genode::print(out, Hex(c, Hex::OMIT_PREFIX));
					}
				} else {
					Genode::print(out, Hex(c, Hex::OMIT_PREFIX, Hex::PAD));
				}
			}
			if (leading_zero) {
				Genode::print(out, "0");
			}
		}
	};


	/*
	 * The Key contains the key-material that is used to
	 * process cipher-blocks.
	 *
	 * (For now it is not used but the ID field is already referenced
	 *  by type 2 nodes.)
	 */
	struct Key
	{
		enum { KEY_SIZE = 64u };
		char value[KEY_SIZE];

		struct Id { uint32_t value; };
		Id id;

		using String = Genode::String<sizeof(value) * 2 + 3>;

		/* debug */
		void print(Genode::Output &out) const
		{
			using namespace Genode;
			Genode::print(out, "[", id.value, ", ");
			for (char const c : value) {
				Genode::print(out, Hex(c, Hex::OMIT_PREFIX, Hex::PAD));
			}
			Genode::print(out, "]");
		}
	} __attribute__((packed));


	/*
	 * The Snapshot stores the information about given tree within
	 * the CBE.
	 */
	struct Snapshot
	{
		struct /* Snapshot_info */{
			Hash                   hash;
			Physical_block_address pba;
			Generation             gen;
			Number_of_leaves       leaves;
			Height                 height;
			uint8_t                _valid;
		};
		uint32_t id;
		enum {
			FLAGS_CLEAR = 0u,
			FLAG_KEEP   = 1u<<0,
		};
		uint32_t flags;

		bool keep()  const { return flags & FLAG_KEEP; }

		bool valid() const
		{
			struct Bad_value : Exception { };
			if (_valid == 1) {
				return true;
			} else if (_valid == 0) {
				return false;
			} else {
				throw Bad_value();
			}
		}

		void valid(bool v)
		{
			if (v) {
				_valid = 1;
			} else {
				_valid = 0;
			}
		}

		/* debug */
		void print(Genode::Output &out) const
		{
			if (!valid()) {
				Genode::print(out, "<invalid>");
				return;
			}

			Genode::print(out, "id: ", id, " gen: ", gen,
			              " pba: ", pba, " leafs: ", leaves,
			              " height: ", height, " hash: <", hash, ">");
		}
	} __attribute__((packed));


	/*
	 * The Superblocks_index
	 *
	 * (It stands to reason if the type is needed.)
	 */
	struct Superblocks_index
	{
		uint64_t value;

		Superblocks_index(uint64_t val) : value(val) { }

		void print(Genode::Output &out) const
		{
			Genode::print(out, value);
		}
	};


	/*
	 * The Superblock contains all information of a CBE
	 * instance including the list of active snapshots. For now
	 * the super-blocks are stored consecutively at the beginning
	 * of the block device, i.e., there is a 1:1 mapping between
	 * the physical-block-address and the SB id.
	 *
	 * Per super-block we have a fixed number of snapshots (about
	 * the amount we can store within one disk sector). Whenever
	 * a generation is sealed, a new snapshot will be created
	 * automatically. If a snapshot is flagged as KEEP, it will never
	 * be overriden.
	 */
	enum : Snapshot_index        { NUM_SNAPSHOTS = 48 };
	enum : Number_of_superblocks { NUM_SUPER_BLOCKS = 8 };

	struct Superblock
	{
		enum { NUM_KEYS = 2u };
		enum { INVALID_SNAPSHOT_SLOT = NUM_SNAPSHOTS, };

		// XXX w/o snapshots about 265 bytes,
		//     snapshots about 68 bytes each, all in all 3529 bytes
		Key key[NUM_KEYS];

		/*
		 * (At the moment we just check the active snapshots of
		 *  the active super-block but should it not make sense
		 *  to iterate overall super-blocks when trying to determine
		 *  if a block may be safely freed? Because if the most
		 *  recent SB is corrupted and we try to use an older one,
		 *  chances are that the snapshot in the corrupt SB has
		 *  reused blocks reference by a snapshot in the older SB.)
		 */
		Snapshot               snapshots[NUM_SNAPSHOTS];
		Generation             last_secured_generation;
		Snapshot_index         snapshot_id;
		Degree                 degree;
		Generation             free_gen;
		Physical_block_address free_number;
		Hash                   free_hash;
		Height                 free_height;
		Degree                 free_degree;
		Number_of_leaves       free_leaves;
		char                   padding[424];

		/**
		 * Get index into snapshot array for the last snapshot
		 *
		 * \return  if found the slot number is returned, otherwise
		 *          a invalid number
		 */
		Snapshot_index snapshot_slot() const
		{
			Snapshot_index snap_slot = INVALID_SNAPSHOT_SLOT;
			for (Snapshot_index i = 0; i < NUM_SNAPSHOTS; i++) {
				Snapshot const &snap = snapshots[i];
				if (!snap.valid()) { continue; }

				if (snap.id == snapshot_id) {
					snap_slot = i;
					break;
				}
			}
			return snap_slot;
		}

		bool valid() const
		{
			return last_secured_generation != INVALID_GEN;
		}

	} __attribute__((packed));

	static_assert(sizeof(Superblock) == BLOCK_SIZE);


	struct Superblocks
	{
		Superblock block[NUM_SUPER_BLOCKS];

	} __attribute__((packed));

	static_assert(
		sizeof(Superblocks) == NUM_SUPER_BLOCKS * BLOCK_SIZE);

	/*
	 * (Strictly speaking the following node types are not the
	 * node itself but just defined a entry in the node. It
	 * would be better to have a Type_1_node that contains a
	 * Type_1_node_entry array.)
	 */

	/*
	 * The Type_i_node contains the on-disk type 1 inner node
	 * information. This node is the primary tree node and as such
	 * used by the virtual-block-device as well as the free-tree.
	 *
	 * In case of the VBD its leaf nodes point to the physical
	 * on disk sectors.
	 */
	struct Type_i_node
	{
		enum { MAX_NODE_SIZE = 64u, };

		Physical_block_address pba;
		Generation             gen;
		Hash                   hash;
		char                        padding[16];

	} __attribute__((packed));

	static_assert(sizeof(Type_i_node) == Type_i_node::MAX_NODE_SIZE);

	constexpr size_t TYPE_1_PER_BLOCK = BLOCK_SIZE / sizeof (Type_i_node);


	/*
	 * The Type_1_node_info contains the in-memory type 1 node
	 * information.
	 */
	struct Type_1_node_info
	{
		Physical_block_address pba;
		Generation             gen;
		Hash                   hash;
	};


	/*
	 * The Type_i_node contains the on-disk type 2 inner node
	 * information. This node is only used in the free-tree at the
	 * level directly above the leaf nodes.
	 */
	struct Type_ii_node
	{
		enum { MAX_NODE_SIZE = 64u, };

		Physical_block_address pba;
		Virtual_block_address  last_vba;
		Generation             alloc_gen;
		Generation             free_gen;
		Key::Id                last_key_id;
		bool                   reserved;
		char                   padding[27];

	} __attribute__((packed));

	static_assert(sizeof (Type_ii_node) == Type_ii_node::MAX_NODE_SIZE);

	constexpr size_t TYPE_2_PER_BLOCK = BLOCK_SIZE / sizeof (Type_ii_node);

} /* namespace Cbe */

#endif /* _CBE_TYPES_H_ */
