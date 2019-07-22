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

	enum class Tag : Genode::uint32_t {
		INVALID_TAG        = 0x00,
		IO_TAG             = 0x10,
		CACHE_TAG          = 0x20,
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
		FREE_TREE_TAG       = 0x200,
		FREE_TREE_TAG_IO    = FREE_TREE_TAG | IO_TAG,
		FREE_TREE_TAG_CACHE = FREE_TREE_TAG | CACHE_TAG,
		FREE_TREE_TAG_WB    = FREE_TREE_TAG | WRITE_BACK_TAG,
	};

	using Number_of_primitives = size_t;

	struct Request
	{
		enum class Operation : Genode::uint32_t { INVALID = 0, READ = 1, WRITE = 2, SYNC = 3 };
		enum class Success   : Genode::uint32_t { FALSE = 0, TRUE = 1 };

		Operation         operation;
		Success           success;
		Genode::uint64_t  block_number;
		Genode::uint64_t  offset;
		Genode::uint32_t  count;
		Genode::uint32_t  tag;

		bool operation_defined() const
		{
			return operation == Operation::READ
				|| operation == Operation::WRITE
				|| operation == Operation::SYNC;
		}

	} __attribute__((packed));

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

		bool equal(Cbe::Primitive const &rhs) const
		{
			return tag == rhs.tag
			    && block_number == rhs.block_number
			    && operation == rhs.operation;
		}
	} __attribute__((packed));

	enum { INVALID_PBA = 18446744073709551615ULL, };
	using Physical_block_address = uint64_t;
	using Virtual_block_address  = uint64_t;
	using Generation             = uint64_t;
	using Height                 = uint32_t;
	using Number_of_leaves       = uint64_t;
	using Degree                 = uint32_t;

	using Timestamp = uint64_t;

	enum { BLOCK_SIZE = 4096u };

	struct Block_data
	{
		char values[BLOCK_SIZE] { };
	} __attribute__((packed));

	struct Index
	{
		uint64_t value;
	};

	struct Hash
	{
		char values[32];

		/* hash as hex value plus "0x" prefix and terminating null */
		using String = Genode::String<sizeof(values) * 2 + 3>;

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

	struct Key_id
	{
		uint32_t value;
	};

	struct Key
	{
		enum { KEY_SIZE = 64u };
		char value[KEY_SIZE];
		Key_id id;
	};

	enum { NUM_SUPER_BLOCKS = 8, };

	struct Ro_snapshot
	{
		Physical_block_address root;
		Hash                   hash;
		Generation             generation;
		Height                 height;
		Number_of_leaves       leaves;
	} __attribute__((packed));

	struct Snapshot
	{
		Ro_snapshot ro_info;
		bool hold;
	} __attribute__((packed));

	struct Super_block
	{
		enum { NUM_KEYS = 2u };

		union {
			struct {
				Key key[NUM_KEYS];

				Snapshot snapshots[16];

				Physical_block_address root_number;
				Hash                   root_hash;
				union {
					Generation             generation;
					Generation             root_gen;
				};
				Height           height;
				Degree           degree;
				Number_of_leaves leaves;

				// XXX remove later
				bool             active;

				Physical_block_address free_number;
				Generation             free_gen;
				Hash                   free_hash;
				Height           free_height;
				Degree           free_degree;
				Number_of_leaves free_leaves;
			};
			char data[BLOCK_SIZE];
		};

		bool valid() { return root_number != 0; }

	} __attribute__((packed));

	/* XXX (ab-)use generation field for debug type */
	enum {
		GEN_TYPE_SHIFT  = 48u,
		GEN_TYPE_PARENT = 1ull << GEN_TYPE_SHIFT,
		GEN_TYPE_CHILD  = 2ull << GEN_TYPE_SHIFT,

		GEN_TYPE_MASK   = 0xffull << 48,
		GEN_VALUE_MASK  = (1ull << GEN_TYPE_SHIFT) -1,
	};

	struct Type_i_node
	{
		union {
			struct {
				Cbe::Physical_block_address pba;
				Cbe::Generation             gen;
				Cbe::Hash                   hash;
			};

			char data[64];
		};
	} __attribute__((packed));

	struct Type_1_node_info
	{
		Cbe::Physical_block_address pba;
		Cbe::Generation             gen;
		Cbe::Hash                   hash;
	};

	struct Type_ii_node
	{
		union {
			struct {
				Cbe::Physical_block_address pba;
				Cbe::Virtual_block_address  last_vba;
				Cbe::Generation             alloc_gen;
				Cbe::Generation             free_gen;
				Cbe::Key_id                 last_key_id;
				bool reserved;
			};

			char data[64];
		};
	} __attribute__((packed));

	constexpr size_t TYPE_1_PER_BLOCK = BLOCK_SIZE / sizeof (Type_i_node);
	constexpr size_t TYPE_2_PER_BLOCK = BLOCK_SIZE / sizeof (Type_ii_node);

	struct Tree_helper
	{
		static inline uint32_t _log2(uint32_t const value)
		{
			if (!value) { return -1; }

			for (int i = 8 * sizeof(value) - 1; i >= 0; --i) {
				if (((uint32_t)1 << i) & value) { return i; }
			}

			return -1; 
		}

		Cbe::Degree const _degree;
		Cbe::Height const _height;

		Cbe::Degree const _degree_log2 { _log2(_degree) };
		Cbe::Degree const _degree_mask { (1u << _degree_log2) - 1 };

		Tree_helper(Cbe::Degree const degree,
		            Cbe::Height const height)
		: _degree(degree), _height(height) { }

		uint32_t index(Cbe::Virtual_block_address const vba,
		               uint32_t                   const level) const
		{
			return (vba >> (_degree_log2 * (level - 1)) & _degree_mask);
		}

		Cbe::Height height() const { return _height; }
		Cbe::Degree degree() const { return _degree; }
	};

} /* namespace Cbe */

#endif /* _CBE_TYPES_H_ */
