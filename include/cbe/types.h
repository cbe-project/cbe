/*
 * \brief  CBE C++ prototype type definitions
 * \author Josef Soentgen
 * \date   2019-01-21
 */

/*
 * Copyright (C) 2019 Genode Labs GmbH
 *
 * This file is part of the Genode OS framework, which is distributed
 * under the terms of the GNU Affero General Public License version 3.
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
	} __attribute__((packed));

	enum { INVALID_PBA = 18446744073709551615ULL, };
	using Physical_block_address = uint64_t;
	using Virtual_block_address  = uint64_t;
	using Generation             = uint64_t;

	using Timestamp = uint64_t;

	enum { BLOCK_SIZE = 4096u };

	struct Block_data
	{
		char values[BLOCK_SIZE] { };
	} __attribute__((packed));

	struct Hash
	{
		char values[32];

		void print(Genode::Output &out) const
		{
			using namespace Genode;

			for (char const c : values) {
				Genode::print(out, Hex(c, Hex::OMIT_PREFIX, Hex::PAD));
			}
		}
	};

	struct Key
	{
		enum { KEY_SIZE = 64u };
		using Id = uint32_t;
		char value[KEY_SIZE];
		Id   id;
	};

	enum { NUM_SUPER_BLOCKS = 8, };

	struct Super_block
	{
		enum { NUM_KEYS = 2u };
		using Number           = uint64_t;
		using Generation       = Cbe::Generation;
		using Height           = uint32_t;
		using Degree           = uint32_t;
		using Number_of_leaves = uint64_t;

		union {
			struct {
				Key key[NUM_KEYS];

				Number           root_number;
				Hash             root_hash;
				Generation       generation;
				Height           height;
				Degree           degree;
				Number_of_leaves leaves;

				Number           free_number;
				Height           free_height;
				Number_of_leaves free_leaves;
			};
			char data[BLOCK_SIZE];
		};
	} __attribute__((packed));

	/* XXX (ab-)use generation field for debug type */
	enum {
		GEN_TYPE_PARENT = 1ull << 48,
		GEN_TYPE_CHILD  = 2ull << 48,
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

} /* namespace Cbe */

#endif /* _CBE_TYPES_H_ */
