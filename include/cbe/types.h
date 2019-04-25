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

namespace Cbe {

	using namespace Genode;

	using Tag                  = uint32_t;
	using Number_of_primitives = size_t;

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

	using Physical_block_address = uint64_t;
	using Virtual_block_address  = uint64_t;
	using Generation             = uint64_t;

	enum { BLOCK_SIZE = 4096u };

	struct Block_data
	{
		char values[BLOCK_SIZE] { };
	};

	struct Hash
	{
		char values[32];
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
		using Generation       = uint64_t;
		using Height           = uint32_t;
		using Degree           = uint32_t;
		using Number_of_leaves = uint64_t;

		union {
			struct {
				Key key[NUM_KEYS];

				Number           root_number;
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
