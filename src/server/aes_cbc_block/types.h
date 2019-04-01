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

	enum { BLOCK_SIZE = 4096u };

	struct Block_data
	{
		char values[BLOCK_SIZE] { };
	};

} /* namespace Cbe */

#endif /* _CBE_TYPES_H_ */
