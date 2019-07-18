/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_UTIL_H_
#define _CBE_UTIL_H_

/* CBE includes */
#include <cbe/types.h>

namespace Cbe {

	/**
	 * Convert CBE request
	 *
	 * \param r  reference to CBE request object
	 *
	 * \return  Block request object
	 */
	Block::Request convert_from(Cbe::Request const &r)
	{
		auto convert_op = [&] (Cbe::Request::Operation o) {
			switch (o) {
			case Cbe::Request::Operation::INVALID: return Block::Operation::Type::INVALID;
			case Cbe::Request::Operation::READ:    return Block::Operation::Type::READ;
			case Cbe::Request::Operation::WRITE:   return Block::Operation::Type::WRITE;
			case Cbe::Request::Operation::SYNC:    return Block::Operation::Type::SYNC;
			}
			return Block::Operation::Type::INVALID;
		};
		auto convert_success = [&] (Cbe::Request::Success s) {
			return s == Cbe::Request::Success::TRUE ? true : false;
		};
		return Block::Request {
			.operation = {
				.type         = convert_op(r.operation),
				.block_number = r.block_number,
				.count        = r.count,
			},
			.success   = convert_success(r.success),
			.offset    = (Block::off_t)r.offset,
			.tag       = { .value = r.tag },
		};
	}

	/**
	 * Convert Block request
	 *
	 * \param r  reference to Block request object
	 *
	 * \return  CBE request object
	 */
	Cbe::Request convert_to(Block::Request const &r)
	{
		auto convert_op = [&] (Block::Operation::Type t) {
			switch (t) {
			case Block::Operation::Type::INVALID: return Cbe::Request::Operation::INVALID;
			case Block::Operation::Type::READ:    return Cbe::Request::Operation::READ;
			case Block::Operation::Type::WRITE:   return Cbe::Request::Operation::WRITE;
			case Block::Operation::Type::SYNC:    return Cbe::Request::Operation::SYNC;
			case Block::Operation::Type::TRIM:    return Cbe::Request::Operation::INVALID; // XXX fix
			}
			return Cbe::Request::Operation::INVALID;
		};
		auto convert_success = [&] (bool success) {
			return success ? Cbe::Request::Success::TRUE : Cbe::Request::Success::FALSE;
		};

		return Cbe::Request {
			.operation    = convert_op(r.operation.type),
			.success      = convert_success(r.success),
			.block_number = r.operation.block_number,
			.offset       = (Genode::uint64_t)r.offset,
			.count        = (Genode::uint32_t)r.operation.count,
			.tag          = (Genode::uint32_t)r.tag.value,
		};
	}


} /* namespace Cbe */

#endif /* _CBE_UTIL_H_ */
