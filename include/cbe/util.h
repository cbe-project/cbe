/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_UTIL_H_
#define _CBE_UTIL_H_

/* Genode includes */
#include <timer_session/connection.h>

/* CBE includes */
#include <cbe/types.h>

namespace Cbe {

	class Time;

	/**
	 * Convert CBE primitive to CBE request
	 *
	 * \param p refrence to primitive
	 *
	 * \return Cbe::Request object
	 */
	static inline Cbe::Request convert_from(Cbe::Primitive const &p)
	{
		auto convert_op = [&] (Cbe::Primitive::Operation o) {
			switch (o) {
			case Cbe::Primitive::Operation::INVALID: return Cbe::Request::Operation::INVALID;
			case Cbe::Primitive::Operation::READ:    return Cbe::Request::Operation::READ;
			case Cbe::Primitive::Operation::WRITE:   return Cbe::Request::Operation::WRITE;
			case Cbe::Primitive::Operation::SYNC:    return Cbe::Request::Operation::SYNC;
			}
			return Cbe::Request::Operation::INVALID;
		};
		return Cbe::Request {
			.operation    = convert_op(p.operation),
			.success      = Cbe::Request::Success::FALSE,
			.block_number = p.block_number,
			.offset       = 0,
			.count        = 1,
			.tag          = 0,
		};
	}

	/**
	 * Convert CBE request
	 *
	 * \param r  reference to CBE request object
	 *
	 * \return  Block request object
	 */
	static inline Block::Request convert_from(Cbe::Request const &r)
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
	static inline Cbe::Request convert_to(Block::Request const &r)
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


class Cbe::Time
{
	public:

		using Timestamp = Genode::uint64_t;

	private:

		Timer::Connection _timer;

		/*
		 * Synchronization timeout handling
		 */

		Timer::One_shot_timeout<Time> _sync_timeout {
			_timer, *this, &Time::_handle_sync_timeout };

		void _handle_sync_timeout(Genode::Duration);

		Genode::Signal_context_capability _sync_sig_cap { };

		/*
		 * Securing timeout handling
		 */

		Timer::One_shot_timeout<Time> _secure_timeout {
			_timer, *this, &Time::_handle_secure_timeout };

		void _handle_secure_timeout(Genode::Duration);

		Genode::Signal_context_capability _secure_sig_cap { };

	public:

		Time(Genode::Env &env);

		Timestamp timestamp();

		void sync_sigh(Genode::Signal_context_capability cap);

		void schedule_sync_timeout(uint64_t msec);

		void secure_sigh(Genode::Signal_context_capability cap);

		void schedule_secure_timeout(uint64_t msec);
};

#endif /* _CBE_UTIL_H_ */
