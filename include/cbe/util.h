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
