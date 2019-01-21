/*
 * \brief  CBE C++ prototype request pool module
 * \author Josef Soentgen
 * \date   2019-01-21
 *
 * The request pool module is used to manage client Block::Requests.
 * It generates a unique tag for the request and notes the number of
 * primitives that are needed to fulfill.
 */

/*
 * Copyright (C) 2019 Genode Labs GmbH
 *
 * This file is part of the Genode OS framework, which is distributed
 * under the terms of the GNU Affero General Public License version 3.
 */

#ifndef _CBE_REQUEST_POOL_MODULE_H_
#define _CBE_REQUEST_POOL_MODULE_H_

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	template <unsigned> class Request_pool;

} /* namespace Module */ } /* namespace Cbe */

template <unsigned N>
class Cbe::Module::Request_pool
{
	public:

		struct Primitive
		{
			Block::Request       request;
			Tag                  tag;
			Number_of_primitives primitives;
			Number_of_primitives done;

			enum State { UNUSED, PENDING, IN_PROGRESS, COMPLETE } state;

			bool pending()  const { return state == State::PENDING; }
			bool complete() const { return state == State::COMPLETE; }

			bool unused() const
			{
				return state != State::PENDING
				    && state != State::IN_PROGRESS
				    && state != State::COMPLETE;
			}
		};

	private:

		Primitive    _entries[N]   {   };
		unsigned _used_entries { 0 };

	public:

		/**
		 * Check if the pool can accept a new request
		 *
		 * \return true if the request can be accepted, otherwise false
		 */
		bool acceptable() const { return _used_entries < N; }

		/**
		 * Submit a new request
		 *
		 * The request as well as the number of primitives will be stored
		 * internally.
		 *
		 * \param r  copy of request
		 * \param n  number of primitives
		 */
		void submit_request(Block::Request       const request,
		                    Number_of_primitives const num)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].unused()) {

					_entries[i] = Primitive {
						.request    = request,
						.tag        = i,
						.primitives = num,
						.done       = 0,
						.state      = Primitive::State::PENDING,
					};

					/* assume success, might be overriden in process_primitive */
					_entries[i].request.success = Block::Request::Success::TRUE;

					_used_entries++;
					break;
				}
			}
		}

		/**
		 * Check for any pending request
		 *
		 * The method will return true as long as there is a pending
		 * request.
		 *
		 * \return true if a request is pending, otherwise false
		 */
		bool peek_request_pending() const
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].pending()) { return true; }
			}

			return false;
		}

		/**
		 * Take the next pending request
		 *
		 * This method must only be called after executing
		 * 'peek_request_pending' returned true.
		 *
		 * \return next pending request
		 */
		Primitive take_pending_request()
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].pending()) {
					_entries[i].state = Primitive::State::IN_PROGRESS;
					return _entries[i];
				}
			}

			/* should never be reached */
			return Primitive();
		}

		/**
		 * Mark the primitive as completed
		 *
		 * \param p  reference to Primitive that is used to lookup
		 *           the corresponding internal primitive as completed
		 */
		void mark_completed_primitive(Cbe::Primitive const &p)
		{
			Tag const &tag = p.tag;

			if (p.success == Cbe::Primitive::Success::FALSE
			    && _entries[tag].request.success == Block::Request::Success::TRUE) {
				_entries[tag].request.success = Block::Request::Success::FALSE;
			}

			_entries[tag].done++;

			if (_entries[tag].done == _entries[tag].primitives) {
				_entries[tag].state = Primitive::State::COMPLETE;
			}
		}

		/**
		 * Check for any completed request
		 *
		 * The method will return true as long as there is a completed
		 * request available.
		 *
		 * \return true if a request is pending, otherwise false
		 */
		bool peek_completed_request()
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].complete()) { return true; }
			}
			return false;
		}

		/**
		 * Take completed request
		 *
		 * This method must only be called after executing
		 * 'peek_completed_request' returned true.
		 *
		 * \return takes next completed request and removes it
		 *         from the module
		 */
		Block::Request take_completed_request()
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].complete()) {
					_entries[i].state = Primitive::State::UNUSED;
					_used_entries--;

					return _entries[i].request;
				}
			}

			/* should never be reached */
			return Block::Request { };
		}

		/**
		 * Get request for given tag
		 *
		 * The method checks if the given tag is valid and belongs to
		 * a known Block request. If all checks out it will return the
		 * corresponding Block request, otherwise a invalid one will
		 * by returned.
		 *
		 * \return a valid Block::Request for the given tag or an
		 *         an invalid one in case there is none
		 */
		Block::Request request_for_tag(Tag const tag) const
		{
			bool const valid = tag < N && !_entries[tag].unused();
			return valid ? _entries[tag].request : Block::Request { };
		}
};

#endif /* _CBE_REQUEST_POOL_MODULE_H_ */
