/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_CACHE_MODULE_H_
#define _CBE_CACHE_MODULE_H_

/* Genode includes */
#include <base/sleep.h>
#include <trace/timestamp.h>

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	template <unsigned, unsigned> class Cache;

} /* namespace Module */ } /* namespace Cbe */


template <unsigned JOBS, unsigned CACHE_SIZE>
class Cbe::Module::Cache : Noncopyable
{
	public:

		struct Primitive
		{
			Cbe::Primitive const &primitive;
			Cbe::Block_data &data;

			bool valid() const { return primitive.valid(); }
		};

	private:

		/*
		 * Internal cache handling
		 */

		struct Entry
		{
			Cbe::Physical_block_address pba { };
			Cbe::Block_data data            { };

			Genode::Trace::Timestamp last_used { 0 };

			enum State { UNUSED, INVALIDATE, PENDING, FILL, COMPLETE } state { UNUSED };
		};

		Entry _entries[CACHE_SIZE] { };

		struct Index
		{
			static constexpr Genode::uint32_t INVALID = ~0u;
			unsigned value;
			bool valid() const { return value != INVALID; }
		};

		enum { FOUND = 0, CONTINUE, };

		template <typename FN>
		void _for_all_cache_entries(unsigned state, FN const &fn)
		{
			for (unsigned i = 0; i < CACHE_SIZE; i++) {
				if (_entries[i].state == state) {
					fn(Index { .value = i }, _entries[i]);
				}
			}
		}

		Index _lookup_cache(Cbe::Physical_block_address pba) const
		{
			Index cdx { Index::INVALID };

			for (unsigned i = 0; i < CACHE_SIZE; i++) {

				Entry const &entry = _entries[i];
				if (   entry.state == Entry::COMPLETE
				    && entry.pba   == pba) {
					cdx.value = i;
					break;
				}
			}

			return cdx;
		}

		Index _get_cache_slot()
		{
			Index cdx { Index::INVALID };

			for (unsigned i = 0; i < CACHE_SIZE; i++) {

				Entry const &entry = _entries[i];
				if (entry.state == Entry::UNUSED) {
					cdx.value = i;
					break;
				}
			}
			if (cdx.valid()) { return cdx; }

			Genode::Trace::Timestamp min_used { ~0ul };

			_for_all_cache_entries(Entry::COMPLETE, [&] (Index idx, Entry &entry) {
				if (min_used > entry.last_used) {
					min_used = entry.last_used;
					cdx = idx;
				}
			});
			if (!cdx.valid()) {
				Genode::error("BUG: cannot evict cache entry");
				Genode::sleep_forever();
			}
			return cdx;
		}

		/*
		 * Internal entry handling
		 */

		struct Job
		{
			Cbe::Primitive  primitive { };
			Cbe::Block_data data      { };

			enum State { UNUSED, SUBMITTED, PENDING, IN_PROGRESS, FILL, COMPLETE } state { UNUSED };
		};

		Job  _jobs[JOBS]      {   };
		unsigned _active_jobs { 0 };

		template <typename FN>
		void _for_each_entry(unsigned state, FN const &fn)
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == state) {
					if (fn(_jobs[i]) == FOUND) { break; }
				}
			}
		}

		template <typename FN>
		void _for_each_entry_const(unsigned state, FN const &fn) const
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == state) {
					if (fn(_jobs[i]) == FOUND) { break; }
				}
			}
		}

		bool _equal_primitives(Cbe::Primitive const &p1, Cbe::Primitive const &p2)
		{
			return p1.block_number == p2.block_number
			    && p1.index        == p2.index
			    && p1.operation    == p2.operation;
		}

		Index _unused_entry()
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == Job::UNUSED) {
					return Index { .value = i };
				}
			}

			error("failed to accept request");
			return Index { .value = Index::INVALID };
		}

		bool _request_pending(Cbe::Physical_block_address pba) const
		{
			bool result = false;
			for (unsigned i = 0; i < JOBS; i++) {
				if (  _jobs[i].state == Job::PENDING
				   || _jobs[i].state == Job::IN_PROGRESS
				   || _jobs[i].state == Job::COMPLETE) {
					if (_jobs[i].primitive.block_number == pba) {
						result = true;
						break;
					}
				}
			}
			return result;
		}

	public:

		/**
		 * Check if the module can accept a new primitive
		 *
		 * \return true if a primitive can be accepted, otherwise false
		 */
		bool available(Cbe::Primitive const &p) const
		{
			return _lookup_cache(p.block_number).valid();
		}

		bool available(Cbe::Physical_block_address const pba) const
		{
			return _lookup_cache(pba).valid();
		}

		/**
		 * Get Block_data for given physical block address
		 *
		 * \return reference to Block_data
		 */
		Cbe::Block_data const &data(Cbe::Primitive const &p)
		{
			Index cdx    = _lookup_cache(p.block_number);
			Entry &entry = _entries[cdx.value];

			entry.last_used = Genode::Trace::timestamp();
			return entry.data;
		}

		Cbe::Block_data const &data(Cbe::Physical_block_address const pba)
		{
			Index cdx    = _lookup_cache(pba);
			Entry &entry = _entries[cdx.value];

			entry.last_used = Genode::Trace::timestamp();
			return entry.data;
		}

		bool acceptable(Cbe::Primitive const &p) const
		{
			return _active_jobs < JOBS || !_request_pending(p.block_number);
		}

		/**
		 * Submit a new primitive
		 *
		 * The primitive will be copied to the internal buffer and the Block_data
		 * reference will be stored as a reference. The method may only be called
		 * after 'acceptable' was executed and returned true. The new primitive is
		 * marked as submitted and waits for execution.
		 *
		 * \param p  reference to the Primitive
		 * \param d  reference to a Block_data object
		 */
		void submit_primitive(Cbe::Primitive const &p)
		{
			if (_request_pending(p.block_number)) {
				Genode::error("_request_pending");
				return;
			}

			if (available(p)) {
				Genode::error("request available");
				return;
			}

			Index edx = _unused_entry();

			_jobs[edx.value].primitive = Cbe::Primitive {
				.tag          = p.tag,
				.operation    = p.operation,
				.success      = Cbe::Primitive::Success::FALSE,
				.block_number = p.block_number,
				.index        = p.index,
			};
			_jobs[edx.value].state = Job::PENDING;

			_active_jobs++;
		}

		/**
		 * Fill cache
		 *
		 * This method fills the cache from any completed primitive.
		 *
		 * \return true if any completed primitive was processed
		 */
		bool execute()
		{
			bool progress = false;
			_for_each_entry(Job::COMPLETE, [&] (Job &j) {

				Index cdx = _get_cache_slot();

				Entry &entry = _entries[cdx.value];
				entry.state     = Entry::COMPLETE;
				entry.pba       = j.primitive.block_number;
				entry.last_used = Genode::Trace::timestamp();

				void       * const dst = (void*)&entry.data;
				void const * const src = (void const*)&j.data;
				Genode::memcpy(dst, src, sizeof (Cbe::Block_data));

				j.state = Job::UNUSED;
				_active_jobs--;
				progress = true;

				return CONTINUE;
			});

			return progress;
		}

		/**
		 * Check for any generated primitive
		 *
		 * \return true if I/O primtive is pending, false otherwise
		 */
		bool peek_generated_primitive() const
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == Job::PENDING) {
					return true;
				}
			}
			return false;
		}

		/**
		 * Take the next generated primitive
		 *
		 * This method must only be called after executing
		 * 'peek_generated_primitive' returned true.
		 *
		 * \return next valid generated primitive
		 */
		Cbe::Primitive take_generated_primitive()
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == Job::PENDING) {
					return _jobs[i].primitive;
				}
			}

			struct No_generated_primitive_available { };
			throw No_generated_primitive_available();
		}

		/**
		 * Take the generated data buffer beloging to given primitive
		 *
		 * This method must only be called after executing
		 * 'peek_generated_primitive' returned true.
		 *
		 * \param  p  reference to primitive the data belongs to
		 *
		 * \return reference to data
		 */
		Cbe::Block_data &take_generated_data(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (   _jobs[i].state == Job::PENDING
				    && _equal_primitives(_jobs[i].primitive, p)) {
					return _jobs[i].data;
				}
			}

			struct No_data_available { };
			throw No_data_available();
		}

		/**
		 * Discard given primitive
		 *
		 * \param  p  reference to primitive
		 */
		void discard_generated_primitive(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (   _jobs[i].state == Job::PENDING
				    && _equal_primitives(_jobs[i].primitive, p)) {
					_jobs[i].state = Job::IN_PROGRESS;
					return;
				}
			}

			struct Cannot_discard_generated_primitive { };
			throw Cannot_discard_generated_primitive();
		}

		/**
		 * Mark the primitive as completed
		 *
		 * \param p  reference to Primitive that is used to mark
		 *           the corresponding internal primitive as completed
		 */
		void mark_completed_primitive(Cbe::Primitive const &p)
		{
			_for_each_entry(Job::IN_PROGRESS, [&] (Job &j) {

				if (!_equal_primitives(j.primitive, p)) { return CONTINUE; }

				j.primitive.success = p.success;
				j.state = Job::COMPLETE;

				return FOUND;
			});
		}
};

#endif /* _CBE_CACHE_MODULE_H_ */
