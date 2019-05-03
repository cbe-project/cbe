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

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	template <unsigned, unsigned> class Cache;

} /* namespace Module */ } /* namespace Cbe */


template <unsigned JOBS, unsigned CACHE_SIZE>
class Cbe::Module::Cache : Noncopyable
{
	public:

		struct Data
		{
			Cbe::Block_data item[CACHE_SIZE];
		};

		struct Job_Data
		{
			Cbe::Block_data item[JOBS];
		};

		struct Index
		{
			static constexpr Genode::uint32_t INVALID = ~0u;
			static constexpr Genode::uint32_t MAX = CACHE_SIZE;
			unsigned value;
			bool valid() const { return value != INVALID && value < MAX; }
		};

	private:

		/*
		 * Internal cache handling
		 */

		struct Entry
		{
			Cbe::Physical_block_address pba { };

			Cbe::Timestamp last_used { 0 };

			enum State { UNUSED, USED } state { UNUSED };
		};

		Entry _entries[CACHE_SIZE] { };

		Index _lookup_cache(Cbe::Physical_block_address pba) const
		{
			Index cdx { Index::INVALID };

			for (unsigned i = 0; i < CACHE_SIZE; i++) {

				Entry const &entry = _entries[i];
				if (   entry.state == Entry::USED
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

			Cbe::Timestamp min_used { ~0ul };

			for (unsigned i = 0; i < CACHE_SIZE; i++) {

				Entry const &entry = _entries[i];
				if (entry.state == Entry::USED) {
					if (min_used > entry.last_used) {
						min_used = entry.last_used;
						cdx.value = i;
					}
				}
			}
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
			Cbe::Physical_block_address pba;
			enum State { UNUSED, PENDING, IN_PROGRESS, COMPLETE } state;
			bool success;
		};

		Job  _jobs[JOBS]      {   };
		unsigned _active_jobs { 0 };

		enum { FOUND = 0, CONTINUE, };

		template <typename FN>
		void _for_each_entry(unsigned state, FN const &fn)
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == state) {
					if (fn(_jobs[i], Index { .value = i }) == FOUND) { break; }
				}
			}
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

		bool _job_pending(Cbe::Physical_block_address pba) const
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (  _jobs[i].state != Job::UNUSED
				   && _jobs[i].pba == pba) {
					return true;
				}
			}
			return false;
		}

		void _copy_to(Cache::Data &data, Cache::Index data_index,
		              Cache::Job_Data const &job_data, Cache::Index job_index)
		{
			void       * const dst = (void*)&data.item[data_index.value];
			void const * const src = (void const*)&job_data.item[job_index.value];

			/* assert sizeof data.item[0] == sizeof job_data.item[0] == sizeof Block_data */
			Genode::memcpy(dst, src, sizeof (Cbe::Block_data));
		}

	public:

		/**
		 * Check if the data for the given physical block address is in the cache
		 *
		 * \return true if a primitive can be accepted, otherwise false
		 */
		bool data_available(Cbe::Physical_block_address const pba) const
		{
			return _lookup_cache(pba).valid();
		}

		/**
		 * Get index of data for given physical block address and update LRU
		 * value
		 *
		 * This method must only be called after executing 'data_available'
		 * returned true.
		 *
		 * \param  pba   physical block address
		 * \param  time  reference to time object used to update LRU
		 *
		 * \return index
		 */
		Index data_index(Cbe::Physical_block_address const pba,
		                 Cbe::Timestamp current_time)
		{
			Index cdx    = _lookup_cache(pba);
			Entry &entry = _entries[cdx.value];

			entry.last_used = current_time;
			return cdx;
		}

		/**
		 * Check if the module can accept a new primitive
		 *
		 * \return true if a primitive can be accepted, otherwise false
		 */
		bool request_acceptable(Cbe::Physical_block_address pba) const
		{
			return _active_jobs < JOBS || !_job_pending(pba);
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
		void submit_request(Cbe::Physical_block_address const pba)
		{
			if (_job_pending(pba)) {
				Genode::error("_request_pending");
				return;
			}

			if (data_available(pba)) {
				Genode::error("request available");
				return;
			}

			Index edx = _unused_entry();

			_jobs[edx.value] = Job {
				.pba     = pba,
				.state   = Job::PENDING,
				.success = false,
			};

			_active_jobs++;
		}

		/**
		 * Fill cache
		 *
		 * This method fills the cache from any completed primitive.
		 *
		 * \return true if any completed primitive was processed
		 */
		bool execute(Cache::Data &data, Cache::Job_Data const &job_data,
		             Cbe::Timestamp current_time)
		{
			bool progress = false;
			_for_each_entry(Job::COMPLETE, [&] (Job &j, Index idx) {

				Index cdx = _get_cache_slot();

				if (j.success) {

					Entry &entry = _entries[cdx.value];
					entry.state     = Entry::USED;
					entry.pba       = j.pba;
					entry.last_used = current_time;

					_copy_to(data, cdx, job_data, idx);
				}

				j.state = Job::UNUSED;
				_active_jobs--;
				progress = true;

				return CONTINUE;
			});

			return progress;
		}

		/**
		 * Take the next generated primitive
		 *
		 * This method must only be called after executing
		 * 'peek_generated_primitive' returned true.
		 *
		 * \return next valid generated primitive
		 */
		Cbe::Primitive peek_generated_primitive()
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == Job::PENDING) {
					return Cbe::Primitive {
						.tag          = Cbe::CACHE_TAG,
						.operation    = Cbe::Primitive::Operation::READ,
						.success      = Cbe::Primitive::Success::FALSE,
						.block_number = _jobs[i].pba,
						.index        = 0,
					};
				}
			}

			return Cbe::Primitive { };
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
		Index peek_generated_data_index(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (   _jobs[i].state == Job::PENDING
				    && _jobs[i].pba == p.block_number) {
					return Index { .value = i };
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
		void drop_generated_primitive(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (   _jobs[i].state == Job::PENDING
				    && _jobs[i].pba == p.block_number) {
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
			_for_each_entry(Job::IN_PROGRESS, [&] (Job &j, Index) {

				if (j.pba != p.block_number) { return CONTINUE; }

				j.success = p.success == Cbe::Primitive::Success::TRUE;
				j.state = Job::COMPLETE;

				return FOUND;
			});
		}
};

#endif /* _CBE_CACHE_MODULE_H_ */
