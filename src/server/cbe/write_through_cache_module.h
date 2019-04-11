/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_WRITE_THROUGH_CACHE_MODULE_H_
#define _CBE_WRITE_THROUGH_CACHE_MODULE_H_

/* Genode includes */
#include <base/sleep.h>
#include <trace/timestamp.h>

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	template <unsigned, unsigned> class Write_through_cache;

} /* namespace Module */ } /* namespace Cbe */


template <unsigned JOBS, unsigned WRITE_THROUGH_CACHE_SIZE>
class Cbe::Module::Write_through_cache : Noncopyable
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
			Cbe::Primitive primitive { };
			Cbe::Block_data data     { };

			Genode::Trace::Timestamp last_used { 0 };
			unsigned fill_index { 0 };

			enum State { UNUSED, INVALIDATE, PENDING, FILL, COMPLETE } state { UNUSED };
		};

		Entry _entries[WRITE_THROUGH_CACHE_SIZE] { };

		struct Index
		{
			static constexpr Genode::uint32_t INVALID = ~0u;
			unsigned value;
			bool valid() const { return value != INVALID; }
		};

		enum { FOUND = 0, CONTINUE, };

		template <typename FN>
		void _for_one_cache_entry(unsigned state, FN const &fn)
		{
			for (unsigned i = 0; i < WRITE_THROUGH_CACHE_SIZE; i++) {
				if (_entries[i].state == state) {
					if (fn(Index { .value = i },
					       _entries[i]) == FOUND) {
						break;
					}
				}
			}
		}

		template <typename FN>
		void _for_all_cache_entries(unsigned state, FN const &fn)
		{
			for (unsigned i = 0; i < WRITE_THROUGH_CACHE_SIZE; i++) {
				if (_entries[i].state == state) {
					fn(Index { .value = i }, _entries[i]);
				}
			}
		}

		Index _lookup_cache(Cbe::Primitive const &p)
		{
			Index cdx { Index::INVALID };

			_for_one_cache_entry(Entry::COMPLETE,
			[&] (Index idx, Entry &entry) {
				if (entry.primitive.block_number == p.block_number) {
					cdx = idx;
					return FOUND;
				}

				return CONTINUE;
			});
			return cdx;
		}

		bool _cache_pending(Cbe::Primitive const &p)
		{
			bool pending = false;
			_for_one_cache_entry(Entry::PENDING,
			[&] (Index, Entry &entry) {
				pending = _equal_primitives(entry.primitive, p);
				return pending ? FOUND : CONTINUE;
			});
			return pending;
		}

		void _cache_fill_with(Cbe::Primitive const &p)
		{
			Index cdx { Index::INVALID };

			_for_one_cache_entry(Entry::UNUSED, [&] (Index idx, Entry &entry) {
				entry.state     = Entry::PENDING;
				entry.primitive = p;
				cdx = idx;
				return FOUND;
			});
			if (cdx.valid()) { return; }

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

			_entries[cdx.value].state     = Entry::PENDING;
			_entries[cdx.value].primitive = p;
		}

		/*
		 * Internal entry handling
		 */

		struct Job
		{
			Cbe::Primitive primitive { };
			Cbe::Block_data *data    { nullptr };

			enum State { UNUSED, SUBMITTED, PENDING, IN_PROGRESS, FILL, COMPLETE } state { UNUSED };
		};

		Cbe::Primitive  _dummy_primitive { };
		Cbe::Block_data _dummy_block_data { };
		Primitive       _dummy_entry { _dummy_primitive, _dummy_block_data };

		Job  _jobs[JOBS]   {   };
		unsigned _active_jobs { 0 };

		template <typename FN>
		void _for_each_entry(unsigned state, FN const &fn)
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == state) {
					if (fn(Index { .value = i }, _jobs[i]) == FOUND) { break; }
				}
			}
		}

		template <typename FN>
		void _for_each_entry_const(unsigned state, FN const &fn) const
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == state) {
					if (fn(Index { .value = i }, _jobs[i]) == FOUND) { break; }
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

	public:

		/**
		 * Check if the module can accept a new primitive
		 *
		 * \return true if a primitive can be accepted, otherwise false
		 */
		bool acceptable() const
		{
			return _active_jobs < JOBS;
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
		void submit_primitive(Cbe::Primitive const &p, Block_data &d)
		{
			Index edx = _unused_entry();

			_jobs[edx.value].primitive = p;
			_jobs[edx.value].data      = &d;
			_jobs[edx.value].state     = Job::SUBMITTED;

			_active_jobs++;
		}

		/**
		 * Process all submitted primitives
		 *
		 * This method tries to process any submitted request. In case it is a
		 * write request any matching cache entry will be invalidated and the
		 * request will be passed on to a I/O module. On the other hand, if it
		 * is a read request, the cache will be queried and any matching primitive
		 * will be served from the cache. If the primitive is not already in the
		 * cache, a request will for a I/O module will be generated.
		 *
		 * \return true if any submitted primitive was processed
		 */
		bool execute()
		{
			bool progress = false;

			/* mark all invalidated entries as unused */
			_for_all_cache_entries(Entry::INVALIDATE, [&] (Index, Entry &entry) {
				entry.state = Entry::UNUSED;
			});

			/* fill cache with results */
			_for_all_cache_entries(Entry::FILL, [&] (Index, Entry &entry) {

				Index edx { entry.fill_index };

				void const * const src = reinterpret_cast<void*>(_jobs[edx.value].data);
				void       * const dst = reinterpret_cast<void*>(&entry.data);
				Genode::memcpy(dst, src, sizeof (entry.data));

				entry.last_used = Genode::Trace::timestamp();
				entry.state = Entry::COMPLETE;

				_jobs[edx.value].state = Job::COMPLETE;
				_jobs[edx.value].primitive.success = Cbe::Primitive::Success::TRUE;
			});

			/* handle new jobs */
			_for_each_entry(Job::SUBMITTED, [&] (Index, Job &j) {

				Cbe::Primitive &p = j.primitive;
				Index cdx   = _lookup_cache(p);

				if (j.primitive.read()) {

					if (cdx.valid()) {

							void const * const src = reinterpret_cast<void*>(&_entries[cdx.value].data);
							void       * const dst = reinterpret_cast<void*>(j.data);
							Genode::memcpy(dst, src, sizeof (_entries[cdx.value].data));

							_entries[cdx.value].last_used = Genode::Trace::timestamp();

							j.state = Job::COMPLETE;
							j.primitive.success = Cbe::Primitive::Success::TRUE;
					} else {
						if (!_cache_pending(p)) { _cache_fill_with(p); }
						j.state = Job::PENDING;
					}

					progress |= true;
				} else

				if (j.primitive.write()) {

					if (cdx.valid()) { _entries[cdx.value].state = Entry::INVALIDATE; }
					j.state = Job::PENDING;

					progress |= true;
				}

				return CONTINUE;
			});

			return progress;
		}

		/**
		 * Check for any completed primitive
		 *
		 * The method will always a return a primitive. The caller can
		 * check which subsequent module must be used to process the
		 * Primitive further as the process-chain might differ for read
		 * and write primtives. The caller always has to check if the
		 * returned primitive is in fact a valid one.
		 *
		 * \return a valid Primitive will be returned if there is an
		 *         completed primitive, otherwise an invalid one
		 */
		Cbe::Primitive peek_completed_primitive() const
		{
			Cbe::Primitive p { };

			_for_each_entry_const(Job::COMPLETE, [&] (Index, Job const &j) {
				p = j.primitive;
				return FOUND;
			});
			return p;
		}

		/**
		 * Take the next completed primitive
		 *
		 * This method must only be called after executing
		 * 'peek_completed_primitive' returned true.
		 *
		 * \return takes next valid completed primitive and removes it
		 *         from the module
		 */
		Cbe::Primitive take_completed_primitive()
		{
			Cbe::Primitive p { };

			_for_each_entry(Job::COMPLETE, [&] (Index, Job &j) {
				j.state = Job::UNUSED;
				_active_jobs--;

				p = j.primitive;
				return FOUND;
			});

			return p;
		}

		/**
		 * Check for any generated primitive
		 *
		 * The method will always a return a primitive. The caller can
		 * check which subsequent module must be used to process the
		 * Primitive further as the process-chain might differ for read
		 * and write primtives. The caller always has to check if the
		 * returned primitive is in fact a valid one.
		 *
		 * \return a valid Primitive will be returned if there is an
		 *         completed primitive, otherwise an invalid one
		 */
		Cbe::Primitive peek_generated_primitive() const
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == Job::PENDING) {
					return _jobs[i].primitive;
				}
			}

			return Cbe::Primitive { };
		}

		/**
		 * Take the next generated primitive
		 *
		 * This method must only be called after executing
		 * 'peek_generated_primitive' returned true.
		 *
		 * \return next valid generated primitive
		 */
		Primitive take_generated_primitive()
		{
			for (unsigned i = 0; i < JOBS; i++) {
				if (_jobs[i].state == Job::PENDING) {
					_jobs[i].state = Job::IN_PROGRESS;

					return Primitive { .primitive = _jobs[i].primitive,
					                   .data      = *_jobs[i].data };
				}
			}

			/* should never be reached */
			return _dummy_entry;
		}

		/**
		 * Mark the primitive as completed
		 *
		 * \param p  reference to Primitive that is used to mark
		 *           the corresponding internal primitive as completed
		 */
		void mark_completed_primitive(Cbe::Primitive const &p)
		{
			_for_each_entry(Job::IN_PROGRESS, [&] (Index edx, Job &j) {

				if (!_equal_primitives(j.primitive, p)) { return CONTINUE; }

				j.primitive.success = p.success;

				if (p.write()) {
					j.state = Job::COMPLETE;
				} else

				if (p.read()) {

					_for_one_cache_entry(Entry::PENDING, [&] (Index, Entry &entry) {
						if (!_equal_primitives(entry.primitive, p)) { return CONTINUE; }

						j.state          = Job::FILL;
						entry.state      = Entry::FILL;
						entry.fill_index = edx.value;
						return FOUND;
					});
				}

				return FOUND;
			});
		}

		/**
		 * Invalidate cached primitive
		 *
		 * \param p  reference to Primitive that is used to lookup
		 *           the corresponding internal cached primitive
		 */
		void mark_as_invalid(Cbe::Primitive const &p)
		{
			Index idx = _lookup_cache(p);
			if (!idx.valid()) { return; }

			_entries[idx.value].state = Entry::INVALIDATE;
		}
};

#endif /* _CBE_WRITE_THROUGH_CACHE_MODULE_H_ */
