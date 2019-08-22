/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_IO_MODULE_H_
#define _CBE_IO_MODULE_H_

/* Genode includes */
#include <block_session/connection.h>

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	struct Io_data
	{
		enum { NUM_ITEMS = 1, };
		Cbe::Block_data item[NUM_ITEMS];
	} __attribute__((packed));

	class Block_io;

} /* namespace Module */ } /* namespace Cbe */

#define MOD_NAME "IO"

class Cbe::Module::Block_io : Noncopyable
{
	public:

		struct Block_size_mismatch : Genode::Exception { };

		static constexpr Genode::uint32_t N = Io_data::NUM_ITEMS;

		struct Index
		{
			static constexpr Genode::uint32_t INVALID = ~0u;
			unsigned value;
			bool valid() const { return value != INVALID; }
		};

	private:

		struct Internal_entry
		{
			Cbe::Tag       orig_tag { };
			Cbe::Primitive primitive { };

			Block::Packet_descriptor packet { };

			enum State { UNUSED, PENDING, IN_PROGRESS, COMPLETE } state { UNUSED };

			void print(Genode::Output &out) const
			{
				auto state_string = [](State state) {
					switch (state) {
					case State::UNUSED:      return "unused";
					case State::PENDING:     return "pending";
					case State::IN_PROGRESS: return "in_progress";
					case State::COMPLETE:    return "complete";
					}
					return "unknown";
				};
				Genode::print(out, "primitive.block_number: ", primitive.block_number,
				              " state: ", state_string(state));
			}
		};

		Internal_entry _entries[N]   {   };
		unsigned       _used_entries { 0 };

		bool _equal_primitives(Cbe::Primitive const &p1, Cbe::Primitive const &p2)
		{
			return p1.block_number == p2.block_number
			    && p1.index        == p2.index
			    && p1.operation    == p2.operation;
		}

	public:

		/**
		 * Constructor
		 */
		Block_io() { }

		/**
		 * Check if the module can accept new primitives
		 *
		 * \return true if a primitive can be accepted, otherwise false
		 */
		bool primitive_acceptable() const { return _used_entries < N; }

		/**
		 * Submit a new primitive
		 *
		 * The primitive will be copied to the internal buffer and the Block_data
		 * reference will be stored as a reference. The method may only be called
		 * after 'acceptable' was executed and returned true. The new primitive is
		 * marked as pending and waits for execution.
		 *
		 * \param p  reference to the Primitive
		 * \param d  reference to a Block_data object
		 */
		void submit_primitive(Tag const tag, Primitive const &p,
		                      Io_data &io_data, Block_data &data, bool checked = false)
		{
			for (unsigned i = 0; i < N; i++) {
				Internal_entry &e = _entries[i];

				if (e.state != Internal_entry::UNUSED) { continue; }

				e.state         = Internal_entry::PENDING;
				e.primitive     = p;
				e.primitive.tag = tag;
				e.orig_tag      = p.tag;

				if (p.write()) {
					Genode::memcpy(&io_data.item[i], &data, sizeof (Cbe::Block_data));
				}

				_used_entries++;
				MOD_DBG("primitive: ", p);

				if (p.read()) {
					// XXX debug helper, remove later
					if (!checked) {
						throw -1;
					}
				}
				return;
			}
		}

		/**
		 * Check for any completed primtive
		 *
		 * \return true if there is an completed primitive, otherwise
		 *         false
		 */
		Primitive peek_completed_primitive()
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::COMPLETE) { continue; }

				return _entries[i].primitive;
			}

			return Cbe::Primitive { };
		}

		/**
		 * Get access to the data of a completed primitive
		 *
		 * This method must only be called after executing
		 * 'peek_completed_primitive' returned a valid primitive.
		 *
		 * \param p  refrence to the completed primitive
		 *
		 * \return reference to the data
		 */
		uint32_t peek_completed_data_index(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::COMPLETE
				    || !_equal_primitives(p, _entries[i].primitive)) { continue; }

				return i;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}

		/**
		 * Get the original tag of the submitted primitive
		 *
		 * This method must only be called after executing
		 * 'peek_completed_primitive' returned a valid primitive.
		 *
		 * \param p  refrence to the completed primitive
		 *
		 * \return original tag of the submitted primitive
		 */
		Cbe::Tag peek_completed_tag(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::COMPLETE
				    || !_equal_primitives(p, _entries[i].primitive)) { continue; }

				return _entries[i].orig_tag;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}

		/**
		 * Take the next completed primtive
		 *
		 * This method must only be called after executing
		 * 'peek_completed_primitive' returned true.
		 *
		 * \return takes next valid completed primtive and removes it
		 *         from the module
		 */
		void drop_completed_primitive(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::COMPLETE
				    || !_equal_primitives(p, _entries[i].primitive)) { continue; }

				_entries[i].state = Internal_entry::UNUSED;
				_used_entries--;
				return;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}

		/**
		 * Check for any generated primitive
		 *
		 * The method will always a return a primitive and the caller
		 * always has to check if the returned primitive is in fact a
		 * valid one.
		 *
		 * \return a valid Primitive will be returned if there is an
		 *         generated primitive pending, otherwise an invalid one
		 */
		Cbe::Primitive peek_generated_primitive()
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::PENDING) { continue; }

				return _entries[i].primitive;
			}

			return Cbe::Primitive { };
		}


		/**
		 * Get index for the data block of the generated primitive
		 *
		 * This method must only be called after 'peek_generated_io_primitive'
		 * returned a valid primitive.
		 *
		 * \param p  reference to the completed primitive
		 *
		 * \return index for data block
		 */
		uint32_t peek_generated_data_index(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {

				MOD_DBG(i, " state: ", (unsigned)_entries[i].state, " ", _entries[i].primitive);
				if (_entries[i].state != Internal_entry::PENDING
				    && _entries[i].state != Internal_entry::IN_PROGRESS) { continue; }

				MOD_DBG(_entries[i].primitive);
				if (!_equal_primitives(p, _entries[i].primitive)) { continue; }

				return i;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}

		/**
		 * Discard given generated primitive
		 *
		 * This method must only be called after 'peek_generated_io_primitive'
		 * returned a valid primitive.
		 *
		 * \param  p  reference to primitive
		 */
		void drop_generated_primitive(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::PENDING
				    || !_equal_primitives(p, _entries[i].primitive)) { continue; }

				_entries[i].state = Internal_entry::IN_PROGRESS;
				return;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}

		/**
		 * Mark given generated primitive as complete
		 *
		 * \param  p  reference to primitive
		 */
		void mark_generated_primitive_complete(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::IN_PROGRESS
				    || !_equal_primitives(p, _entries[i].primitive)) { continue; }

				_entries[i].primitive.success = p.success;

				_entries[i].state = Internal_entry::COMPLETE;
				return;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}
};

#undef MOD_NAME

#endif /* _CBE_IO_MODULE_H_ */
