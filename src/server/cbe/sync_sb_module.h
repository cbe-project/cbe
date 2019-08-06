/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_SYNC_SB_MODULE_H_
#define _CBE_SYNC_SB_MODULE_H_

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	class Sync_sb;

} /* namespace Module */ } /* namespace Cbe */


#define MOD_NAME "SSB"

class Cbe::Module::Sync_sb
{
	private:

		static constexpr uint32_t N = 1;

		struct Entry
		{
			enum State { INVALID, PENDING, IN_PROGRESS, COMPLETE };

			Cbe::Primitive primitive;
			uint64_t id;
			Cbe::Generation gen;

			State state;
		};

		Cbe::Primitive _curr_primitive { };

		Entry _entry[N] { };

	public:

		bool primitive_acceptable() const { return !_curr_primitive.valid(); }

		void submit_primitive(Primitive const &p, uint64_t id, Cbe::Generation const gen)
		{
			if (_curr_primitive.valid()) {
				Genode::error("current primitive already valid");
				throw -1;
			}

			_entry[0] = Entry {
				.primitive = p, .id = id, .gen = gen,
				.state = Entry::State::PENDING
			};

			_curr_primitive = Primitive {
				.tag          = Cbe::Tag::SYNC_SB_TAG,
				.operation    = Cbe::Primitive::Operation::WRITE,
				.success      = Cbe::Primitive::Success::FALSE,
				/* there is currently a 1:1 mapping between SB slot and pba */
				.block_number = (Cbe::Primitive::Number)id,
				.index        = 0
			};
		}

		bool execute() { return false; }

		Primitive peek_completed_primitive()
		{
			if (_entry[0].state == Entry::State::COMPLETE) {
				MOD_DBG("pba: ", _curr_primitive);
				return _curr_primitive;
			}
			return Primitive { };
		}

		Primitive peek_completed_request_primitive(Cbe::Primitive const &p)
		{
			if (_entry[0].state == Entry::State::COMPLETE
			    && p.block_number == _curr_primitive.block_number) {
				return _entry[0].primitive;
			}

			Genode::error(__func__, ": invalid primitive");
			throw -1;
		}

		Cbe::Generation peek_completed_generation(Cbe::Primitive const &p)
		{
			if (_entry[0].state == Entry::State::COMPLETE
			    && p.block_number == _curr_primitive.block_number) {
				return _entry[0].gen;
			}

			Genode::error(__func__, ": invalid primitive");
			throw -1;
		}

		void drop_completed_primitive(Cbe::Primitive const &p)
		{
			if (p.block_number != _curr_primitive.block_number) {
				Genode::error(__func__, ": invalid primitive");
				throw -1;
			}

			MOD_DBG("pba: ", p);

			_entry[0] = Entry {
				.primitive = Primitive { },
				.id        = ~0ull,
				.gen       = 0,
				.state     = Entry::State::INVALID
			};

			_curr_primitive = Primitive { };
		}

		Primitive peek_generated_primitive()
		{
			if (_entry[0].state == Entry::State::PENDING) {
				MOD_DBG("pba: ", _curr_primitive);
				return _curr_primitive;
			}

			return Primitive { };
		}

		uint64_t peek_generated_id(Cbe::Primitive const &p)
		{
			if (p.block_number != _curr_primitive.block_number) {
				Genode::error(__func__, ": invalid primitive");
				throw -1;
			}

			return _entry[0].id;
		}

		void drop_generated_primitive(Cbe::Primitive const &p)
		{
			if (p.block_number != _curr_primitive.block_number) {
				Genode::error(__func__, ": invalid primitive");
				throw -1;
			}

			_entry[0].state = Entry::State::IN_PROGRESS;
		}

		void mark_generated_primitive_complete(Cbe::Primitive const &p)
		{
			if (p.block_number != _curr_primitive.block_number) {
				Genode::error(__func__, ": invalid primitive");
				throw -1;
			}

			_curr_primitive.success = p.success;

			_entry[0].state = Entry::State::COMPLETE;
		}
};

#undef MOD_NAME

#endif /* _CBE_SYNC_SB_MODULE_H_ */
