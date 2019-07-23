/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_RECLAIM_MODULE_H_
#define _CBE_RECLAIM_MODULE_H_

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	struct Reclaim_Index
	{
		Genode::uint32_t value;
	};

	struct Reclaim_Data
	{
		Cbe::Block_data item[1];
	} __attribute__((packed));

	struct Reclaim;
} /* namespace Module */

	struct Block_manager;

} /* namespace Cbe */

/**********************************
 ** Temporary free block manager **
 **********************************/

#include <util/bit_array.h>

struct Cbe::Block_manager
{
	/* gives us 128MiB */
	enum { ENTRIES = 32u * 1024, };
	struct Entry /* Cbe::Type_ii_node */
	{
		bool reserved { false }; /* reserved => true */
		Cbe::Generation g_a { 0 };
		Cbe::Generation g_f { 0 };
	};
	Entry _entries[ENTRIES] { };
	Genode::Bit_array<ENTRIES> _array { };
	Genode::size_t             _used  { 0 };

	Cbe::Physical_block_address _start { 0 };
	Genode::size_t _size { 0 };

	Block_manager(Cbe::Physical_block_address start, size_t size)
	: _start(start), _size(size)
	{
		if (_size > ENTRIES) {
			Genode::warning("limit entries to ", (unsigned)ENTRIES);
			_size = ENTRIES;
		}

		Genode::log("Free Block manager entries: ", _size,
		            " start block address: ", _start);
	}

	void dump() const
	{

		for (Genode::size_t i = 0; i < ENTRIES; i++) {
			Entry const &e = _entries[i];
			if (!e.reserved) { continue; }

			Cbe::Generation const f_gen = e.g_f;
			Cbe::Generation const a_gen = e.g_a;

			Genode::log("\033[35;1m", __func__, ": ", i, ": (", _start + i, ") ", " a: ", a_gen, " f: ", f_gen);
		}
	}

	Cbe::Physical_block_address alloc(Cbe::Super_block const *sb,
	                                  Cbe::Generation const s_gen,
	                                  Cbe::Generation const curr_gen,
	                                  size_t count)
	{
		for (size_t i = 0; i < ENTRIES; i++) {
			bool found = false;
			try { found = !_array.get(i, count); }
			catch (...) { }

			if (!found) { continue; }

			bool free = true;
			for (size_t j = i; j < (i+count); j++) {
				Entry &e = _entries[j];
				if (!e.reserved) {
					continue;
				} else {
					Cbe::Generation const f_gen = e.g_f;
					Cbe::Generation const a_gen = e.g_a;

					if (f_gen <= s_gen) {

						bool in_use = false;
						for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
							Cbe::Super_block const &b = sb[i];
							if (!b.active) {
								continue;
							}
							Cbe::Generation const b_gen = b.generation;

							bool const is_free = ((f_gen <= s_gen) && (f_gen <= b_gen || a_gen >= (b_gen + 1)));

							in_use |= !is_free;
						}

						if (in_use) {
							free = false;
							break;
						}
						Genode::error("REUSE PBA: ", _start + j, " f: ", f_gen, " a: ", a_gen);
					} else {
						free = false;
						break;
					}
				}
			}
			if (!free) { continue; }

			_array.set(i, count);
			_used += count;

			for (size_t j = i; j < (i+count); j++) {
				Entry &e = _entries[j];
				e.reserved = true;
				e.g_a = curr_gen;
				e.g_f = 0;
			}
			Cbe::Physical_block_address const pba = _start + i;
			return pba;
		}

		struct Alloc_failed { };
		throw Alloc_failed();
	}

	void free(Cbe::Generation const curr_gen,
	          Cbe::Physical_block_address pba)
	{
		if (pba < _start) {
			Genode::warning("attempt to free invalid pba: ", pba);
			return;
		}

		size_t const i = pba - _start;

		Entry &e = _entries[i];
		e.g_f = curr_gen;

		try {
			_array.clear(i, 1);
			_used--;
		} catch (...) { Genode::warning(i, " already cleared"); }
	}
};



struct Cbe::Module::Reclaim
{
	struct Entry
	{
		Cbe::Physical_block_address _pba[4096] { };
		Genode::uint32_t count { 0 };

		void push(Cbe::Physical_block_address pba)
		{
			_pba[count] = pba;
			count++;
		}

		Cbe::Physical_block_address pop()
		{
			count--;
			return _pba[count];
		}

		Cbe::Physical_block_address peek() const
		{
			return _pba[count-1];
		}

		Genode::uint32_t lookup(Cbe::Physical_block_address const pba)
		{
			for (Genode::uint32_t i = 0; i < count; i++) {
				if (pba == _pba[i]) {
					return i;
				}
			}

			return ~0u;
		}

		void reset() { count = 0; }
	};

	Entry _request_list { };
	Entry _pending_list { };
	Entry _free_list    { };

	Cbe::Physical_block_address _data_pba { 0 };
	bool                        _data_pba_done { false };

	Cbe::Physical_block_address _root { 0 };
	Cbe::Physical_block_address _generation { 0 };
	Cbe::Height    _height { 0 };

	bool _finished { false };

	bool execute(Reclaim_Data const &data)
	{
		bool result = false;

		if (_data_pba != 0 && _data_pba_done) {
			if (_height) {
				Cbe::Type_i_node const *node = reinterpret_cast<Cbe::Type_i_node const*>(&data);
				for (size_t i = 0; i < sizeof (Cbe::Block_data) / sizeof (Cbe::Type_i_node); i++) {
					Cbe::Generation             const gen = node[i].gen;

					if ((gen & GEN_VALUE_MASK) == _generation) {
						_request_list.push(node[i].pba);
					}
				}
			}

			_data_pba = 0;
			_data_pba_done = false;
			result |= true;
		}

		return result;
	}

	bool request_acceptable() const
	{
		return !_root && !_generation;
	}

	void submit_request(Cbe::Super_block const &sb)
	{
		if (_root && _generation) {
			struct Request_already_pending { };
			throw Request_already_pending();
		}

		_root       = sb.root_number;
		_generation = sb.generation;
		_height     = sb.height + 1;

		_request_list.reset();
		_pending_list.reset();
		_free_list.reset();

		_request_list.push(_root);
	}

	bool peek_completed_request() const { return _finished; }

	void reclaim_completed_request(Cbe::Block_manager &bm, Cbe::Generation const curr_gen)
	{
		for (Genode::uint32_t i = 0; i < _free_list.count; i++) {
			Cbe::Physical_block_address const pba = _free_list._pba[i];
			bm.free(curr_gen, pba);
		}
	}

	void drop_completed_request()
	{
		_root       = 0;
		_generation = 0;
		_height     = 0;
		_finished   = false;
	}

	Cbe::Primitive peek_generated_primitive() const
	{
		if (_request_list.count > 0 && _data_pba == 0) {
			return Cbe::Primitive {
				.tag          = Cbe::Tag::RECLAIM_TAG,
				.operation    = Cbe::Primitive::Operation::READ,
				.success      = Cbe::Primitive::Success::FALSE,
				.block_number = _request_list.peek(),
				.index        = 0,
			};
		}
		else {
			return Cbe::Primitive { };
		}
	}

	Reclaim_Index peek_generated_data_index(Cbe::Primitive const &prim)
	{
		(void)prim;
		return Reclaim_Index { .value = 0u };
	}

	void drop_generated_primitive(Cbe::Primitive const &prim)
	{
		if (prim.block_number != _request_list.peek()) {
			struct Invalid_primitive { };
			throw Invalid_primitive();
		}

		Cbe::Physical_block_address pba = _request_list.pop();
		_pending_list.push(pba);

		_data_pba = pba;
	}

	void mark_generated_primitive_complete(Cbe::Primitive const &prim)
	{
		Cbe::Primitive::Number const pba = prim.block_number;

		if (prim.success != Cbe::Primitive::Success::TRUE) {
			Genode::error(__func__, ": ", pba, " failed");
			throw -1;
		}

		Genode::uint32_t idx = _pending_list.lookup(pba);
		if (idx == ~0u || pba != _pending_list.peek()) {
			Genode::error(__func__, ": ", pba, " not found");
			throw -2;
		}

		(void)_pending_list.pop();

		_free_list.push(pba);
		if (!_request_list.count) {
			Genode::uint32_t const new_height = _height ? _height - 1 : 0;
			_height = new_height;
			if (!_height) {
				_finished = true;
			}
		}
		_data_pba_done = true;
	}
};

#endif 	/* _CBE_RECLAIM_MODULE_H_ */
