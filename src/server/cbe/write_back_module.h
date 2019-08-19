/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_WRITE_BACK_MODULE_H_
#define _CBE_WRITE_BACK_MODULE_H_

/* local includes */
#include <cbe/types.h>
#include <translation_module.h>


namespace Cbe { namespace Module {

	struct Write_back_data
	{
		enum { NUM_ITEMS = 1, };
		Cbe::Block_data item[NUM_ITEMS];
	} __attribute__((packed));

	struct Write_back_data_index
	{
		enum { INVALID = Write_back_data::NUM_ITEMS, };
		uint32_t value;
	};

	template <typename> class Write_back;

} /* namespace Module */ } /* namespace Cbe */

#define MOD_NAME "WRBCK"

template <typename T>
class Cbe::Module::Write_back
{
	public:

		static constexpr Genode::uint32_t N = Translation::MAX_LEVELS;

	private:

		struct Entry
		{
			enum State { INVALID, PENDING, IN_PROGRESS, COMPLETE };

			Cbe::Physical_block_address pba { 0 };
			Cbe::Physical_block_address update_pba { 0 };

			State state;

			Cbe::Tag tag;

			void print(Genode::Output &out) const
			{
				Genode::print(out, "pba: ", pba, " update_pba: ", update_pba);
			}
		};

		/*
		 * _entry[0]               -> leaf node
		 * _entry[1] ... _entry[N] -> inner node
		 */
		Entry _entry[N] { };

		Cbe::Hash _entry_hash[N] { };

		uint32_t _levels { 0 };
		uint32_t _io_levels { 0 };
		bool _io { false };

		enum State { INVALID, CRYPTO, IO, CACHE, COMPLETE };
		State _state { INVALID };

		Cbe::Primitive _pending_primitive { };
		bool           _pending_failure   { false };
		Cbe::Virtual_block_address _vba { ~0ull };
		Cbe::Generation _new_generation { 0 };

	public:

		void update(Cbe::Physical_block_address const pba,
		            Cbe::Tree_helper            const &tree,
		            Cbe::Block_data             const &data,
		            Cbe::Block_data                   &update_data)
		{
			for (Genode::uint32_t i = 1; i < _levels; i++) {
				Entry &e = _entry[i];

				if (e.tag != Cbe::Tag::CACHE_TAG || e.pba != pba) {
					continue;
				}

				T *t = reinterpret_cast<T*>(&update_data);

				/* CoW action incoming */
				if (e.pba != e.update_pba) {
					MOD_DBG("copy ", e.pba, " -> ", e.update_pba);
					Genode::memcpy(t, (void const *)&data, sizeof (Cbe::Block_data));
				}

				/* save as long as only inner nodes in cache */
				Cbe::Physical_block_address const &child_update_pba = _entry[i-1].update_pba;
				Cbe::Hash                   const &child_hash       = _entry_hash[i-1];

				/* get index from VBA in inner node */
				Genode::uint32_t const index   = tree.index(_vba, i);
				t[index].pba = child_update_pba;
				t[index].gen = _new_generation;
				Genode::memcpy(t[index].hash.values, child_hash.values, sizeof (Cbe::Hash));

				MOD_DBG("index: ", index, " child_update_pba: ", child_update_pba, " <", child_hash, ">");

				/* calculate hash */
				Sha256_4k::Data const &hash_data = *reinterpret_cast<Sha256_4k::Data const*>(&update_data);
				Sha256_4k::Hash &hash = *reinterpret_cast<Sha256_4k::Hash*>(&_entry_hash[i]);
				Sha256_4k::hash(hash_data, hash);

				e.state = Entry::State::COMPLETE;
				MOD_DBG("entry: ", i, " ", e, " ---> ", (unsigned)e.state);
				break;
			}

			/* for now always check all entries after update */
			bool complete = true;
			for (Genode::uint32_t i = 1; i < _levels; i++) {
				Entry const &e = _entry[i];
				if (e.state != Entry::State::COMPLETE) {
					MOD_DBG("entry: ", i, " ", e, " ++ ", (unsigned)e.state);
					complete = false;
					break;
				}
			}

			MOD_DBG("complete: ", complete);

			if (complete) {
				_state = State::COMPLETE;
			}
		}

		bool primitive_acceptable() const { return !_pending_primitive.valid(); }

		void submit_primitive(Cbe::Primitive              const &p,
		                      Cbe::Generation             const new_generation,
		                      Cbe::Virtual_block_address  const vba,
		                      Cbe::Physical_block_address const *update_pba,
		                      Cbe::Type_1_node_info       const *pba,
		                      uint32_t                    const n,
		                      Cbe::Block_data             const &d,
		                      Write_back_data                   &wb_data)
		{
			_pending_primitive = p;
			_new_generation = new_generation;
			_vba = vba;

			_levels = n;
			_io = false;
			_io_levels = 1;

			_state = State::CRYPTO;

			/* handle common members */
			for (Genode::uint32_t i = 0; i < n; i++) {
				Entry &e = _entry[i];

				e.pba = pba[i].pba;
				e.update_pba = update_pba[i];

				MOD_DBG("entry: ", i, " ", e);

				e.state = Entry::State::PENDING;
				e.tag   = Cbe::Tag::CACHE_TAG;
			}

			/* the data or rather leave node is special */
			Entry &e = _entry[0];
			e.tag = Cbe::Tag::CRYPTO_TAG_ENCRYPT;
			Genode::memcpy(&wb_data.item[0], &d, sizeof (Cbe::Block_data));
		}


		Cbe::Primitive peek_completed_primitive()
		{
			if (!_pending_primitive.valid()) { return Cbe::Primitive { }; }
			if (_state != State::COMPLETE)   { return Cbe::Primitive { }; }

			_pending_primitive.success =
				_pending_failure ? Cbe::Primitive::Success::FALSE
				                 : Cbe::Primitive::Success::TRUE;
			return _pending_primitive;
		}

		Cbe::Physical_block_address peek_completed_root(Cbe::Primitive const &p) const
		{
			if (_state != State::COMPLETE || p.block_number != _pending_primitive.block_number) {
				MOD_DBG("invalid primitive: ", p);
				throw -1;
			}

			Entry const &e = _entry[_levels-1];
			return e.update_pba;
		}

		void peek_competed_root_hash(Cbe::Primitive const &p, Cbe::Hash &hash) const
		{
			if (_state != State::COMPLETE || p.block_number != _pending_primitive.block_number) {
				MOD_DBG("invalid primitive: ", p);
				throw -1;
			}

			Genode::memcpy(hash.values, _entry_hash[_levels-1].values, sizeof (Cbe::Hash));
		}

		void drop_completed_primitive(Cbe::Primitive const &p)
		{
			if (_state != State::COMPLETE || p.block_number != _pending_primitive.block_number) {
				MOD_DBG("invalid primitive: ", p);
				throw -1;
			}

			_pending_primitive = Cbe::Primitive { };
		}


		Primitive _peek_generated_leaf_primitive() const
		{
			Entry const &e = _entry[0];
			if (e.state != Entry::State::PENDING) { return Cbe::Primitive { }; }

			Cbe::Primitive::Number const block_number = e.update_pba;
			Cbe::Primitive p {
				.tag          = e.tag,
				.operation    = Cbe::Primitive::Operation::WRITE,
				.success      = Cbe::Primitive::Success::FALSE,
				.block_number = block_number,
				.index        = 0,
			};
			MOD_DBG(p);
			return p;
		}

		Write_back_data_index _peek_generated_leaf_data(Cbe::Primitive const &p)
		{
			Entry const &e = _entry[0];
			if (p.block_number != e.update_pba /* || e.state != Entry::State::PENDING */) {
				MOD_ERR("invalid primitive: ", p, " ", e, " ", (unsigned)e.state);
				throw -1;
			}

			return Write_back_data_index { .value = 0 };
		}

		void _drop_generated_leaf_primitive(Cbe::Primitive const &p)
		{
			Entry &e = _entry[0];
			if (p.block_number != e.update_pba || e.state != Entry::State::PENDING) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			e.state = Entry::State::IN_PROGRESS;
			MOD_DBG(p);
		}

		/************
		 ** Crypto **
		 ************/

		Primitive peek_generated_crypto_primitive() const
		{
			if (_state != State::CRYPTO) { return Cbe::Primitive { }; }
			return _peek_generated_leaf_primitive();
		}

		Write_back_data_index peek_generated_crypto_data(Cbe::Primitive const &p)
		{
			if (_state != State::CRYPTO) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}
			return _peek_generated_leaf_data(p);
		}

		void drop_generated_crypto_primitive(Cbe::Primitive const &p)
		{
			if (_state != State::CRYPTO) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			return _drop_generated_leaf_primitive(p);
		}

		void mark_completed_crypto_primitive(Cbe::Primitive  const &p,
		                                     Cbe::Block_data const &crypto_data)
		{
			if (_state != State::CRYPTO) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			Entry &e = _entry[0];
			if (p.block_number != e.update_pba || e.state != Entry::State::IN_PROGRESS) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			Sha256_4k::Data const &data =
				*reinterpret_cast<Sha256_4k::Data const*>(&crypto_data);
			Sha256_4k::Hash hash { };
			Sha256_4k::hash(data, hash);
			Genode::memcpy(_entry_hash[0].values, hash.values, sizeof (hash));

			MOD_DBG(e, " <", _entry_hash[0], ">");

			if (p.success == Cbe::Primitive::Success::FALSE) {
				_pending_failure = true;
				_state = State::COMPLETE;
				MOD_ERR(p);
				return;
			}

			e.state = Entry::State::PENDING;
			e.tag   = Cbe::Tag::IO_TAG;
			_state = State::IO;
		}

		/*********
		 ** I/O **
		 *********/

		Primitive peek_generated_io_primitive() const
		{
			if (_state != State::IO) { return Cbe::Primitive { }; }
			return _peek_generated_leaf_primitive();
		}

		Write_back_data_index peek_generated_io_data(Cbe::Primitive const &p)
		{
			if (_state != State::IO) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}
			return _peek_generated_leaf_data(p);
		}

		void drop_generated_io_primitive(Cbe::Primitive const &p)
		{
			if (_state != State::IO) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			return _drop_generated_leaf_primitive(p);
		}

		void mark_completed_io_primitive(Cbe::Primitive  const &p)
		{
			if (_state != State::IO) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			Entry &e = _entry[0];
			if (p.block_number != e.update_pba || e.state != Entry::State::IN_PROGRESS) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			MOD_DBG(p);

			if (p.success == Cbe::Primitive::Success::FALSE) {
				_pending_failure = true;
				_state = State::COMPLETE;
				MOD_ERR(p);
				return;
			}

			e.state = Entry::State::COMPLETE;
			_state = State::CACHE;
		}

		/***********
		 ** Cache **
		 ***********/

		Primitive peek_generated_cache_primitive() const
		{
			if (_state != State::CACHE) { return Cbe::Primitive { }; }

			for (Genode::uint32_t i = 1; i < _levels; i++) {
				Entry const &e = _entry[i];
				if (e.state != Entry::State::PENDING) { continue; }

				Cbe::Primitive::Number const block_number = e.pba;
				Cbe::Primitive p {
					.tag          = e.tag,
					.operation    = Cbe::Primitive::Operation::WRITE,
					.success      = Cbe::Primitive::Success::FALSE,
					.block_number = block_number,
					.index        = 0,
				};
				MOD_DBG(p);
				return p;
			}
			return Cbe::Primitive { };
		}

		Cbe::Physical_block_address peek_generated_cache_update_pba(Cbe::Primitive const &p) const
		{
			if (_state != State::CACHE) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			for (Genode::uint32_t i = 1; i < _levels; i++) {
				Entry const &e = _entry[i];
				bool const match = p.block_number == e.pba;
				if (!match) { continue; }

				MOD_DBG("entry: ", i, " ", e);
				return e.update_pba;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}

		void drop_generated_cache_primitive(Cbe::Primitive const &p)
		{
			if (_state != State::CACHE) {
				MOD_ERR("invalid primitive: ", p);
				throw -1;
			}

			for (Genode::uint32_t i = 1; i < _levels; i++) {
				Entry &e = _entry[i];
				bool const match = p.block_number == e.pba;
				if (!match) { continue; }
				
				e.state = Entry::State::IN_PROGRESS;
				MOD_DBG(p);
				return;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}
};

#undef MOD_NAME

#endif /* _CBE_WRITE_BACK_MODULE_H_ */
