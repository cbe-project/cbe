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


namespace Cbe { namespace Module {

	template <unsigned, typename> class Write_back;

} /* namespace Module */ } /* namespace Cbe */

#define MOD_NAME "WRBCK"

template <unsigned N, typename T>
class Cbe::Module::Write_back
{
	public:

		struct Index
		{
			static constexpr Genode::uint32_t INVALID = ~0u;
			unsigned value;
			bool valid() const { return value != INVALID; }
		};

	private:

		struct Entry {
			uint32_t level { ~0u };
			Cbe::Physical_block_address pba { 0 };
			Cbe::Physical_block_address update_pba { 0 };

			bool in_progress { false };
			bool done { false };

			bool io_in_progress { false };
			bool io_done { false };

			Cbe::Tag tag { Cbe::Tag::INVALID_TAG };
		};

		/*
		 * _entry[0]               -> leaf node
		 * _entry[1] ... _entry[N] -> inner node
		 */
		Entry _entry[N] { };
		Cbe::Block_data _crypto_data { };

		Cbe::Hash _entry_hash[N] { };

		uint32_t _levels { 0 };
		uint32_t _io_levels { 0 };
		bool _io { false };

		Cbe::Primitive _pending_primitive { };
		Cbe::Virtual_block_address _vba { ~0ull };
		Cbe::Generation _new_generation { 0 };

	public:

		Write_back() { }

		void update(Cbe::Physical_block_address const pba,
		            Cbe::Tree_helper const &tree,
		            Cbe::Block_data &data,
		            Cbe::Block_data const &old_data)
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];

				if (e.tag != Cbe::Tag::CACHE_TAG || e.pba != pba) {
					continue;
				}

				T *t = reinterpret_cast<T*>(&data);

				/* CoW action incoming */
				if (e.pba != e.update_pba) {
					MOD_DBG("copy ", e.pba, " -> ", e.update_pba);
					Genode::memcpy(t, (void const *)&old_data, sizeof (Cbe::Block_data));
				}

				/* save as long as only inner nodes in cache */
				Cbe::Physical_block_address const &child_update_pba = _entry[i-1].update_pba;
				Cbe::Hash                   const &child_hash    = _entry_hash[i-1];

				/* get index from VBA in inner node */
				Genode::uint32_t const index   = tree.index(_vba, i);
				Cbe::Generation  const old_gen = t[index].gen;
				t[index].pba = child_update_pba;
				t[index].gen = (old_gen & Cbe::GEN_TYPE_MASK) | _new_generation;
				Genode::memcpy(t[index].hash.values, child_hash.values, sizeof (Cbe::Hash));

				MOD_DBG("index: ", index, " child_update_pba: ", child_update_pba, " <", child_hash, ">");

				/* calculate hash */
				{
					Sha256_4k::Data *hash_data = reinterpret_cast<Sha256_4k::Data*>(&data);
					Sha256_4k::Hash &hash = *reinterpret_cast<Sha256_4k::Hash*>(&_entry_hash[i]);
					Sha256_4k::hash(*hash_data, hash);
				};

				e.done = true;
				break;
			}
		}

		bool primitive_acceptable() const { return !_pending_primitive.valid(); }

		void submit_primitive(Primitive const &p, Cbe::Generation new_generation,
		                      Cbe::Virtual_block_address vba,
		                      Cbe::Physical_block_address const *update_pba,
		                      Cbe::Type_1_node_info const *pba, uint32_t n, Block_data const &d)
		{
			_pending_primitive = p;
			_new_generation = new_generation;
			_vba = vba;

			_levels = n;
			_io = false;
			_io_levels = 1;

			/* handle common members */
			for (Genode::uint32_t i = 0; i < n; i++) {
				Entry &e = _entry[i];

				e.pba = pba[i].pba;
				e.update_pba = update_pba[i];

				MOD_DBG("i: ", i, " old: ", e.pba, " new: ", e.update_pba);

				e.in_progress = false;
				e.done        = false;
				e.io_in_progress = false;
				e.io_done        = false;
				e.tag = Cbe::Tag::CACHE_TAG;
			}

			/* the data or rather leave node is special */
			Entry &e = _entry[0];
			e.tag = Cbe::Tag::CRYPTO_TAG_ENCRYPT;
			Genode::memcpy(&_crypto_data, &d, sizeof (Cbe::Block_data));
		}

		bool execute()
		{
			if (!_pending_primitive.valid()) { return false; }

			if (_io) { return false; }

			bool done = true;
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				if (!e.done) {
					done = false;
					break;
				}
			}
			if (!done) { return false; }

			/* arm I/O and rebrand entries */
			for (Genode::uint32_t i = 0; i < _io_levels; i++) {
				Entry &e = _entry[i];
				e.tag = Cbe::Tag::IO_TAG;
			}
			_io = true;

			return true;
		}

		Primitive peek_completed_primitive()
		{
			if (!_pending_primitive.valid()) { return Primitive { }; }
			if (!_io) { return Primitive { }; }

			bool completed = true;
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				if (e.tag != Cbe::Tag::IO_TAG) { continue; }
				if (!e.io_done) {
					completed = false;
					break;
				}
			}

			if (completed) {
				_pending_primitive.success = Cbe::Primitive::Success::TRUE;
				return _pending_primitive;
			}

			return Primitive { };
		}

		Cbe::Physical_block_address peek_completed_root(Cbe::Primitive const &p) const
		{
			(void)p;

			Entry const &e = _entry[_levels-1];
			return e.update_pba;
		}

		void peek_competed_root_hash(Cbe::Primitive const &p, Cbe::Hash &hash) const
		{
			(void)p;
			Genode::memcpy(hash.values, _entry_hash[_levels-1].values, sizeof (Cbe::Hash));
		}

		void drop_completed_primitive(Cbe::Primitive const &p)
		{
			if (p.block_number == _pending_primitive.block_number) {
				_pending_primitive = Cbe::Primitive { };
				return;
			}

			MOD_DBG("invalid primitive");
			throw -1;
		}

		bool crypto_done() const
		{
			return _entry[0].done;
		}

		Primitive peek_generated_primitive()
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				if (e.in_progress || e.done) { continue; }

				bool const cache = e.tag == Cbe::Tag::CACHE_TAG;
				Cbe::Primitive::Number const block_number = cache ? e.pba : e.update_pba;

				MOD_DBG("block_number: ", block_number);

				return Primitive {
					.tag          = e.tag,
					.operation    = Cbe::Primitive::Operation::WRITE,
					.success      = Cbe::Primitive::Success::FALSE,
					.block_number = block_number,
					.index        = 0,
				};
			}
			return Cbe::Primitive { };
		}

		Cbe::Block_data &peek_generated_crypto_data(Cbe::Primitive const &p)
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				bool const cache = e.tag == Cbe::Tag::CACHE_TAG;
				bool const match = p.block_number == (cache ? e.pba : e.update_pba);
				if (match) { return _crypto_data; }
			}

			MOD_ERR("invalid primitive");
			throw -1;
		}

		Cbe::Physical_block_address peek_generated_pba(Cbe::Primitive const &p) const
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry const &e = _entry[i];
				bool const cache = e.tag == Cbe::Tag::CACHE_TAG;
				bool const match = cache && p.block_number == e.pba;
				if (match) { return e.update_pba; }
			}

			Cbe::Physical_block_address const pba = p.block_number;
			MOD_ERR("invalid primitive: ", pba);
			throw -1;
		}

		void drop_generated_primitive(Cbe::Primitive const &p)
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				bool const cache = e.tag == Cbe::Tag::CACHE_TAG;
				bool const match = p.block_number == (cache ? e.pba : e.update_pba);
				if (!match) { continue; }
				
				e.in_progress = true;
				return;
			}

			MOD_ERR("invalid primitive");
			throw -1;
		}

		void mark_completed_crypto_primitive(Cbe::Primitive const &p)
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				bool const crypto = e.tag == Cbe::Tag::CRYPTO_TAG_ENCRYPT;
				bool const match = p.block_number == e.update_pba;
				if (!crypto || !match) { continue; }

				Sha256_4k::Data *data = reinterpret_cast<Sha256_4k::Data*>(&_crypto_data);
				Sha256_4k::Hash hash { };
				Sha256_4k::hash(*data, hash);
				Genode::memcpy(_entry_hash[i].values, hash.values, sizeof (hash));

				MOD_DBG("i: ", i, " update_pba: ", e.update_pba, " <", _entry_hash[i], ">");

				e.done = true;
				return;
			}

			MOD_ERR("invalid primitive");
			throw -1;
		}

		Primitive peek_generated_io_primitive()
		{
			Primitive p { };

			if (!_io) { return p; }

			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];

				if (e.tag != Cbe::Tag::IO_TAG) { continue; }

				if (e.io_in_progress || e.io_done || !e.done) { continue; }

				MOD_DBG("i: ", i, " pba: ", e.update_pba);

				p = Primitive {
					.tag          = Cbe::Tag::IO_TAG,
					.operation    = Cbe::Primitive::Operation::WRITE,
					.success      = Cbe::Primitive::Success::FALSE,
					.block_number = e.update_pba,
					.index        = 0,
				};
				break;
			}
			return p;
		}

		Cbe::Block_data &peek_generated_io_data(Cbe::Primitive const &p)
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				bool const match = p.block_number == e.update_pba;
				if (match) { return _crypto_data; }
			}

			MOD_ERR("invalid primitive");
			throw -1;
		}

		void drop_generated_io_primitive(Cbe::Primitive const &p)
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				bool const io = e.tag == Cbe::Tag::IO_TAG;
				bool const match = p.block_number == e.update_pba;
				if (!io || !match) { continue; }
				
				e.io_in_progress = true;
				return;
			}

			MOD_ERR("invalid primitive");
			throw -1;
		}

		void mark_completed_io_primitive(Cbe::Primitive const &p)
		{
			for (Genode::uint32_t i = 0; i < _levels; i++) {
				Entry &e = _entry[i];
				bool const io = e.tag == Cbe::Tag::IO_TAG;
				bool const match = p.block_number == e.update_pba;
				if (!io || !match) { continue; }

				MOD_DBG("i: ", i, " pba: ", e.update_pba, " io_done");
				e.io_done = true;
				return;
			}

			MOD_ERR("invalid primitive");
			throw -1;
		}
};

#undef MOD_NAME

#endif /* _CBE_WRITE_BACK_MODULE_H_ */
