/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_TRANSLATION_MODULE_H_
#define _CBE_TRANSLATION_MODULE_H_

/* repo includes */
#include <util/sha256_4k.h>

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	template <typename> class Translation;

} /* namespace Module */ } /* namespace Cbe */


template <typename T>
class Cbe::Module::Translation
{
	public:

		static constexpr uint32_t MAX_LEVELS = 6;

	private:

		static inline uint32_t _log2(uint32_t value)
		{
			if (!value) return -1;
			for (int i = 8 * sizeof(value) - 1; i >= 0; --i)
				if (((uint32_t)1 << i) & value) return i;

			return -1;
		}

		uint32_t _degree_log2;

		uint32_t _get_index(Cbe::Primitive::Number const vba,
		                           uint32_t        const level) const
		{
			static uint32_t const mask = (1u << _degree_log2) - 1;
			return (vba >> (_degree_log2*(level-1))) & mask;
		}

		Cbe::Primitive::Number _get_pba(Cbe::Block_data const &data, uint32_t i) const
		{
			T const *entry = reinterpret_cast<T const*>(&data);
			return entry[i].pba;
		}

		Cbe::Hash const *_get_hash(Cbe::Block_data const &data, uint32_t i) const
		{
			T const *entry = reinterpret_cast<T const*>(&data);
			return &entry[i].hash;
		}


		struct Data
		{
			uint32_t        _avail { 0u };
			Cbe::Block_data _data[MAX_LEVELS] { };

			bool available(uint32_t level) const { return _avail & (1u << (level-1)); }

			void set_available(uint32_t level) { _avail |= (1u << (level-1)); }

			void reset() { _avail = 0u; }

			Cbe::Block_data &data(uint32_t level) { return _data[level-1]; }

			Cbe::Block_data const &data(uint32_t level) const { return _data[level-1]; }
		};

		Data _data { };

		uint32_t const _max_height;

		Cbe::Primitive::Number _root { ~0ull };
		Cbe::Hash              _root_hash { };

		Cbe::Primitive  _current    { };
		uint32_t        _level      { ~0u };
		Cbe::Primitive::Number _num { ~0ull };

		Cbe::Primitive::Number _pba { ~0ull };

		bool _suspended { false };

	public:

		Translation(uint32_t levels, uint32_t degree)
		: _degree_log2(_log2(degree)), _max_height(levels) { }

		/**
		 * Return height of the tree
		 *
		 * \return height of the tree
		 */
		uint32_t height() const { return _max_height; }

		/**
		 * Return index for address of given level
		 *
		 * \return index
		 */
		uint32_t index(Cbe::Primitive::Number const vba,
		               uint32_t               const level)
		{
			return _get_index(vba, level);
		}

		/**
		 * Check if the pool can accept a new request
		 *
		 * \return true if the request can be accepted, otherwise false
		 */
		bool acceptable() const
		{
			return !_current.valid() && !_suspended;
		}


		void suspend()
		{
			Genode::log(__func__, " SUSPEND TRANSLATION");
			_suspended = true;
		}

		void resume()
		{
			Genode::log(__func__, " RESUME TRANSLATION");
			_suspended = false;
		}

		/**
		 * Submit a new translation request
		 *
		 * \param r  physical block address of the root of the tree
		 * \param p  referencet to the primitive
		 */
		void submit_primitive(Cbe::Physical_block_address r,
		                      Cbe::Hash const &root_hash,
		                      Cbe::Primitive const &p)
		{
			if (_current.valid()) { throw -1; }

			_data.reset();

			// Cbe::Primitive::Number number = p.block_number;
			// Genode::log("Translation::", __func__, ": level: ", _max_height, " root: ", r, " vba: ", number);

			_root      = r;
			Genode::memcpy(_root_hash.values, root_hash.values, sizeof (Cbe::Hash));
			_current   = p;
			_level     = _max_height;
			_pba       = ~0ull;
		}

		bool execute()
		{
			/* no active translation request */
			if (!_current.valid()) { return false; }

			/* request already translated */
			if (_pba != ~0ull) { return false; }

			if (!_data.available(_level)) {

				/* data request already pending */
				if (_num != ~0ull) { return false; }

				/* otherwise start from root */
				if (_level == _max_height) {
					_num = _root;
				} else {

					/* or use previous level data to get next level */
					uint32_t const i = _get_index(_current.block_number, _level+1);
					_num = _get_pba(_data.data(_level+1), i);
				}

				return true;
			}

			/*
			 * Check hash
			 */
			struct Hash_mismatch { };
			Sha256_4k::Hash hash { };
			Sha256_4k::Data const &data =
				*reinterpret_cast<Sha256_4k::Data const*>(&_data.data(_level));
			Sha256_4k::hash(data, hash);

			Cbe::Hash const *h = nullptr;
			if (_level == _max_height) {
				h = &_root_hash;
			} else {
				/* or use previous level data to get next level */
				uint32_t   const i = _get_index(_current.block_number, _level+1);
				h = _get_hash(_data.data(_level+1), i);
			}

			if (Genode::memcmp(hash.values, h->values, sizeof (Cbe::Hash))) {
				Genode::error("level: ", _level, " <", hash, "> != <", *h, ">");
				throw Hash_mismatch();
			}

			/*
			 * We query the next level and should it already be the last,
			 * we have found the pba for the data leave node.
			 */
			if (--_level == 0) {
				uint32_t const i = _get_index(_current.block_number, 1);
				_pba = _get_pba(_data.data(1), i);

			}

			return true;
		}

		/**
		 *
		 */
		bool peek_completed_primitive() const { return _pba != ~0ull; }

		/**
		 *
		 */
		Cbe::Primitive take_completed_primitive()
		{
			return Cbe::Primitive {
				.tag          = _current.tag,
				.operation    = _current.operation,
				.success      = Cbe::Primitive::Success::FALSE,
				.block_number = _pba,
				.index        = _current.index,
			};
		}

		/**
		 * 
		 */
		Cbe::Primitive::Number get_virtual_block_address(Cbe::Primitive const &p)
		{
			if (p.block_number != _pba) { throw -2; }

			return _current.block_number;
		}


		bool get_physical_block_addresses(Cbe::Primitive const &p,
		                                  Cbe::Physical_block_address *pba, size_t n)
		{
			if (_pba == ~0ull || p.block_number != _pba || !pba || n > MAX_LEVELS) { return false; }

			pba[_max_height] = _root;
			for (uint32_t l = _max_height; l > 0; l--) {
					uint32_t const i = _get_index(_current.block_number, l);
					pba[l-1] = _get_pba(_data.data(l), i);
			}

			return true;
		}

		void dump() const
		{
			Cbe::Physical_block_address pba[MAX_LEVELS] { };
			pba [_max_height] = _root;
			for (uint32_t l = _max_height; l > 0; l--) {
				uint32_t const i = _get_index(_current.block_number, l);
				pba[l-1] = _get_pba(_data.data(l), i);
			}

			Genode::error(__func__, ": ", pba[0], " -> ", pba[1], " -> ", pba[2]);
		}


		/**
		 *
		 */
		void discard_completed_primitive(Cbe::Primitive const &p)
		{
			(void)p;
			// Cbe::Primitive::Number number = p.block_number;
			// Genode::log("Translation::", __func__, ": vba: ", number);

			/* allow further translation requests */
			_current = Cbe::Primitive { };

			/* reset completed primitive */
			_pba = ~0ull;
		}

		/**
		 * Check for any generated primitive
		 *
		 * \return true if I/O primtive is pending, false otherwise
		 */
		bool peek_generated_primitive() const
		{
			return _num != ~0ull;
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
			return Cbe::Primitive {
				.tag          = _current.tag,
				.operation    = Cbe::Primitive::Operation::READ,
				.success      = Cbe::Primitive::Success::FALSE,
				.block_number = _num,
				.index        = _current.index,
			};
		}

		/**
		 * Discard
		 */
		void discard_generated_primitive(Cbe::Primitive const &p)
		{
			if (p.block_number != _num) { return; }

			_num = ~0ull;
		}

		/**
		 * Mark the primitive as completed
		 *
		 * \param p  reference to Primitive that is used to mark
		 *           the corresponding internal primitive as completed
		 */
		void mark_generated_primitive_complete(Cbe::Primitive const  &p,
		                                       Cbe::Block_data const &data)
		{
			(void)p;

			_data.set_available(_level);
			Genode::memcpy(&_data.data(_level), &data, sizeof (Cbe::Block_data));

			/* XXX should be taken care of by discard_generated_primitive */
			_num = ~0ull;
		}
};

#endif /* _CBE_TRANSLATION_MODULE_H_ */
