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

	enum { TRANSLATION_MAX_LEVELS = 6, };

	struct Translation_Data
	{
		Cbe::Block_data item[1];
	} __attribute__((packed));

	template <typename> class Translation;

} /* namespace Module */ } /* namespace Cbe */


template <typename T>
class Cbe::Module::Translation
{
	public:

		static constexpr uint32_t MAX_LEVELS = TRANSLATION_MAX_LEVELS;

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
			uint32_t _avail { 0u };

			bool available(uint32_t level) const { return _avail & (1u << (level-1)); }
			void set_available(uint32_t level) { _avail |= (1u << (level-1)); }

			void reset() { _avail = 0u; }
		};

		Cbe::Physical_block_address _walk[MAX_LEVELS] { };
		Cbe::Hash                   _walk_hash[MAX_LEVELS] { };

		Data _data { };

		uint32_t const _height;

		Cbe::Physical_block_address _root { Cbe::INVALID_PBA };
		Cbe::Hash              _root_hash { };

		Cbe::Primitive  _current    { };
		uint32_t        _level      { ~0u };
		Cbe::Physical_block_address _next_pba { Cbe::INVALID_PBA };

		Cbe::Physical_block_address _data_pba { Cbe::INVALID_PBA };

		bool _suspended { false };

	public:

		Translation(uint32_t levels, uint32_t degree)
		: _degree_log2(_log2(degree)), _height(levels) { }

		/**
		 * Return height of the tree
		 *
		 * \return height of the tree
		 */
		uint32_t height() const { return _height; }

		/**
		 * Return index for address of given level
		 *
		 * \return index
		 */
		uint32_t index(Cbe::Virtual_block_address const vba,
		               uint32_t                   const level)
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
			_suspended = true;
		}

		void resume()
		{
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

			_root      = r;
			Genode::memcpy(_root_hash.values, root_hash.values, sizeof (Cbe::Hash));
			_current   = p;
			_level     = _height;
			_data_pba  = Cbe::INVALID_PBA;
			_next_pba  = Cbe::INVALID_PBA;
		}

		bool execute(Translation_Data &trans_data)
		{
			/* no active translation request */
			if (!_current.valid()) { return false; }

			/* request already translated */
			if (_data_pba != Cbe::INVALID_PBA) { return false; }

			if (!_data.available(_level)) {

				/* data request already pending */
				if (_next_pba != Cbe::INVALID_PBA) { return false; }

				/* otherwise start from root */
				if (_level == _height) {
					_next_pba = _root;
					Genode::memcpy(_walk_hash[_level].values,
					               _root_hash.values, sizeof (Cbe::Hash));
				} else {

					/* or use previous level data to get next level */
					uint32_t const i = _get_index(_current.block_number, _level+1);
					_next_pba = _get_pba(trans_data.item[0], i);

					Genode::memcpy(_walk_hash[_level].values,
					               _get_hash(trans_data.item[0], i),
					               sizeof (Cbe::Hash));
				}

				_walk[_level] = _next_pba;
				return true;
			}

			/*
			 * Check hash
			 */
			struct Hash_mismatch { };
			Sha256_4k::Hash hash { };
			Sha256_4k::Data const &data =
				*reinterpret_cast<Sha256_4k::Data const*>(&trans_data.item[0]);
			Sha256_4k::hash(data, hash);

			Cbe::Hash const *h = nullptr;
			if (_level == _height) {
				h = &_root_hash;
			} else {
				/* or use previous level data to get next level */
				h = &_walk_hash[_level];
			}

			if (Genode::memcmp(hash.values, h->values, sizeof (Cbe::Hash))) {
				Genode::error("level: ", _level, " pba: ", _walk[_level], " <", hash, "> != <", *h, ">");
				for (uint32_t l = 0; l < _height+1; l++) {
					Genode::error("node[", l, "]: ", _walk[l], " <", _walk_hash[l], ">");
				}
				throw Hash_mismatch();
			}

			/*
			 * We query the next level and should it already be the last,
			 * we have found the pba for the data leave node.
			 */
			if (--_level == 0) {
				uint32_t const i = _get_index(_current.block_number, 1);
				_data_pba = _get_pba(trans_data.item[0], i);

				_walk[_level] = _data_pba;
				Genode::memcpy(_walk_hash[_level].values,
				               _get_hash(trans_data.item[0], i),
				               sizeof (Cbe::Hash));
			}

			return true;
		}

		/**
		 *
		 */
		Cbe::Primitive peek_completed_primitive()
		{
			if (_data_pba != Cbe::INVALID_PBA) {
				return Cbe::Primitive {
					.tag          = _current.tag,
					.operation    = _current.operation,
					.success      = Cbe::Primitive::Success::FALSE,
					.block_number = _data_pba,
					.index        = _current.index,
				};
			}

			return Cbe::Primitive { };
		}

		/**
		 *
		 */
		void drop_completed_primitive(Cbe::Primitive const &p)
		{
			if (p.block_number != _data_pba) {
				Genode::error(__func__, " invalid primitive");
				throw -1;
			}

			/* allow further translation requests */
			_current = Cbe::Primitive { };

			/* reset completed primitive */
			_data_pba = Cbe::INVALID_PBA;
		}

		/**
		 * 
		 */
		Cbe::Primitive::Number get_virtual_block_address(Cbe::Primitive const &p)
		{
			if (p.block_number != _data_pba) { throw -2; }

			return _current.block_number;
		}


		bool get_physical_block_addresses(Cbe::Primitive const &p,
		                                  Cbe::Physical_block_address *pba, size_t n)
		{
			if (_data_pba == Cbe::INVALID_PBA
			    || p.block_number != _data_pba
			    || !pba
			    || n > MAX_LEVELS) { return false; }

			for (uint32_t l = 0; l < _height+1; l++) {
				pba[l] = _walk[l];
			}

			return true;
		}

		void dump() const
		{
			Genode::error("WALK: ");
			for (uint32_t l = 0; l < _height+1; l++) {
				Genode::error("      ", _walk[_height-l], " <", _walk_hash[_height-l], ">");
			}
		}

		/**
		 * Check for any generated primitive
		 *
		 * \return true if I/O primtive is pending, false otherwise
		 */
		Cbe::Primitive peek_generated_primitive() const
		{
			if (_next_pba != Cbe::INVALID_PBA) {
				return Cbe::Primitive {
					.tag          = _current.tag,
					.operation    = Cbe::Primitive::Operation::READ,
					.success      = Cbe::Primitive::Success::FALSE,
					.block_number = _next_pba,
					.index        = _current.index,
				};
			}

			return Cbe::Primitive { };
		}

		/**
		 * Discard generated primitive
		 *
		 * \param p   reference to the primitive being discarded
		 */
		void discard_generated_primitive(Cbe::Primitive const &p)
		{
			if (p.block_number != _next_pba) {
				Genode::error(__func__, " invalid primitive");
				throw -1;
			}

			_next_pba = Cbe::INVALID_PBA;
		}

		/**
		 * Mark the primitive as completed
		 *
		 * \param p  reference to Primitive that is used to mark
		 *           the corresponding internal primitive as completed
		 */
		void mark_generated_primitive_complete(Cbe::Primitive const  &p,
		                                       Cbe::Block_data const &data,
		                                       Translation_Data &trans_data)
		{
			if (p.block_number != _next_pba) {
				Genode::error(__func__, " invalid primitive");
				throw -1;
			}

			_data.set_available(_level);
			Genode::memcpy(&trans_data.item[0], &data, sizeof (Cbe::Block_data));
		}
};

#endif /* _CBE_TRANSLATION_MODULE_H_ */
