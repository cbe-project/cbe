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

template <unsigned N, typename T>
class Cbe::Module::Write_back
{
	private:

		struct Entry {
			Cbe::Block_data data { };
			bool modified { false };
		};

		Entry _entry[N] { };
		bool _in_progress { false };
		bool _pending { false };

	public:

		Write_back() { }

		bool primitive_acceptable() const { return !_in_progress && !_pending; }

		bool copy_and_update(Genode::uint32_t level, Cbe::Block_data const &orig,
		                     Genode::uint32_t index, Cbe::Physical_block_address pba)
		{
			if (_in_progress || level >= N) { return false; }

			_pending |= true;

			Genode::memcpy(&_entry[level].data, &orig, sizeof (orig));
			_entry[level].modified = true;

			T *t = reinterpret_cast<T*>(&_entry[level].data);
			t[index].pba = pba;

			for (Genode::uint32_t i = 0; i < 4; i++) {
				Cbe::Physical_block_address pba = t[i].pba;
				Genode::log(__func__, ": ", pba);
			}

			return true;
		}

		void commit() { _in_progress |= true; }

		void submit_primitive(Primitive const &p, Cbe::Physical_block_address new_pba,
		                      Cbe::Physical_block_address *old_pba, uint32_t n, Block_data &d)
		{
			(void)p;
			(void)new_pba;
			(void)old_pba;
			(void)n;
			(void)d;
		}

		bool execute()
		{
			return false;
		}

		Primitive peek_completed_primitive()
		{
			return Primitive { };
		}

		void drop_completed_primitive(Cbe::Primitive const &p)
		{
			(void)p;
		}

		Primitive peek_generated_primitive()
		{
			return Primitive { };
		}

		Cbe::Block_data _dummy { };

		Cbe::Block_data &peek_generated_data(Cbe::Primitive const &p)
		{
			(void)p;
			return _dummy;
		}


		void drop_generated_primitive(Cbe::Primitive const &p)
		{
			(void)p;
		}

		void mark_generated_primitive_complete(Cbe::Primitive const &p)
		{
			(void)p;

			_pending = false;
			_in_progress = false;
		}
};

#endif /* _CBE_WRITE_BACK_MODULE_H_ */
