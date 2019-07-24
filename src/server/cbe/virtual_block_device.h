/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_VIRTUAL_BLOCK_DEVICE_H_
#define _CBE_VIRTUAL_BLOCK_DEVICE_H_

/* local includes */
#include <cbe/types.h>


namespace Cbe {

	struct Virtual_block_device;

} /* namespace Cbe */


struct Cbe::Virtual_block_device
{
	using Cache          = Module::Cache;
	using Cache_Index    = Module::Cache_Index;
	using Cache_Data     = Module::Cache_Data;
	using Cache_Job_Data = Module::Cache_Job_Data;

	Cache _cache { };

	using Translation      = Module::Translation;
	using Translation_Data = Module::Translation_Data;

	bool _show_progress { false };

	Constructible<Tree_helper> _trans_helper { };
	Constructible<Translation> _trans { };

	Virtual_block_device(Cbe::Height           const height,
	                     Cbe::Degree           const degree,
	                     Cbe::Number_of_leaves const leafs)
	{
		_trans_helper.construct(degree, height, leafs);
		_trans.construct(*_trans_helper, false);
	}

	/**********************
	 ** Module interface **
	 **********************/

	bool primitive_acceptable() const
	{
		return _trans->acceptable();
	}

	void submit_request(Cbe::Physical_block_address const  pba,
	                    Cbe::Generation             const  gen,
	                    Cbe::Hash                   const &hash,
	                    Cbe::Primitive              const &prim)
	{
		_trans->submit_primitive(pba, gen, hash, prim);
	}

	bool execute(Translation_Data &trans_data,
	             Cache_Data       &cache_data,
	             Cache_Job_Data   &cache_job_data,
	             Time             &time)
	{
		bool progress = false;

		/**************************
		 ** Translation handling **
		 **************************/

		bool const trans_progress = _trans->execute(trans_data);
		progress |= trans_progress;
		if (_show_progress) {
			Genode::log("Translation progress: ", trans_progress);
		}

		while (true) {

			Cbe::Primitive p = _trans->peek_generated_primitive();
			if (!p.valid()) { break; }

			Cbe::Physical_block_address const pba = p.block_number;
			if (!_cache.data_available(pba)) {

				if (_cache.request_acceptable(pba)) {
					_cache.submit_request(pba);
				}
				break;
			} else {

				Cache_Index     const idx   = _cache.data_index(pba,
				                                                time.timestamp());
				Cbe::Block_data const &data = cache_data.item[idx.value];
				_trans->mark_generated_primitive_complete(p, data, trans_data);

				_trans->discard_generated_primitive(p);
			}

			progress |= true;
		}

		/********************
		 ** Cache handling **
		 ********************/

		bool const cache_progress = _cache.execute(cache_data, cache_job_data,
		                                           time.timestamp());
		progress |= cache_progress;
		if (_show_progress) {
			Genode::log("Cache progress: ", cache_progress);
		}

		return progress;
	}

	Cbe::Primitive peek_generated_primitive() /* const */
	{
		/* cache */
		{
			Cbe::Primitive prim = _cache.peek_generated_primitive();
			if (prim.valid()) { return prim; }
		}

		return Cbe::Primitive { };
	}

	Index peek_generated_data_index(Cbe::Primitive const &prim) /* const */
	{
		Index idx { .value = ~0u };

		switch (prim.tag) {
		case Tag::CACHE_TAG:
		{
			// XXX move _cache into _handle_requests so that data_index() can be used
			Cache_Index const cidx = _cache.peek_generated_data_index(prim);
			idx.value = cidx.value;
			break;
		}
		default: break;
		}

		return idx;
	}

	void drop_generated_primitive(Cbe::Primitive const &prim)
	{
		switch (prim.tag) {
		case Tag::CACHE_TAG:
			_cache.drop_generated_primitive(prim);
			break;
		default:
			Genode::error(__func__, ": invalid primitive");
			break;
		}
	}

	void mark_generated_primitive_complete(Cbe::Primitive const &prim)
	{
		switch (prim.tag) {
		case Tag::CACHE_TAG:
			_cache.mark_completed_primitive(prim);
			break;
		default:
			Genode::error(__func__, ": invalid primitive");
		break;
		}
	}

	Cbe::Primitive peek_completed_primitive()
	{
		/* trans */
		{
			Cbe::Primitive prim = _trans->peek_completed_primitive();
			if (prim.valid()) { return prim; }
		}

		Genode::error(__func__, ": invalid primitive");
		return Cbe::Primitive { };
	}

	void drop_completed_primitive(Cbe::Primitive const &prim)
	{
		_trans->drop_completed_primitive(prim);
	}
};

#endif /* _CBE_VIRTUAL_BLOCK_DEVICE_H_ */
