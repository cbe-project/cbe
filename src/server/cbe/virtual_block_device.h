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


#define MOD_NAME "VBD"

struct Cbe::Virtual_block_device
{
	using Cache          = Module::Cache;
	using Cache_Index    = Module::Cache_Index;
	using Cache_Data     = Module::Cache_Data;

	using Translation      = Module::Translation;
	using Translation_Data = Module::Translation_Data;

	Constructible<Tree_helper> _trans_helper { };
	Constructible<Translation> _trans { };

	Virtual_block_device(Cbe::Height           const height,
	                     Cbe::Degree           const degree,
	                     Cbe::Number_of_leaves const leafs)
	{
		_trans_helper.construct(degree, height, leafs);
		_trans.construct(*_trans_helper, false);
	}

	/**********************************
	 ** Translation module interface **
	 **********************************/

	void trans_inhibit_translation()
	{
		MOD_DBG("");
		_trans->suspend();
	}

	void trans_resume_translation()
	{
		MOD_DBG("");
		_trans->resume();
	}

	Cbe::Primitive::Number trans_get_virtual_block_address(Cbe::Primitive const &p)
	{
		return _trans->get_virtual_block_address(p);
	}

	bool trans_get_type_1_info(Cbe::Primitive const &p,
	                           Cbe::Type_1_node_info info[Translation::MAX_LEVELS])
	{
		return _trans->get_type_1_info(p, info);
	}

	Cbe::Height tree_height() const
	{
		return _trans_helper->height();
	}

	uint32_t index_for_level(Cbe::Virtual_block_address const vba,
	                         uint32_t                   const level) const
	{
		return _trans_helper->index(vba, level);
	}

	Cbe::Tree_helper const &tree_helper() const
	{
		return *_trans_helper;
	}

	/**********************
	 ** Module interface **
	 **********************/

	bool primitive_acceptable() const
	{
		return _trans->acceptable();
	}

	void submit_primitive(Cbe::Physical_block_address const  pba,
	                      Cbe::Generation             const  gen,
	                      Cbe::Hash                   const &hash,
	                      Cbe::Primitive              const &prim)
	{
		MOD_DBG("pba: ", pba, " gen: ", gen, " prim: ", prim);
		_trans->submit_primitive(pba, gen, hash, prim);
	}

	bool execute(Translation_Data &trans_data,
	             Cache            &cache,
	             Cache_Data       &cache_data,
	             Time             &time)
	{
		bool progress = false;

		/**************************
		 ** Translation handling **
		 **************************/

		bool const trans_progress = _trans->execute(trans_data);
		progress |= trans_progress;

		// XXX prevent module from checking the cache again and again

		while (true) {

			Cbe::Primitive p = _trans->peek_generated_primitive();
			if (!p.valid()) { break; }

			Cbe::Physical_block_address const pba = p.block_number;
			if (!cache.data_available(pba)) {

				MOD_DBG("data not available: pba: ", pba);
				if (cache.cxx_request_acceptable(pba)) {
					cache.cxx_submit_request(pba);
					MOD_DBG("submit cache request: pba: ", pba);
				}
				progress |= true;
				break;
			} else {

				Cache_Index     const idx   = cache.data_index(pba,
				                                               time.timestamp());
				Cbe::Block_data const &data = cache_data.item[idx.value];
				_trans->mark_generated_primitive_complete(p, data, trans_data);

				_trans->discard_generated_primitive(p);
				MOD_DBG("mark_generated_primitive_complete: pba: ", pba);
			}

			progress |= true;
		}

		return progress;
	}

	Cbe::Primitive peek_completed_primitive()
	{
		Cbe::Primitive prim = _trans->peek_completed_primitive();
		if (prim.valid()) { MOD_DBG(prim); }
		return prim;
	}

	void drop_completed_primitive(Cbe::Primitive const &prim)
	{
		MOD_DBG(prim);
		_trans->drop_completed_primitive(prim);
	}
};

#undef MOD_NAME

#endif /* _CBE_VIRTUAL_BLOCK_DEVICE_H_ */
