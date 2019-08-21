/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_CACHE_FLUSHER_MODULE_H_
#define _CBE_CACHE_FLUSHER_MODULE_H_

/* Genode includes */
#include <base/sleep.h>

/* local includes */
#include <cbe/types.h>
#include <cbe/spark_object.h>

namespace Cbe { namespace Module {

	class Cache_flusher;

	Genode::uint32_t object_size(Cache_flusher const &);

} /* namespace Module */ } /* namespace Cbe */

#define MOD_NAME "FLSHR"

struct Cbe::Module::Cache_flusher : Cbe::Spark_object<512>
{
	/**
	 * Constructor
	 */
	Cache_flusher();

	/**
	 * Check if the module can accept a request to flush a cache entry
	 *
	 * \return true if a request can be accepted, otherwise false
	 */
	bool request_acceptable() const;

	bool cxx_request_acceptable() const
	{
		bool const res = request_acceptable();
		MOD_DBG("res: ", res);
		return res;
	}

	/**
	 * Submit a new request
	 *
	 * This method must only be called after executing 'request_acceptable'
	 * returned true.
	 *
	 * \param pba  physical block address of the block data
	 * \param idx  index of the cache data entry corresponding to the physical block address
	 */
	void submit_request(Cbe::Physical_block_address const pba, Cache_Index const idx);
	void cxx_submit_request(Cbe::Physical_block_address const pba, Cache_Index const idx)
	{
		MOD_DBG("pba: ", pba, " idx: ", idx.value);
		submit_request(pba, idx);
	}

	/**
	 * Get a completed primitive
	 *
	 * \return next valid generated primitive in case a primitive is pending,
	 *         otherwise an invalid primitive will be returned
	 */
	Cbe::Primitive peek_completed_primitive() const;
	Cbe::Primitive cxx_peek_completed_primitive() const
	{
		Cbe::Primitive prim = peek_completed_primitive();
		if (prim.valid()) {
			MOD_DBG("prim: ", prim);
		}
		return prim;
	}


	/**
	 * Discard given completed primitive
	 *
	 * \param  p  reference to primitive
	 */
	void drop_completed_primitive(Cbe::Primitive const &p);
	void cxx_drop_completed_primitive(Cbe::Primitive const &p)
	{
		MOD_DBG("prim: ", p);
		drop_completed_primitive(p);
	}

	/**
	 * Get a generated primitive
	 *
	 * \return next valid generated primitive in case a primitive is pending,
	 *         otherwise an invalid primitive will be returned
	 */
	Cbe::Primitive peek_generated_primitive() const;
	Cbe::Primitive cxx_peek_generated_primitive() const
	{
		Cbe::Primitive prim = peek_generated_primitive();
		if (prim.valid()) {
			MOD_DBG("prim: ", prim);
		}
		return prim;
	}

	/**
	 * Get index of data for given primitive
	 *
	 * This method must only be called after 'peek_generated_primitive' returned
	 * a valid primitive.
	 *
	 * \param  p   reference to primitive object
	 *
	 * \return index
	 */
	Cache_Index peek_generated_data_index(Cbe::Primitive const &p);
	Cache_Index cxx_peek_generated_data_index(Cbe::Primitive const &p)
	{
		Cache_Index idx = peek_generated_data_index(p);
		MOD_DBG("prim: ", p, " index: ", idx.value);
		return idx;
	}

	/**
	 * Discard given generated primitive
	 *
	 * \param  p  reference to primitive
	 */
	void drop_generated_primitive(Cbe::Primitive const &p);
	void cxx_drop_generated_primitive(Cbe::Primitive const &p)
	{
		MOD_DBG("prim: ", p);
		drop_generated_primitive(p);
	}

	/**
	 * Mark the generated primitive as completed
	 *
	 * \param p  reference to Primitive that is used to mark
	 *           the corresponding internal primitive as completed
	 */
	void mark_generated_primitive_complete(Cbe::Primitive const &p);
	void cxx_mark_generated_primitive_complete(Cbe::Primitive const &p)
	{
		MOD_DBG("prim: ", p);
		mark_generated_primitive_complete(p);
	}
};

#undef MOD_NAME

#endif /* _CBE_CACHE_FLUSHER_MODULE_H_ */
