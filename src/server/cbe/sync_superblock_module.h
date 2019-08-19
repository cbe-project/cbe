/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_SYNC_SUPERBLOCK_MODULE_H_
#define _CBE_SYNC_SUPERBLOCK_MODULE_H_

/* local includes */
#include <cbe/types.h>
#include <cbe/spark_object.h>


namespace Cbe { namespace Module {

	class Sync_superblock;

	Genode::uint32_t object_size(Sync_superblock const &);

} /* namespace Module */ } /* namespace Cbe */


#define MOD_NAME "SSB"

/*
 * The Sync_superblock module will write the given super-block
 * back to the block device. There is a 1:1 mapping between the
 * id of the super-block and the physical-block-address where
 * the super-blocks resides. The id is also the index of the SB
 * within the SB array.
 */
struct Cbe::Module::Sync_superblock : Cbe::Spark_object<48>
{
	/**
	 * Constructor
	 */
	Sync_superblock();

	/**
	 * Check if the module can accept a request to synchronize a super-block
	 *
	 * The check depends on the state of the Job queue rather than the
	 * number of entries in the cache.
	 *
	 * \return true if a request can be accepted, otherwise false
	 */
	bool request_acceptable() const;
	bool cxx_request_acceptable() const
	{
		return request_acceptable();
	}

	/**
	 * Submit a new request
	 *
	 * This method must only be called after executing 'request_acceptable'
	 * returned true.
	 *
	 * \param id   id of the super-block
	 * \param gen  latest generation the super-block belongs to
	 */
	void submit_request(uint64_t id, Cbe::Generation const gen);
	void cxx_submit_request(uint64_t id, Cbe::Generation const gen)
	{
		MOD_DBG("id: ", id, " gen: ", gen);
		submit_request(id, gen);
	}

	/**
	 * Check for any completed primitive
	 *
	 * The method will always a return a primitive and the caller
	 * always has to check if the returned primitive is in fact a
	 * valid one.
	 *
	 * \return a valid Primitive will be returned if there is an
	 *         completed primitive, otherwise an invalid one
	 */
	Cbe::Primitive peek_completed_primitive() const;
	Cbe::Primitive cxx_peek_completed_primitive() const
	{
		Cbe::Primitive prim = peek_completed_primitive();
		if (prim.valid()) {
			MOD_DBG(prim);
		}
		return prim;
	}

	/**
	 * Get generation of the succesfully synchronized super-block
	 *
	 * This method must only be called after 'peek_completed_primitive'
	 * returned a valid primitive.
	 *
	 * \param p   reference to the completed primitive
	 *
	 * \return generation of the last synchronized super-block
	 */
	Cbe::Generation peek_completed_generation(Cbe::Primitive const &p) const;
	Cbe::Generation cxx_peek_completed_generation(Cbe::Primitive const &p) const
	{
		Cbe::Generation gen = peek_completed_generation(p);
		MOD_DBG(p, " gen: ", gen);
		return gen;
	}

	/**
	 * Discard given completed primitive
	 *
	 * This method must only be called after 'peek_completed_primitive'
	 * returned a valid primitive.
	 *
	 * \param  p  reference to primitive
	 */
	void drop_completed_primitive(Cbe::Primitive const &p);
	void cxx_drop_completed_primitive(Cbe::Primitive const &p)
	{
		MOD_DBG(p);
		drop_completed_primitive(p);
	}

	/**
	 * Check for any generated primitive
	 *
	 * The method will always a return a primitive and the caller
	 * always has to check if the returned primitive is in fact a
	 * valid one.
	 *
	 * \return a valid Primitive will be returned if there is an
	 *         generated primitive pending, otherwise an invalid one
	 */
	Cbe::Primitive peek_generated_primitive() const;
	Cbe::Primitive cxx_peek_generated_primitive() const
	{
		Cbe::Primitive prim = peek_generated_primitive();
		if (prim.valid()) {
			MOD_DBG(prim);
		}
		return prim;
	}

	/**
	 * Get id of the currently pending super-block
	 *
	 * This method must only be called after 'peek_generated_primitive'
	 * returned a valid primitive.
	 *
	 * \param p  reference to the completed primitive
	 *
	 * \return id of the super-block
	 */
	uint64_t peek_generated_id(Cbe::Primitive const &p) const;
	uint64_t cxx_peek_generated_id(Cbe::Primitive const &p) const
	{
		uint64_t gen = peek_generated_id(p);
		MOD_DBG(p, " gen: ", gen);
		return gen;
	}

	/**
	 * Discard given generated primitive
	 *
	 * This method must only be called after 'peek_generated_primitive'
	 * returned a valid primitive.
	 *
	 * \param  p  reference to primitive
	 */
	void drop_generated_primitive(Cbe::Primitive const &p);
	void cxx_drop_generated_primitive(Cbe::Primitive const &p)
	{
		MOD_DBG(p);
		drop_generated_primitive(p);
	}

	/**
	 * Mark given generated primitive as complete
	 *
	 * \param  p  reference to primitive
	 */
	void mark_generated_primitive_complete(Cbe::Primitive const &p);
	void cxx_mark_generated_primitive_complete(Cbe::Primitive const &p)
	{
		MOD_DBG(p);
		mark_generated_primitive_complete(p);
	}
};

#undef MOD_NAME

#endif /* _CBE_SYNC_SUPERBLOCK_MODULE_H_ */
