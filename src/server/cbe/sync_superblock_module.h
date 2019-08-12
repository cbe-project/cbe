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

struct Cbe::Module::Sync_superblock : Cbe::Spark_object<48>
{
	/**
	 * Constructor
	 */
	Sync_superblock();

	bool request_acceptable() const;
	bool cxx_request_acceptable() const
	{
		return request_acceptable();
	}

	void submit_request(uint64_t id, Cbe::Generation const gen);
	void cxx_submit_request(uint64_t id, Cbe::Generation const gen)
	{
		MOD_DBG("id: ", id, " gen: ", gen);
		submit_request(id, gen);
	}

	Cbe::Primitive peek_completed_primitive() const;
	Cbe::Primitive cxx_peek_completed_primitive() const
	{
		Cbe::Primitive prim = peek_completed_primitive();
		if (prim.valid()) {
			MOD_DBG(prim);
		}
		return prim;
	}

	Cbe::Generation peek_completed_generation(Cbe::Primitive const &p) const;
	Cbe::Generation cxx_peek_completed_generation(Cbe::Primitive const &p) const
	{
		Cbe::Generation gen = peek_completed_generation(p);
		MOD_DBG(p, " gen: ", gen);
		return gen;
	}


	void drop_completed_primitive(Cbe::Primitive const &p);
	void cxx_drop_completed_primitive(Cbe::Primitive const &p)
	{
		MOD_DBG(p);
		drop_completed_primitive(p);
	}

	Cbe::Primitive peek_generated_primitive() const;
	Cbe::Primitive cxx_peek_generated_primitive() const
	{
		Cbe::Primitive prim = peek_generated_primitive();
		if (prim.valid()) {
			MOD_DBG(prim);
		}
		return prim;
	}

	uint64_t peek_generated_id(Cbe::Primitive const &p) const;
	uint64_t cxx_peek_generated_id(Cbe::Primitive const &p) const
	{
		uint64_t gen = peek_generated_id(p);
		MOD_DBG(p, " gen: ", gen);
		return gen;
	}

	void drop_generated_primitive(Cbe::Primitive const &p);
	void cxx_drop_generated_primitive(Cbe::Primitive const &p)
	{
		MOD_DBG(p);
		drop_generated_primitive(p);
	}

	void mark_generated_primitive_complete(Cbe::Primitive const &p);
	void cxx_mark_generated_primitive_complete(Cbe::Primitive const &p)
	{
		MOD_DBG(p);
		mark_generated_primitive_complete(p);
	}
};

#undef MOD_NAME

#endif /* _CBE_SYNC_SUPERBLOCK_MODULE_H_ */
