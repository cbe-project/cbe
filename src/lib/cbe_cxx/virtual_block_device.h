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

	Genode::uint32_t object_size(Virtual_block_device const &);

} /* namespace Cbe */


#define MOD_NAME "VBD"

struct Cbe::Virtual_block_device : Cbe::Spark_object<416>
{
	using Cache          = Module::Cache;
	using Cache_Index    = Module::Cache_Index;
	using Cache_Data     = Module::Cache_Data;

	using Translation      = Module::Translation;
	using Translation_Data = Module::Translation_Data;

	Virtual_block_device(Cbe::Height           const height,
	                     Cbe::Degree           const degree,
	                     Cbe::Number_of_leaves const leafs);

	void trans_inhibit_translation();

	void trans_resume_translation();

	Cbe::Primitive::Number trans_get_virtual_block_address(Cbe::Primitive const &p);

	bool trans_can_get_type_1_info(Cbe::Primitive const &p,
	                               Cbe::Type_1_node_info info[Translation::MAX_LEVELS]);

	void trans_get_type_1_info(Cbe::Type_1_node_info info[Translation::MAX_LEVELS]);

	Cbe::Height tree_height() const;

	uint32_t index_for_level(Cbe::Virtual_block_address const vba,
	                         uint32_t                   const level) const;

	Cbe::Tree_helper const tree_helper() const;

	bool primitive_acceptable() const;

	void submit_primitive(Cbe::Physical_block_address const  pba,
	                      Cbe::Generation             const  gen,
	                      Cbe::Hash                   const &hash,
	                      Cbe::Primitive              const &prim);

	void execute(Translation_Data &trans_data,
	             Cache            &cache,
	             Cache_Data       &cache_data,
	             Time::Timestamp   timestamp);

	bool execute_progress() const;

	Cbe::Primitive peek_completed_primitive();

	void drop_completed_primitive(Cbe::Primitive const &prim);
};

#undef MOD_NAME

#endif /* _CBE_VIRTUAL_BLOCK_DEVICE_H_ */
