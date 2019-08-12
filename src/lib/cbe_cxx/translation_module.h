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

	enum { TRANSLATION_MAX_LEVELS = Cbe::TREE_MAX_HEIGHT, };

	struct Translation_Data
	{
		Cbe::Block_data item[1];
	} __attribute__((packed));

	class Translation;

	Genode::uint32_t object_size(Translation const &);

} /* namespace Module */ } /* namespace Cbe */

#define MOD_NAME "TRANS"

struct Cbe::Module::Translation : Cbe::Spark_object<384>
{
	static constexpr uint32_t MAX_LEVELS = 6;

	Translation(Cbe::Tree_helper &helper, bool free_tree);

	uint32_t height() const;

	uint32_t index(Cbe::Virtual_block_address const vba,
	               uint32_t                   const level);

	bool acceptable() const;

	void suspend();

	void resume();

	void submit_primitive(Cbe::Physical_block_address r,
	                      Cbe::Generation root_gen,
	                      Cbe::Hash const &root_hash,
	                      Cbe::Primitive const &p);

	void execute(Translation_Data &trans_data);

	bool execute_progress() const;

	Cbe::Primitive peek_completed_primitive();

	void drop_completed_primitive(Cbe::Primitive const &p);

	Cbe::Primitive::Number get_virtual_block_address(Cbe::Primitive const &p);

	bool can_get_type_1_info(Cbe::Primitive const &p,
	                         Cbe::Type_1_node_info info[Translation::MAX_LEVELS]);

	void get_type_1_info(Cbe::Type_1_node_info info[Translation::MAX_LEVELS]);

	Cbe::Primitive peek_generated_primitive() const;

	void discard_generated_primitive(Cbe::Primitive const &p);

	void mark_generated_primitive_complete(Cbe::Primitive const  &p,
	                                       Cbe::Block_data const &data,
	                                       Translation_Data &trans_data);
};

#undef MOD_NAME

#endif /* _CBE_TRANSLATION_MODULE_H_ */
