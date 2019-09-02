/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_IO_MODULE_H_
#define _CBE_IO_MODULE_H_

/* Genode includes */
#include <block_session/connection.h>

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	struct Io_data
	{
		enum { NUM_ITEMS = 1, };
		Cbe::Block_data item[NUM_ITEMS];
	} __attribute__((packed));

	class Block_io;

	Genode::uint32_t object_size(Module::Block_io const &);

} /* namespace Module */ } /* namespace Cbe */

#define MOD_NAME "IO"

struct Cbe::Module::Block_io : Noncopyable, Cbe::Spark_object<48>
{
	struct Block_size_mismatch : Genode::Exception { };

	static constexpr Genode::uint32_t N = Io_data::NUM_ITEMS;

	struct Index
	{
		static constexpr Genode::uint32_t INVALID = ~0u;
		unsigned value;
		bool valid() const { return value != INVALID; }
	};

	Block_io();

	bool primitive_acceptable() const;

	void submit_primitive(Tag const tag, Primitive const &p,
	                      Io_data &io_data, Block_data &data, bool checked = false);

	Primitive peek_completed_primitive();

	uint32_t peek_completed_data_index(Cbe::Primitive const &p);

	Cbe::Tag peek_completed_tag(Cbe::Primitive const &p);

	void drop_completed_primitive(Cbe::Primitive const &p);

	Cbe::Primitive peek_generated_primitive();

	uint32_t peek_generated_data_index(Cbe::Primitive const &p);

	void drop_generated_primitive(Cbe::Primitive const &p);

	void mark_generated_primitive_complete(Cbe::Primitive const &p);
};

#undef MOD_NAME

#endif /* _CBE_IO_MODULE_H_ */
