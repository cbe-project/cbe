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
#include <translation_module.h>


namespace Cbe { namespace Module {

	struct Write_back_data
	{
		enum { NUM_ITEMS = 1, };
		Cbe::Block_data item[NUM_ITEMS];
	} __attribute__((packed));

	struct Write_back_data_index
	{
		uint32_t value;
	};

	class Write_back;

	Genode::uint32_t object_size(Module::Write_back const &);

} /* namespace Module */ } /* namespace Cbe */

#define MOD_NAME "WRBCK"

/*
 * The Write_back module is used to update the virtual-block-device in
 * a CoW fashion whenever new data is written to the device.
 *
 * It operates in the following steps:
 *    1. (CRYPTO)   it hands the leaf data to the Crypto module for encryption
 *    2. (IO)       it hands the encrypted leaf data to I/O module to write it
 *                  to the block device
 *    3. (CACHE)    starting by the lowest inner node it will update the node
 *                  entry (pba and hash)
 *    4. (COMPLETE) it returns the new root pba and root hash
 *
 * To reduce the tag handling in the arbiter this module contains methods
 * necessary for each step so that the arbiter simply polls these ones.
 */
struct Cbe::Module::Write_back : public Noncopyable, Cbe::Spark_object<392>
{
	public:

		static constexpr Genode::uint32_t N = Translation::MAX_LEVELS;

		Write_back();

		void update(Cbe::Physical_block_address const pba,
		            Cbe::Tree_helper            const &tree,
		            Cbe::Block_data             const &data,
		            Cbe::Block_data                   &update_data);

		bool primitive_acceptable() const;

		void submit_primitive(Cbe::Primitive              const &p,
		                      Cbe::Generation             const gen,
		                      Cbe::Virtual_block_address  const vba,
		                      Cbe::Physical_block_address const new_pba[Translation::MAX_LEVELS],
		                      Cbe::Type_1_node_info       const old_pba[Translation::MAX_LEVELS],
		                      uint32_t                    const n,
		                      Cbe::Block_data             const &d,
		                      Write_back_data                   &wb_data);

		Cbe::Primitive peek_completed_primitive();

		Cbe::Physical_block_address peek_completed_root(Cbe::Primitive const &p) const;

		void peek_competed_root_hash(Cbe::Primitive const &p, Cbe::Hash &hash) const;

		void drop_completed_primitive(Cbe::Primitive const &p);

		Primitive peek_generated_crypto_primitive() const;

		Write_back_data_index peek_generated_crypto_data(Cbe::Primitive const &p);

		void drop_generated_crypto_primitive(Cbe::Primitive const &p);

		void mark_completed_crypto_primitive(Cbe::Primitive  const &p,
		                                     Cbe::Block_data const &crypto_data);

		Primitive peek_generated_io_primitive() const;

		Write_back_data_index peek_generated_io_data(Cbe::Primitive const &p);

		void drop_generated_io_primitive(Cbe::Primitive const &p);

		void mark_completed_io_primitive(Cbe::Primitive  const &p);

		Primitive peek_generated_cache_primitive() const;

		Cbe::Physical_block_address peek_generated_cache_update_pba(Cbe::Primitive const &p) const;

		void drop_generated_cache_primitive(Cbe::Primitive const &p);
};

#undef MOD_NAME

#endif /* _CBE_WRITE_BACK_MODULE_H_ */
