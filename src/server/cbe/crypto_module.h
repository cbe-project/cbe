/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_CRYPTO_MODULE_H_
#define _CBE_CRYPTO_MODULE_H_

/* library includes */
#include <aes_cbc_4k/aes_cbc_4k.h>

/* local includes */
#include <cbe/types.h>
#include <spark_object.h>


namespace Cbe { namespace Module {

	class Crypto;

} /* namespace Module */ } /* namespace Cbe */


struct Cbe::Module::Crypto : Spark::Object<8264>
{
	Crypto(char const *key /* must be 32b total */,
	       Genode::size_t obj_sz  = sizeof (Crypto),
	       Genode::size_t data_sz = sizeof (Cbe::Block_data),
	       Genode::size_t prim_sz = sizeof (Primitive));

	/**
	 * Check if the module can accept a new primitive
	 *
	 * \return true if a primitive can be accepted, otherwise false
	 */
	bool primitive_acceptable() const;

	/**
	 * Submit a new primitive
	 *
	 * The primitive will be copied to the internal buffer and the Block_data
	 * reference will be stored as a reference. The method may only be called
	 * after 'acceptable' was executed and returned true. The new primitive is
	 * marked as submitted and waits for execution.
	 *
	 * \param p  reference to the Primitive
	 * \param d  reference to a Block_data object
	 * \param c  reference to a Block_data cipher object
	 */
	void submit_primitive(Primitive const &p, Block_data &d, Block_data &c);

	/**
	 * Process all submitted primitives
	 *
	 * This method tries to process any submitted request. In case it is a
	 * write request the Block_data will be encrypted and the primitive is
	 * marked as complete. On the other hand, if it is a read request, the
	 * primitive will be marked as pending and will later on be passed on
	 * to a I/O module.
	 */
	void execute();
	bool execute_progress();

	/**
	 * Check for any completed primitive
	 *
	 * The method will always a return a primitive. The caller can
	 * check which subsequent module must be used to process the
	 * Primitive further as the process-chain might differ for read
	 * and write primtives. The caller always has to check if the
	 * returned primitive is in fact a valid one.
	 *
	 * \return a valid Primitive will be returned if there is an
	 *         completed primitive, otherwise an invalid one
	 */
	Primitive peek_completed_primitive() const;

	/**
	 * Take the next completed primitive
	 *
	 * This method must only be called after executing
	 * 'peek_completed_primitive' returned true.
	 *
	 * \return takes next valid completed primitive and removes it
	 *         from the module
	 */
	void drop_completed_primitive(Primitive const &prim);

	/**
	 * Check for any generated primitive
	 *
	 * The method will always a return a primitive. The caller can
	 * check which subsequent module must be used to process the
	 * Primitive further as the process-chain might differ for read
	 * and write primtives. The caller always has to check if the
	 * returned primitive is in fact a valid one.
	 *
	 * \return a valid Primitive will be returned if there is an
	 *         completed primitive, otherwise an invalid one
	 */
	Primitive  peek_generated_primitive() const;

	/**
	 * Take the next generated crypto primitive
	 *
	 * This method must only be called after executing
	 * 'peek_generated_primitive' returned true.
	 */
	void drop_generated_primitive(Primitive const &prim);

	/**
	 * Mark the primitive as completed
	 *
	 * \param p  reference to Primitive that is used to lookup
	 *           the corresponding internal primitive as completed
	 */
	void mark_completed_primitive(Primitive const &p);
};

#endif /* _CBE_CRYPTO_MODULE_H_ */
