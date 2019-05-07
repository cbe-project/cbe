/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_SPLITTER_MODULE_H_
#define _CBE_SPLITTER_MODULE_H_

/* cbe includes */
#include <cbe/types.h>

/* local includes */
#include <spark_object.h>


namespace Cbe { namespace Module {

	struct Splitter;

} /* namespace Module */ } /* namespace Cbe */

struct Cbe::Module::Splitter : Spark::Object<56>
{
	Splitter(size_t size      = sizeof(Splitter),
	         size_t req_size  = sizeof(Cbe::Request),
	         size_t prim_size = sizeof(Primitive));

	/**
	 * Get the number of primitives the request will generate
	 *
	 * \return number of primitives
	 */
	static Number_of_primitives number_of_primitives(Cbe::Request const &request);

	/**
	 * Check if the module can accept a new request
	 *
	 * \return true if a request can be accepted, otherwise false
	 */
	bool request_acceptable() const;

	/**
	 * Submit a new request
	 *
	 * The request will be copied to the internal buffer.
	 * The method may only be called after 'request_acceptable' was
	 * executed and returned true.
	 *
	 * \param r  reference to the Block request
	 */
	void submit_request(Cbe::Request const &request);

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
	Cbe::Primitive peek_generated_primitive() const;

	/**
	 * Drop the given primitive
	 */
	void drop_generated_primitive(Cbe::Primitive const &primtive);


	/***************
	 ** Accessors **
	 ***************/

	Cbe::Request curr_req() const;
};


#endif /* _CBE_SPLITTER_MODULE_H_ */
