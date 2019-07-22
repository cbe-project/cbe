/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_CACHE_MODULE_H_
#define _CBE_CACHE_MODULE_H_

/* Genode includes */
#include <base/sleep.h>

/* local includes */
#include <cbe/types.h>
#include <cbe/spark_object.h>

namespace Cbe { namespace Module {

	struct Cache_Data
	{
		Cbe::Block_data item[16];
	} __attribute__((packed));

	struct Cache_Job_Data
	{
		Cbe::Block_data item[1];
	} __attribute__((packed));

	struct Cache_Index
	{
		Genode::uint32_t value;
	};

	class Cache;

	Genode::uint32_t object_size(Cache const &);

} /* namespace Module */ } /* namespace Cbe */


struct Cbe::Module::Cache : Cbe::Spark_object<424>
{
	/**
	 * Constructor
	 */
	Cache();

	/**
	 * Check if the data for the given physical block address is in the cache
	 *
	 * \return true if data is available, otherwise false is returned
	 */
	bool data_available(Cbe::Physical_block_address const pba) const;

	/**
	 * Get index of data for given physical block address and update LRU
	 * value
	 *
	 * This method must only be called after executing 'data_available'
	 * returned true.
	 *
	 * \param  pba   physical block address
	 * \param  time  reference to time object used to update LRU
	 *
	 * \return index
	 */
	Cache_Index data_index(Cbe::Physical_block_address const pba,
	                       Cbe::Timestamp current_time);

	/**
	 * Invalid cache entry containg the physical block address
	 *
	 * \param  pba  physical block address
	 */
	void invalidate(Cbe::Physical_block_address const pba);

	/**
	 * Check if the module can accept a request to cache a block 
	 *
	 * The check depends on the state of the Job queue rather than the
	 * number of entries in the cache.
	 *
	 * \return true if a request can be accepted, otherwise false
	 */
	bool request_acceptable(Cbe::Physical_block_address pba) const;

	/**
	 * Submit a new request
	 *
	 * This method must only be called after executing 'request_acceptable'
	 * returned true.
	 *
	 * \param pba  physical block address of the block data
	 */
	void submit_request(Cbe::Physical_block_address const pba);

	/**
	 * Fill cache
	 *
	 * This method fills the cache from any completed primitive.
	 *
	 * \param data          reference to the shared data memory buffer
	 * \param job_data      reference to the shared job data memory buffer
	 * \param current_time  current time at the time of the operation
	 */
	void fill_cache(Cache_Data &data, Cache_Job_Data const &job_data,
	                Cbe::Timestamp current_time);

	/**
	 * Report progress of last fill-cache operation
	 *
	 * \return true if any completed primitive was processed
	 */
	bool progress() const;

	/**
	 * Execute all pending requests
	 *
	 * This method calls 'fill_cache' and 'progress' internally and
	 * is just a C++-side wrapper.
	 *
	 * \return true if any completed primitive was processed
	 */
	bool execute(Cache_Data &data, Cache_Job_Data const &job_data,
	             Cbe::Timestamp current_time)
	{
		fill_cache(data, job_data, current_time);
		return progress();
	}

	/**
	 * Take the next generated primitive
	 *
	 * This method must only be called after executing
	 * 'peek_generated_primitive' returned true.
	 *
	 * \return next valid generated primitive in case a Job is pending,
	 *         otherwise an invalid primitive will be returned
	 */
	Cbe::Primitive peek_generated_primitive();

	/**
	 * Get index of the data buffer belonging to the given primitive
	 *
	 * This method must only be called after executing
	 * 'peek_generated_primitive' returned true.
	 *
	 * \param  p  reference to primitive the data belongs to
	 *
	 * \return index of data buffer
	 *
	 * XXX remove later as this is a merely a convenience method
	 */
	Cache_Index peek_generated_data_index(Cbe::Primitive const &p);

	/**
	 * Discard given primitive
	 *
	 * \param  p  reference to primitive
	 */
	void drop_generated_primitive(Cbe::Primitive const &p);

	/**
	 * Mark the primitive as completed
	 *
	 * \param p  reference to Primitive that is used to mark
	 *           the corresponding internal primitive as completed
	 */
	void mark_completed_primitive(Cbe::Primitive const &p);
};

#endif /* _CBE_CACHE_MODULE_H_ */
