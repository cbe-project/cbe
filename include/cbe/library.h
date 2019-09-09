/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_LIBRARY_H_
#define _CBE_LIBRARY_H_

/* Genode includes */
#include <base/stdint.h>
#include <base/output.h>

/* CBE includes */
#include <cbe/types.h>
#include <cbe/util.h>
#include <cbe/spark_object.h>


namespace Cbe {

	using namespace Genode;

	class Public_Library;

	Genode::uint32_t object_size(Public_Library const &);

} /* namespace Cbe */


struct Cbe::Public_Library : Cbe::Spark_object<216648>
{
	/**
	 * Constructor
	 *
	 * \param  time    time object used throughout the CBE to query the
	 *                 current time
	 * \param  sync    interval in ms after which the current generation
	 *                 should be sealed
	 * \param  secure  interval in ms after which the current super-block
	 *                 should be secured
	 * \param  block   reference to the Block::Connection used by the I/O
	 *                 module
	 * \param  sbs     array of all super-blocks, will be copied
	 *
	 * \param  current_sb  super-block that should be used initially
	 */
	Public_Library(Time::Timestamp   const  now,
	               Time::Timestamp   const  sync,
	               Time::Timestamp   const  secure,
	               Cbe::Super_blocks const &sbs,
	               Cbe::Super_block_index   current_sb);

	/**
	 * Print current active super-block/snapshot information to LOG
	 */
//	void dump_cur_sb_info() const;

	/**
	 * Get highest virtual-block-address useable by the current active snapshot
	 *
	 * \return  highest addressable virtual-block-address
	 */
	Cbe::Virtual_block_address max_vba() const;

	/**
	 * Execute one loop of the CBE
	 *
	 * \param  now               current time as timestamp
	 * \param  show_progress     if true, generate a LOG message of the current
	 *                           progress (basically shows the progress state of
	 *                           all modules)
	 * \param  show_if_progress  if true, generate LOG message only when progress was
	 *                           acutally made
	 */
	void execute(Time::Timestamp now);

	/**
	 * Return whether the last call to 'execute' has made progress or not
	 */
	bool execute_progress() const;

	/**
	 * Check if the CBE can accept a new requeust
	 *
	 * \return true if a request can be accepted, otherwise false
	 */
	bool request_acceptable() const;

	/**
	 * Submit a new request
	 *
	 * This method must only be called after executing 'request_acceptable'
	 * returned true.
	 *
	 * \param request  block request
	 */
	void submit_request(Cbe::Request const &request);

	/**
	 * Check for any completed request
	 *
	 * \return a valid block request will be returned if there is an
	 *         completed request, otherwise an invalid one
	 */
	Cbe::Request peek_completed_request() const;

	/**
	 * Drops the completed request
	 *
	 * This method must only be called after executing
	 * 'peek_completed_request' returned a valid request.
	 *
	 */
	void drop_completed_request(Cbe::Request const &req);

	/*
	 * Backend block I/O
	 */

	/**
	 * Return a request for the backend block session
	 *
	 * \return valid request in case the is one pending that
	 *         needs data, otherwise an invalid one is returned
	 */
	void need_data(Cbe::Request &result);

	/**
	 * Take read request for backend block session
	 *
	 * \param  request  reference to the request from the CBE
	 * \return  true if the CBE could process the request
	 */
	void take_read_data(Cbe::Request const &request, bool &result);

	/**
	 * Acknowledge read request to the backend block session
	 *
	 * The given data will be transfered to the CBE.
	 *
	 * \param  request  reference to the request from the CBE
	 * \param  data     reference to the data associated with the
	 *                  request
	 *
	 * \return  true if the CBE acknowledged the request
	 */
	void ack_read_data(Cbe::Request    const &request,
	                   Cbe::Block_data const &data,
	                   bool                  &result);

	/**
	 * Take write request for the backend block session
	 *
	 * The CBE will transfer the payload to the given data.
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Request
	 *
	 * \return  true if the CBE could process the request
	 */
	void take_write_data(Cbe::Request    const &request,
	                     Cbe::Block_data       &data,
	                     bool                  &result);

	/**
	 * Acknowledge write request to backend block session
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \return  true if the CBE acknowledged the request
	 */
	void ack_write_data(Cbe::Request const &request,
	                    bool               &result);

	/*
	 * Frontend block I/O
	 */

	/**
	 * Return a request that provides data to the frontend block data
	 *
	 * \return valid request in case the is one pending that
	 *         needs data, otherwise an invalid one is returned
	 */
	void have_data(Cbe::Request &result);

	/**
	 * Return primitive index
	 */
	uint64_t give_data_index(Cbe::Request const &request) const;

	/**
	 * Request access to the Block::Request data for storing data
	 *
	 * \param  request      reference to the Block::Request processed
	 *                      by the CBE
	 * \param  data         reference to the data associated with the
	 *                      Block::Request
	 * \param  processable  'true' on return if the CBE could process the request
	 */
	void give_read_data(Cbe::Request const &request,
	                    Cbe::Block_data    &data,
	                    bool               &processable);

	/**
	 * Request access to the Block::Request data for reading data
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Block::Request
	 *
	 * \return  true if the CBE could process the request
	 */
	bool give_write_data(Time::Timestamp const now,
	                     Cbe::Request    const &request,
	                     Cbe::Block_data const &data);

	/*
	 * Return a timeout request with the objective of synchronization
	 * generated by the CBE during a previous operation. If the returned
	 * timeout request is invalid, then no timeout request is pending.
	 */
	Timeout_request peek_sync_timeout_request() const;

	/**
	 * Return a timeout request with the objective of securing
	 * generated by the CBE during a previous operation. If the returned
	 * timeout request is invalid, then no timeout request is pending.
	 */
	Timeout_request peek_secure_timeout_request() const;

	/**
	 * Ackowledge that a synchronization timeout has been set according
	 * to the previously peek'd timeout request of the CBE
	 */
	void ack_sync_timeout_request();

	/**
	 * Ackowledge that a securing timeout has been set according
	 * to the previously peek'd timeout request of the CBE
	 */
	void ack_secure_timeout_request();
};

#endif /* _CBE_LIBRARY_H_ */
