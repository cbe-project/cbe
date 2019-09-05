/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _LIBRARY_H_
#define _LIBRARY_H_

/* Genode includes */
#include <base/stdint.h>
#include <base/output.h>
#include <timer_session/connection.h>

/* CBE includes */
#include <cbe/types.h>
#include <cbe/util.h>

namespace Cbe {

	using namespace Genode;

	class Library;

} /* namespace Cbe */


class Cbe::Library
{
	public:

		struct Invalid_tree               : Genode::Exception { };
		struct Spark_object_size_mismatch : Genode::Exception { };
		struct Invalid_snapshot_slot      : Genode::Exception { };
		struct Primitive_failed           : Genode::Exception { };

		enum {
			SYNC_INTERVAL   = 0ull,
			SECURE_INTERVAL = 5ull,
		};

	private:

		bool                 _execute_progress       { false };
		Cbe::Time::Timestamp _sync_interval          { SYNC_INTERVAL };
		Cbe::Time::Timestamp _last_time              { 0 };
		Cbe::Time::Timestamp _secure_interval        { SECURE_INTERVAL };
		Cbe::Time::Timestamp _last_secure_time       { 0 };
		Timeout_request      _sync_timeout_request   { false, 0};
		Timeout_request      _secure_timeout_request { false, 0};

		/*
		 * Check if we provided enough memory for all the SPARK objects.
		 * We use the check of '_object_sizes_match' in the constructor
		 * to prevent the compiler from optimizing the call away.
		 */

		bool _check_object_sizes()
		{
			Cbe::assert_valid_object_size<Module::Cache>();
			Cbe::assert_valid_object_size<Module::Cache_flusher>();
			Cbe::assert_valid_object_size<Module::Crypto>();
			Cbe::assert_valid_object_size<Module::Request_pool>();
			Cbe::assert_valid_object_size<Module::Splitter>();
			Cbe::assert_valid_object_size<Module::Sync_superblock>();
			Cbe::assert_valid_object_size<Module::Translation>();
			Cbe::assert_valid_object_size<Virtual_block_device>();
			Cbe::assert_valid_object_size<Module::Block_io>();
			Cbe::assert_valid_object_size<Module::Write_back>();
			Cbe::assert_valid_object_size<Free_tree>();
			Cbe::assert_same_object_size<Tree_helper>();
			return true;
		}

		bool const _object_sizes_match { _check_object_sizes() };

		/*
		 * Request_pool module
		 */
		using Pool = Module::Request_pool;
		Pool _request_pool { };

		/*
		 * Splitter module
		 */
		using Splitter = Module::Splitter;
		Splitter _splitter { };

		/*
		 * Crypto module
		 */
		using Crypto = Module::Crypto;
		Crypto   _crypto        { "All your base are belong to us  " };
		Block_data _crypto_data { };

		/*
		 * I/O module
		 */
		// Block::Connection<> &_block;
		using Io       = Module::Block_io;
		using Io_data  = Module::Io_data;
		using Io_index = Module::Block_io::Index;
		Io      _io      { };
		Io_data _io_data { };

		/*
		 * Cache module
		 */
		using Cache          = Module::Cache;
		using Cache_Index    = Module::Cache_Index;
		using Cache_Data     = Module::Cache_Data;
		using Cache_Job_Data = Module::Cache_Job_Data;
		using Cache_flusher  = Module::Cache_flusher;

		Cache           _cache          { };
		Cache_Data      _cache_data     { };
		Cache_Job_Data  _cache_job_data { };
		Cache_flusher   _cache_flusher        { };

		/*
		 * Virtual-block-device module
		 */
		using Translation      = Module::Translation;
		using Translation_Data = Module::Translation_Data;

		Translation_Data _trans_data     { };
		Constructible<Cbe::Virtual_block_device> _vbd { };

		/*
		 * Write-back module
		 */
		using Write_back            = Module::Write_back;
		using Write_back_data       = Module::Write_back_data;
		using Write_back_data_index = Module::Write_back_data_index;

		Write_back      _write_back { };
		Write_back_data _write_back_data { };

		/*
		 * Sync-superblock module
		 */
		using Sync_superblock = Module::Sync_superblock;
		Sync_superblock _sync_sb { };

		/*
		 * Free-tree module
		 */
		enum { FREE_TREE_RETRY_LIMIT = 3u, };
		Constructible<Cbe::Free_tree> _free_tree { };
		uint32_t                      _free_tree_retry_count { 0 };
		Translation_Data              _free_tree_trans_data { };
		Query_data                    _free_tree_query_data { };

		/*
		 * Super-block/snapshot handling
		 */

		Cbe::Super_block       _super_block[Cbe::NUM_SUPER_BLOCKS] { };
		Cbe::Super_block_index _cur_sb { Cbe::Super_block_index::INVALID };
		Cbe::Generation  _cur_gen { 0 };
		Cbe::Generation  _last_secured_generation { 0 };
		uint32_t         _cur_snap { 0 };
		uint32_t         _last_snapshot_id { 0 };
		bool             _seal_generation { false };
		bool             _secure_superblock { false };
		bool             _superblock_dirty { false };


		static bool _discard_snapshot(Cbe::Snapshot active[Cbe::NUM_SNAPSHOTS],
		                              uint32_t      current);

		void _dump_cur_sb_info() const;

		/* debugging aid */
		Cbe::Primitive current_primitive { };

	public:

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
		Library(Time::Timestamp  const  now,
		        Time::Timestamp  const  sync,
		        Time::Timestamp  const  secure,
		        Cbe::Super_block        sbs[Cbe::NUM_SUPER_BLOCKS],
		        Cbe::Super_block_index  current_sb);

		Timeout_request peek_sync_timeout_request() const;
		Timeout_request peek_secure_timeout_request() const;

		void ack_sync_timeout_request();
		void ack_secure_timeout_request();

		/**
		 * Print current active super-block/snapshot information to LOG
		 */
		void dump_cur_sb_info() const;

		/**
		 * Get highest virtual-block-address useable by the current active snapshot
		 *
		 * \return  highest addressable virtual-block-address
		 */
		Cbe::Virtual_block_address max_vba() const;

		/**
		 * Execute one loop of the CBE
		 *
		 * \param  show_progress     if true, generate a LOG message of the current
		 *                           progress (basically shows the progress state of
		 *                           all modules)
		 * \param  show_if_progress  if true, generate LOG message only when progress was
		 *                           acutally made
		 *
		 * \return  true if progress was made, false otherwise
		 */
		void execute(Time::Timestamp now, bool show_progress, bool show_if_progress);
		bool execute_progress() const { return _execute_progress; };

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
		 * Defining the structure here is just an interims solution
		 * and should be properly managed, especially handling more
		 * than one request is "missing".
		 */
		struct Req_prim
		{
			Cbe::Request   req;
			Cbe::Primitive prim;
			Cbe::Tag       tag;

			bool in_progress;
		};

		/*
		 * Backend block I/O
		 */

		Req_prim _backend_req_prim { };

		/**
		 * Return a request for the backend block session
		 *
		 * \return valid request in case the is one pending that
		 *         needs data, otherwise an invalid one is returned
		 */
		Cbe::Request need_data();

		/**
		 * Take read request for backend block session
		 *
		 * \param  request  reference to the request from the CBE
		 * \return  true if the CBE could process the request
		 */
		bool take_read_data(Cbe::Request const &request);

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
		bool ack_read_data(Cbe::Request    const &request,
		                   Cbe::Block_data const &data);

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
		bool take_write_data(Cbe::Request    const &request,
		                     Cbe::Block_data       &data);

		/**
		 * Acknowledge write request to backend block session
		 *
		 * \param  request  reference to the Block::Request processed
		 *                  by the CBE
		 * \return  true if the CBE acknowledged the request
		 */
		bool ack_write_data(Cbe::Request const &request);

		/*
		 * Frontend block I/O
		 */

		Req_prim _frontend_req_prim { };

		/**
		 * Return a request that provides data to the frontend block data
		 *
		 * \return valid request in case the is one pending that
		 *         needs data, otherwise an invalid one is returned
		 */
		Cbe::Request have_data();

		/**
		 * Get primitive index
		 *
		 * \return 
		 */
		uint64_t give_data_index(Cbe::Request const &request);

		/**
		 * Request access to the Block::Request data for storing data
		 *
		 * \param  request  reference to the Block::Request processed
		 *                  by the CBE
		 * \param  data     reference to the data associated with the
		 *                  Block::Request
		 * \param  processable  'true' on return if the CBE could process the request
		 */
		void give_read_data(Cbe::Request const &request, Cbe::Block_data &data, bool &processable);

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
};

#endif /* _LIBRARY_H_ */
