/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

/* Genode includes */
#include <base/allocator_avl.h>
#include <base/attached_ram_dataspace.h>
#include <base/attached_rom_dataspace.h>
#include <base/component.h>
#include <base/heap.h>
#include <block/request_stream.h>
#include <root/root.h>
#include <timer_session/connection.h>
#include <util/bit_allocator.h>

#include <terminal_session/connection.h>

/* repo includes */
#include <util/sha256_4k.h>

/* cbe includes */
#include <cbe/types.h>
#include <cbe/util.h>

/* local includes */
#include <util.h>


namespace Cbe {

	struct Time;
	struct Library;

	struct Block_session_component;
	struct Main;

} /* namespace Cbe */


struct Cbe::Block_session_component : Rpc_object<Block::Session>,
                                      Block::Request_stream
{
	Entrypoint &_ep;

	Block_session_component(Region_map               &rm,
	                        Dataspace_capability      ds,
	                        Entrypoint               &ep,
	                        Signal_context_capability sigh,
	                        Block::sector_t           block_count)
	:
		Request_stream(rm, ds, ep, sigh,
		               Info { .block_size  = Cbe::BLOCK_SIZE,
		                      .block_count = block_count,
		                      .align_log2  = Genode::log2((Block::sector_t)Cbe::BLOCK_SIZE),
		                      .writeable    = true }),
		_ep(ep)
	{
		_ep.manage(*this);
	}

	~Block_session_component() { _ep.dissolve(*this); }

	Info info() const override { return Request_stream::info(); }

	Capability<Tx> tx_cap() override { return Request_stream::tx_cap(); }
};


struct Cbe::Time
{
	Timer::Connection _timer;

	using Timestamp = Genode::uint64_t;

	/*
	 * Synchronization timeout handling
	 */

	Timer::One_shot_timeout<Time> _sync_timeout {
		_timer, *this, &Time::_handle_sync_timeout };

	void _handle_sync_timeout(Genode::Duration)
	{
		if (_sync_sig_cap.valid()) {
			Genode::Signal_transmitter(_sync_sig_cap).submit();
		}
	}

	Genode::Signal_context_capability _sync_sig_cap { };

	/*
	 * Securing timeout handling
	 */

	Timer::One_shot_timeout<Time> _secure_timeout {
		_timer, *this, &Time::_handle_secure_timeout };

	void _handle_secure_timeout(Genode::Duration)
	{
		if (_secure_sig_cap.valid()) {
			Genode::Signal_transmitter(_secure_sig_cap).submit();
		}
	}

	Genode::Signal_context_capability _secure_sig_cap { };

	Time(Genode::Env &env)
	: _timer(env) { }

	Timestamp timestamp()
	{
		return _timer.curr_time().trunc_to_plain_ms().value;
	}

	void sync_sigh(Genode::Signal_context_capability cap)
	{
		_sync_sig_cap = cap;
	}

	void schedule_sync_timeout(uint64_t msec)
	{
		_sync_timeout.schedule(Genode::Microseconds { msec * 1000 });
	}

	void secure_sigh(Genode::Signal_context_capability cap)
	{
		_secure_sig_cap = cap;
	}

	void schedule_secure_timeout(uint64_t msec)
	{
		_secure_timeout.schedule(Genode::Microseconds { msec * 1000 });
	}
};


/******************
 ** debug macros **
 ******************/

#define DEBUG 1
#if defined(DEBUG) && DEBUG > 0

static inline Genode::uint64_t __rdtsc__()
{
	Genode::uint32_t lo, hi;
	/* serialize first */
	asm volatile ("xorl %%eax,%%eax\n\tcpuid\n\t" ::: "%rax", "%rbx", "%rcx", "%rdx");
	asm volatile ("rdtsc" : "=a" (lo), "=d" (hi));
	return (Genode::uint64_t)hi << 32 | lo;
}


static inline Genode::uint64_t __timestamp__()
{
	static Genode::uint64_t initial_ts = __rdtsc__();
	return __rdtsc__() - initial_ts;
}


#define MOD_ERR(...) \
	do { \
		Genode::error(MOD_NAME " ", __timestamp__(), "> ", \
		              __func__, ":", __LINE__, ": ", __VA_ARGS__); \
	} while (0)


#define MOD_DBG(...) \
	do { \
		Genode::log("\033[36m" MOD_NAME " ", __timestamp__(), "> ", \
		            __func__, ":", __LINE__, ": ", __VA_ARGS__); \
	} while (0)


#define DBG_NAME "ML"
#define DBG(...) \
	do { \
		Genode::log("\033[35m" DBG_NAME " ", __timestamp__(), "> ", \
		            __func__, ":", __LINE__, ": ", __VA_ARGS__); \
	} while (0)
#else
#define MOD_ERR(...)
#define MOD_DBG(...)
#define DBG(...)
#endif

#define LOG_PROGRESS(v) \
	do { \
		if (show_progress || (show_if_progress && v)) { \
			Genode::log(#v, ": ", v); \
		} \
	} while (0)


/*************
 ** modules **
 *************/

/* SPARK modules */
#include <cache_module.h>
#include <cache_flusher_module.h>
#include <crypto_module.h>
#include <request_pool_module.h>
#include <splitter_module.h>
#include <sync_superblock_module.h>

/* C++ modules */
#include <translation_module.h>
#include <write_back_module.h>
#include <io_module.h>
#include "free_tree.h"
#include "virtual_block_device.h"


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

		Time &_time;
		Time::Timestamp _sync_interval    { SYNC_INTERVAL };
		Time::Timestamp _last_time        { _time.timestamp() };
		Time::Timestamp _secure_interval  { SECURE_INTERVAL };
		Time::Timestamp _last_secure_time { _time.timestamp() };

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
		Block::Connection<> &_block;
		using Io       = Module::Block_io;
		using Io_data  = Module::Io_data;
		using Io_index = Module::Block_io::Index;
		Io      _io      { _block };
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
		using Write_back            = Module::Write_back<Cbe::Type_i_node>;
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


		bool _discard_snapshot(Cbe::Snapshot active[Cbe::NUM_SNAPSHOTS],
		                       uint32_t      current)
		{
			uint32_t lowest_id = Cbe::Snapshot::INVALID_ID;
			for (uint32_t i = 0; i < Cbe::NUM_SNAPSHOTS; i++) {
				Cbe::Snapshot const &snap = active[i];
				if (!snap.valid())      { continue; }
				if (snap.keep())        { continue; }
				if (snap.id == current) { continue; }

				lowest_id = Genode::min(lowest_id, snap.id);
			}

			if (lowest_id == Cbe::Snapshot::INVALID_ID) {
				return false;
			}

			Cbe::Snapshot &snap = active[lowest_id];
			DBG("discard snapshot: ", snap);
			snap.discard();
			return true;
		}

		void _dump_cur_sb_info() const
		{
			Cbe::Super_block const &sb = _super_block[_cur_sb.value];
			Cbe::Snapshot    const &snap = sb.snapshots[_cur_snap];

			Cbe::Physical_block_address const root_number = snap.pba;
			Cbe::Height                 const height      = snap.height;
			Cbe::Number_of_leaves       const leaves      = snap.leaves;

			Cbe::Degree                 const degree      = sb.degree;
			Cbe::Physical_block_address const free_number = sb.free_number;
			Cbe::Number_of_leaves       const free_leaves = sb.free_leaves;
			Cbe::Height                 const free_height = sb.free_height;

			Genode::log("Virtual block-device info in SB[", _cur_sb, "]: ",
			            " SNAP[", _cur_snap, "]: ",
			            "tree height: ", height, " ",
			            "edges per node: ", degree, " ",
			            "leaves: ", leaves, " ",
			            "root block address: ", root_number, " ",
			            "free block address: ", free_number, " ",
			            "free leaves: (", free_leaves, "/", free_height, ")"
			);
		}


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
		Library(Cbe::Time              &time,
		        Time::Timestamp  const  sync,
		        Time::Timestamp  const  secure,
		        Block::Connection<>    &block,
		        Cbe::Super_block        sbs[Cbe::NUM_SUPER_BLOCKS],
		        Cbe::Super_block_index  current_sb)
		:
			_time(time), _sync_interval(sync), _secure_interval(secure),
			_block(block)
		{
			/*
			 * We have to make sure we actually execute the code to check
			 * if we provide enough space for the SPARK objects.
			 */
			if (!_object_sizes_match) {
				error("object size mismatch");
				throw Spark_object_size_mismatch();
			}

			/*
			 * Copy initial state of all super-blocks. During the life-time
			 * of the CBE library these blocks will only be (over-)written
			 * and nevert read again.
			 *
			 *
			 * (The idea is to keep the setup phase seperated from the actual
			 *  CBE work during run-time - not sure if this is necessary.)
			 */
			for (uint32_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				Genode::memcpy(&_super_block[i], &sbs[i], sizeof (Cbe::Super_block));
			}

			/*
			 * Now we look up the proper snapshot from the current super-block
			 * and fill in our internal meta-data.
			 */

			_cur_sb = current_sb;

			using SB = Cbe::Super_block;
			using SS = Cbe::Snapshot;

			SB const &sb = _super_block[_cur_sb.value];

			uint32_t snap_slot = sb.snapshot_slot();
			if (snap_slot == Super_block::INVALID_SNAPSHOT_SLOT) {
				Genode::error("snapshot slot not found");
				throw Invalid_snapshot_slot();
			}

			_cur_snap = snap_slot;

			SS const &snap = sb.snapshots[_cur_snap];

			Cbe::Degree           const degree = sb.degree;
			Cbe::Height           const height = snap.height;
			Cbe::Number_of_leaves const leaves = snap.leaves;

			/*
			 * The current implementation is limited with regard to the
			 * tree topology. Make sure it fits.
			 */
			if (height > Cbe::TREE_MAX_HEIGHT || height < Cbe::TREE_MIN_HEIGHT) {
				Genode::error("tree height of ", height, " not supported");
				throw Invalid_tree();
			}

			if (degree < Cbe::TREE_MIN_DEGREE) {
				Genode::error("tree outer-degree of ", degree, " not supported");
				throw Invalid_tree();
			}

			/*
			 * The VBD class is currently nothing more than a glorified
			 * Translation meta-module - pass on all information that is
			 * needed to construct the Tree_helper.
			 *
			 *
			 * (Having the VBD is somewhat artificial, using the Translation
			 *  module directly works well. The idea was to later on move
			 *  all module which are needed to deal with a r/o snapshort in
			 *  there and have an extended versions that is also able to
			 *  manage the current active working snapshot.)
			 */
			_vbd.construct(height, degree, leaves);

			Cbe::Physical_block_address const free_number = sb.free_number;
			Cbe::Generation             const free_gen    = sb.free_gen;
			Cbe::Hash                   const free_hash   = sb.free_hash;
			Cbe::Height                 const free_height = sb.free_height;
			Cbe::Degree                 const free_degree = sb.free_degree;
			Cbe::Number_of_leaves       const free_leafs  = sb.free_leaves;

			/*
			 * The FT encapsulates all modules needed for traversing the
			 * free-tree and allocating new blocks. For now we do not update
			 * the FT itself, i.e, only the leaf node entries are changed.
			 *
			 *
			 * (Later, when the FT itself is updating its inner-nodes in a CoW
			 *  fashion, we will store the root and hash for a given generation.
			 *  That means every super-block will probably have its own FT.
			 *  After all the FT includes all nodes used by the list of active
			 *  snapshots.)
			 */
			_free_tree.construct(free_number, free_gen, free_hash, free_height,
			                     free_degree, free_leafs);

			/*
			 * The current version always is the last secured version incremented
			 * by one.
			 */
			_last_secured_generation = sb.last_secured_generation;
			_cur_gen                = _last_secured_generation + 1;
			_last_snapshot_id        = snap.id;

			/*
			 * If the timeout intervals were configured set initial timeout.
			 *
			 *
			 * (It stands to reasons if we should initial or rather only set
			 *  them when a write request was submitted.)
			 */
			if (_sync_interval)   { _time.schedule_sync_timeout(_sync_interval); }
			if (_secure_interval) { _time.schedule_secure_timeout(_secure_interval); }

			/* for diagnostic reasons */
			_dump_cur_sb_info();
		}

		/**
		 * Print current active super-block/snapshot information to LOG
		 */
		void dump_cur_sb_info() const { _dump_cur_sb_info(); }

		/**
		 * Get highest virtual-block-address useable by the current active snapshot
		 *
		 * \return  highest addressable virtual-block-address
		 */
		Cbe::Virtual_block_address max_vba() const
		{
			return _super_block[_cur_sb.value].snapshots[_cur_snap].leaves - 1;
		}

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
		bool execute(bool show_progress, bool show_if_progress)
		{
			bool progress = false;

			/*******************
			 ** Time handling **
			 *******************
			 *
			 * Query current time and check if a timeout has triggered
			 */

			/*
			 * Seal the current generation if sealing is not already
			 * in progress. In case no write operation was performed just set
			 * the trigger for the next interval.
			 *
			 *
			 * (Instead of checking all cache entries it would be nice if the
			 *  Cache module would provide an interface that would allow us to
			 *  simple check if it contains any dirty entries as it could easily
			 *  track that condition internally itself.)
			 */
			Cbe::Time::Timestamp const diff_time = _time.timestamp() - _last_time;
			if (diff_time >= _sync_interval && !_seal_generation) {

				bool cache_dirty = false;
				for (size_t i = 0; i < _cache.cxx_cache_slots(); i++) {
					Cache_Index const idx = Cache_Index { .value = (uint32_t)i };
					if (_cache.dirty(idx)) {
						cache_dirty |= true;
						break;
					}
				}

				if (cache_dirty) {
					Genode::log("\033[93;44m", __func__, " SEAL current generation: ", _cur_gen);
					_seal_generation = true;
				} else {
					DBG("cache is not dirty, re-arm trigger");
					_last_time = _time.timestamp();
					_time.schedule_sync_timeout(_sync_interval);
				}
			}

			/*
			 * Secure the current super-block if securing is not already
			 * in progress. In case no write operation was performed, i.e., no
			 * snapshot was changed, just set the trigger for the next interval.
			 *
			 *
			 * (_superblock_dirty is set whenver the Write_back module has done its work
			 *  and will be reset when the super-block was secured.)
			 */
			Cbe::Time::Timestamp const diff_secure_time = _time.timestamp() - _last_secure_time;
			if (diff_secure_time >= _secure_interval && !_secure_superblock) {

				if (_superblock_dirty) {
					Genode::log("\033[93;44m", __func__,
					            " SEAL current super-block: ", _cur_sb);
					_secure_superblock = true;
				} else {
					DBG("no snapshots created, re-arm trigger");
					_last_secure_time = _time.timestamp();
				}
			}

			/************************
			 ** Free-tree handling **
			 ************************/

			/*
			 * The FT meta-module uses the Translation module internally and
			 * needs access to the cache since it wants to use its data.
			 * Because accessing a cache entry will update its LRU value, the
			 * cache must be mutable (that is also the reason we need the
			 * time object).
			 *
			 * Since it might need to reuse reserved blocks, we have to hand
			 * over all active snapshots as well as the last secured generation.
			 * Both are needed for doing the reuse check.
			 *
			 *
			 * (Rather than passing the cache module itself to the FT it might
			 *  be better to use a different interface for that purpose as I
			 *  do not know how well the current solution works with SPARK...)
			 */
			{
				Cbe::Super_block const &sb = _super_block[_cur_sb.value];

				bool const ft_progress = _free_tree->execute(sb.snapshots,
				                                             _last_secured_generation,
				                                             _free_tree_trans_data,
				                                             _cache, _cache_data,
				                                             _free_tree_query_data,
				                                             _time);
				progress |= ft_progress;
				LOG_PROGRESS(ft_progress);
			}

			/*
			 * A complete primitive was either successful or has failed.
			 *
			 * In the former case we will instruct the Write_back module to
			 * write all changed nodes of the VBD back to the block device
			 * and eventually will leadt to ACKing the block request.
			 *
			 * In the later case we will attempt to free reserved blocks in
			 * the FT by discarding snapshots. Briefly speaking all snapshots
			 * that were not specifically marked (see FLAG_KEEP) will be
			 * discarded. A finit number of retries will be performed. If we
			 * cannot free enough blocks, the write operation is marked as
			 * failed and will result in an I/O error at the Block session.
			 *
			 */
			while (true) {

				Cbe::Primitive prim = _free_tree->peek_completed_primitive();
				if (!prim.valid()) { break; }

				if (prim.success == Cbe::Primitive::Success::FALSE) {

					DBG("allocating new blocks failed: ", _free_tree_retry_count);
					if (_free_tree_retry_count < FREE_TREE_RETRY_LIMIT) {

						Cbe::Super_block &sb = _super_block[_cur_sb.value];
						uint32_t const current = sb.snapshots[_cur_snap].id;
						if (_discard_snapshot(sb.snapshots, current)) {
							_free_tree_retry_count++;
							/*
 							 * Instructing the FT to retry the allocation will
							 * lead to clearing its internal 'query branches'
							 * state and executing the previously submitted
							 * request again.
							 *
							 * (This retry attempt is a shortcut as we do not have
							 *  all information available at this point to call
							 *  'submit_request' again - so we must not call
							 *  'drop_completed_primitive' as this will clear the
							 *  request.)
							 */
							_free_tree->retry_allocation();
						}
						break;
					}

					Genode::error("could not find enough useable blocks");
					_request_pool.mark_completed_primitive(prim);
					// XXX
					_vbd->trans_resume_translation();
				} else {

					if (!_write_back.primitive_acceptable()) { break; }

					if (_free_tree_retry_count) {
						DBG("reset retry count: ", _free_tree_retry_count);
						_free_tree_retry_count = 0;
					}

					/*
					 * Accessing the write-back data in this manner is still a shortcut
					 * and probably will not work with SPARK - we have to get rid of
					 * the 'block_data' pointer.
					 */
					Free_tree::Write_back_data const &wb = _free_tree->peek_completed_wb_data(prim);

					_write_back.submit_primitive(wb.prim, wb.gen, wb.vba,
					                             wb.new_pba, wb.old_pba, wb.tree_height,
					                             *wb.block_data, _write_back_data);
				}
				_free_tree->drop_completed_primitive(prim);
				progress |= true;
			}

			/*
			 * There are two types of generated primitives by FT module,
			 * the traversing of the tree is done by the internal Translation
			 * module, which will access the nodes through the cache - I/O
			 * primitives will therefor be generated as a side-effect of the
			 * querying attempt by the Cache module.
			 *
			 * - IO_TAG primitives are only used for querying type 2 nodes, i.e.,
			 *   inner nodes of the free-tree containg free or reserved blocks.
			 *
			 * - WRITE_BACK_TAG primitve are only used for writing one changed
			 *   branch back to the block device. Having the branch written
			 *   will lead to a complete primitve.
			 */
			while (true) {

				Cbe::Primitive prim = _free_tree->peek_generated_primitive();
				if (!prim.valid()) { break; }
				if (!_io.primitive_acceptable()) { break; }

				Index const idx = _free_tree->peek_generated_data_index(prim);

				Cbe::Block_data *data = nullptr;
				Cbe::Tag tag { Tag::INVALID_TAG };
				switch (prim.tag) {
				case Tag::WRITE_BACK_TAG:
					tag  = Tag::FREE_TREE_TAG_WB;
					/*
					 * XXX Accessing the cache in this way could be dangerous because
					 * the cache is shared by the VBD as well as the FT. If we would
					 * not suspend the VBD while doing the write-back, another request
					 * could evict the entry belonging to the idx value and replace it.
					 *
					 * (Since the prim contains the PBA we could check the validity of
					 *  the index beforehand - but not storing the index in the first
					 *  place would be the preferable solution.)
					 */
					data = &_cache_data.item[idx.value];
					break;
				case Tag::IO_TAG:
					tag  = Tag::FREE_TREE_TAG_IO;
					data = &_free_tree_query_data.item[idx.value];
					break;
				default: break;
				}

				_io.submit_primitive(tag, prim, _io_data, *data);

				_free_tree->drop_generated_primitive(prim);
				progress |= true;
			}

			/*******************************
			 ** Put request into splitter **
			 *******************************/

			/*
			 * An arbitrary sized Block request will be cut into 4096 byte
			 * sized primitves by the Splitter module.
			 */
			while (true) {

				Cbe::Request const &req = _request_pool.peek_pending_request();
				if (!req.valid()) { break; }
				if (!_splitter.request_acceptable()) { break; }

				_request_pool.drop_pending_request(req);
				_splitter.submit_request(req);

				progress |= true;
			}

			/*
			 * Give primitive to the translation module
			 */
			while (true) {

				Cbe::Primitive prim = _splitter.peek_generated_primitive();
				if (!prim.valid()) { break; }
				if (!_vbd->primitive_acceptable()) { break; }

				// XXX why is _seal_generation check not necessary?
				/* that mainly is intended to block write primitives */
				if (_secure_superblock) {
					DBG("prevent processing new primitives while securing super-block");
					break;
				}

				_splitter.drop_generated_primitive(prim);

				/*
				 * For every new request, we have to use the currently active
				 * snapshot as a previous request may have changed the tree.
				 */
				Cbe::Super_block const &sb = _super_block[_cur_sb.value];
				Cbe::Snapshot    const &snap = sb.snapshots[_cur_snap];

				Cbe::Physical_block_address const  pba  = snap.pba;
				Cbe::Hash                   const &hash = snap.hash;
				Cbe::Generation             const  gen  = snap.gen;

				_vbd->submit_primitive(pba, gen, hash, prim);
				progress |= true;
			}

			/******************
			 ** VBD handling **
			 ******************/

			/*
			 * The VBD meta-module uses the Translation module internally and
			 * needs access to the cache since it wants to use its data.
			 * Because accessing a cache entry will update its LRU value, the
			 * cache must be mutable (that is also the reason we need the
			 * time object).
			 *
			 *
			 * (Basically the same issue regarding SPARK as the FT module...)
			 */
			{
				bool const vbd_progress = _vbd->execute(_trans_data,
				                                        _cache, _cache_data,
				                                        _time);
				progress |= vbd_progress;
				LOG_PROGRESS(vbd_progress);
			}

			/****************************
			 ** Cache_flusher handling **
			 ****************************
			 *
			 * The Cache_flusher module is used to flush all dirty cache entries
			 * to the block device and mark them as clean again. While the flusher
			 * is doing its work, all cache entries should be locked, i.e., do not
			 * cache an entry while its flushed - otherwise the change might not
			 * end up on the block device. Should be guarded by '_seal_generation'.
			 *
			 * (For better or worse it is just a glorified I/O manager. At some
			 *  point it should be better merged into the Cache module later on.)
			 */

			/*
			 * Mark the corresponding cache entry as clean. If it was
			 * evicted in the meantime it will be ignored.
			 */
			while (true) {

				Cbe::Primitive prim = _cache_flusher.cxx_peek_completed_primitive();
				if (!prim.valid()) { break; }

				if (prim.success != Cbe::Primitive::Success::TRUE) {
					DBG(prim);
					throw Primitive_failed();
				}

				Cbe::Physical_block_address const pba = prim.block_number;
				_cache.mark_clean(pba);
				DBG("mark_clean: ", pba);

				_cache_flusher.cxx_drop_completed_primitive(prim);
				progress |= true;
			}

			/*
			 * Just pass the primitive on to the I/O module.
			 */
			while (true) {

				Cbe::Primitive prim = _cache_flusher.cxx_peek_generated_primitive();
				if (!prim.valid()) { break; }
				if (!_io.primitive_acceptable()) { break; }

				Cache_Index     const  idx  = _cache_flusher.cxx_peek_generated_data_index(prim);
				Cbe::Block_data       &data = _cache_data.item[idx.value];

				_io.submit_primitive(Tag::CACHE_FLUSH_TAG, prim, _io_data, data);

				_cache_flusher.cxx_drop_generated_primitive(prim);
				progress |= true;
			}

			/*************************
			 ** Write-back handling **
			 *************************
			 *
			 * The Write_back module will store a changed branch including its leaf
			 * node on the block device.
			 *
			 * The way it currently operates is as follows:
			 *    1. (CRYPTO)   it hands the leaf data to the Crypto module for encryption
			 *    2. (IO)       it hands the encrypted leaf data to I/O module to write it
			 *                  to the block device
			 *    3. (CACHE)    starting by the lowest inner node it will update the node
			 *                  entry (pba and hash)
			 *    4. (COMPLETE) it returns the new root pba and root hash
			 *
			 * When '_seal_generation' is set, it will first instruct the Cache_flusher
			 * module to clean the cache. Afterwards it will store the current snapshot
			 * and increment the '_cur_snap' as well as '_cur_gen' (-> there is only one
			 * snapshot per generation and there are currently only 48 snapshot slots per
			 * super-block) and set the sync trigger.
			 *
			 * Otherwise it will just update the root hash in place.
			 */

			while (true) {

				Cbe::Primitive prim = _write_back.peek_completed_primitive();
				if (!prim.valid()) { break; }

				if (prim.success != Cbe::Primitive::Success::TRUE) {
					DBG(prim);
					throw Primitive_failed();
				}

				if (_seal_generation) {

					// XXX only check flusher when the cache is dirty
					// XXX and track if flusher is already active, e.g. by adding
					//     a 'active' function that returns true whenever is doing
					//     its job. I fear it currently only works by chance
					if (!_cache_flusher.cxx_request_acceptable()) { break; }

					bool cache_dirty = false;
					for (uint32_t i = 0; i < _cache.cxx_cache_slots(); i++) {
						Cache_Index const idx = Cache_Index { .value = (uint32_t)i };
						if (_cache.dirty(idx)) {
							cache_dirty |= true;

							Cbe::Physical_block_address const pba = _cache.flush(idx);
							DBG(" i: ", idx.value, " pba: ", pba, " needs flushing");

							_cache_flusher.cxx_submit_request(pba, idx);
						}
					}

					/*
					 * In case we have to flush the cache, wait until we have finished
					 * doing that.
					 */
					if (cache_dirty) {
						DBG("CACHE FLUSH NEEDED: progress: ", progress);
						progress |= true;
						break;
					}

					/*
					 * Look for a new snapshot slot. If we cannot find one
					 * we manual intervention b/c there are too many snapshots
					 * flagged as keep
					 */
					Cbe::Super_block &sb = _super_block[_cur_sb.value];
					uint32_t next_snap = _cur_snap;
					for (uint32_t i = 0; i < Cbe::NUM_SNAPSHOTS; i++) {
						next_snap = (next_snap + 1) % Cbe::NUM_SNAPSHOTS;
						Cbe::Snapshot const &snap = sb.snapshots[next_snap];
						if (!snap.valid()) {
							break;
						} else {
							if (!(snap.flags & Cbe::Snapshot::FLAG_KEEP)) {
								break;
							}
						}
					}

					if (next_snap == _cur_snap) {
						Genode::error("could not find free snapshot slot");
						/* proper handling pending */
						throw Invalid_snapshot_slot();
						break;
					}

					/*
					 * Creating a new snapshot only involves storing its
					 * meta-data in a new slot and afterwards setting the
					 * seal timeout again.
					 */
					Cbe::Snapshot &snap = sb.snapshots[next_snap];

					snap.pba = _write_back.peek_completed_root(prim);
					Cbe::Hash *snap_hash = &snap.hash;
					_write_back.peek_competed_root_hash(prim, *snap_hash);

					Cbe::Tree_helper const &tree = _vbd->tree_helper();
					snap.height = tree.height();
					snap.leaves = tree.leafs();
					snap.gen = _cur_gen;
					snap.id  = ++_last_snapshot_id;

					DBG("new snapshot for generation: ", _cur_gen, " snap: ", snap);

					_cur_gen++;
					_cur_snap++;

					_seal_generation = false;
					/*
					 * (As already briefly mentioned in the time handling section,
					 *  it would be more reasonable to only set the timeouts when
					 *  we actually perform write request.)
					 */
					_last_time = _time.timestamp();
					_time.schedule_sync_timeout(_sync_interval);
				} else {

					/*
					 * No need to create a new snapshot, just update the hash in place
					 * and move on.
					 */

					Cbe::Super_block &sb = _super_block[_cur_sb.value];
					Cbe::Snapshot    &snap = sb.snapshots[_cur_snap];

					/* update snapshot */
					Cbe::Physical_block_address const pba = _write_back.peek_completed_root(prim);
					// XXX why do we need that again?
					if (snap.pba != pba) {
						snap.gen = _cur_gen;
						snap.pba = pba;
					}

					Cbe::Hash *snap_hash = &snap.hash;
					_write_back.peek_competed_root_hash(prim, *snap_hash);
				}

				/*
				 * We touched the super-block, either by updating a snapshot or by
				 * creating a new one - make sure it gets secured within the next
				 * interval.
				 */
				_superblock_dirty |= true;

				_write_back.drop_completed_primitive(prim);

				/*
				 * Since the write request is finally finished, all nodes stored
				 * at some place "save" (leafs on the block device, inner nodes within
				 * the cache, acknowledge the primitive.
				 */
				_request_pool.mark_completed_primitive(prim);
				progress |= true;

				/*
				 * XXX stalling translation as long as the write-back takes places
				 *     is not a good idea
				 */
				_vbd->trans_resume_translation();
			}


			/*
			 * Give the leaf data to the Crypto module.
			 */
			while (true) {

				Cbe::Primitive prim = _write_back.peek_generated_crypto_primitive();
				if (!prim.valid()) { break; }
				if (!_crypto.cxx_primitive_acceptable()) { break; }

				Write_back_data_index const idx = _write_back.peek_generated_crypto_data(prim);
				Cbe::Block_data &data = _write_back_data.item[idx.value];
				/* the data will be copied into the Crypto module's internal buffer */
				_crypto.cxx_submit_primitive(prim, data, _crypto_data);

				_write_back.drop_generated_crypto_primitive(prim);
				progress |= true;
			}

			/*
			 * Pass the encrypted leaf data to the I/O module.
			 */
			while (true) {

				Cbe::Primitive prim = _write_back.peek_generated_io_primitive();
				if (!prim.valid()) { break; }
				if (!_io.primitive_acceptable()) { break; }

				Write_back_data_index const idx = _write_back.peek_generated_io_data(prim);
				Cbe::Block_data           &data = _write_back_data.item[idx.value];
				_io.submit_primitive(Tag::WRITE_BACK_TAG, prim, _io_data, data);

				_write_back.drop_generated_io_primitive(prim);
				progress |= true;
			}

			/*
			 * Update the inner nodes of the tree. This is always done after the
			 * encrypted leaf node was stored by the I/O module.
			 */
			while (true) {

				Cbe::Primitive prim = _write_back.peek_generated_cache_primitive();
				DBG(prim);
				if (!prim.valid()) { break; }

				using PBA = Cbe::Physical_block_address;
				PBA const pba        = prim.block_number;
				PBA const update_pba = _write_back.peek_generated_cache_update_pba(prim);

				/*
				 * Check if the cache contains the needed entries. In case of the
				 * of the old node's block that is most likely. The new one, if
				 * there is one (that happens when the inner nodes are _not_ updated
				 * in place, might not be in the cache - check and request both.
				 */

				bool cache_miss = false;
				if (!_cache.data_available(pba)) {
					DBG("cache miss pba: ", pba);
					if (_cache.cxx_request_acceptable(pba)) {
						_cache.cxx_submit_request(pba);
					}
					cache_miss |= true;
				}

				if (pba != update_pba) {
					if (!_cache.data_available(update_pba)) {
						DBG("cache miss update_pba: ", update_pba);
						if (_cache.cxx_request_acceptable(update_pba)) {
							_cache.cxx_submit_request(update_pba);
						}
						cache_miss |= true;
					}
				}

				/* read the needed blocks first */
				if (cache_miss) {
					DBG("cache_miss");
					break;
				}

				_write_back.drop_generated_cache_primitive(prim);

				DBG("cache hot pba: ", pba, " update_pba: ", update_pba);

				/*
				 * To keep it simply, always set both properly - even if
				 * the old and new node are the same.
				 */
				Cache_Index const idx        = _cache.data_index(pba, _time.timestamp());
				Cache_Index const update_idx = _cache.data_index(update_pba, _time.timestamp());

				Cbe::Block_data const &data        = _cache_data.item[idx.value];
				Cbe::Block_data       &update_data = _cache_data.item[update_idx.value];

				_write_back.update(pba, _vbd->tree_helper(), data, update_data);
				/* make the potentially new entry as dirty so it gets flushed next time */
				_cache.mark_dirty(update_pba);
				progress |= true;
			}

			/**************************
			 ** Super-block handling **
			 **************************/

			/*
			 * Store the current generation and snapshot id in the current
			 * super-block before it gets secured.
			 */
			if (_secure_superblock && _sync_sb.cxx_request_acceptable()) {

				Cbe::Super_block &sb = _super_block[_cur_sb.value];

				sb.last_secured_generation = _cur_gen;
				sb.snapshot_id             = _cur_snap;

				DBG("secure current super-block gen: ", _cur_gen,
				    " snap_id: ", _cur_snap);

				_sync_sb.cxx_submit_request(_cur_sb.value, _cur_gen);
			}

			/*
			 * When the current super-block was secured, select the next one.
			 */
			while (true) {

				Cbe::Primitive prim = _sync_sb.cxx_peek_completed_primitive();
				if (!prim.valid()) { break; }

				if (prim.success != Cbe::Primitive::Success::TRUE) {
					DBG(prim);
					throw Primitive_failed();
				}

				DBG("primitive: ", prim);


				Cbe::Super_block_index  next_sb = Cbe::Super_block_index {
					.value = (_cur_sb.value + 1) % Cbe::NUM_SUPER_BLOCKS
				};
				Cbe::Super_block       &next    = _super_block[next_sb.value];
				Cbe::Super_block const &curr    = _super_block[_cur_sb.value];
				Genode::memcpy(&next, &curr, sizeof (Cbe::Super_block));

				/* handle state */
				_cur_sb                  = next_sb;
				_last_secured_generation = _sync_sb.cxx_peek_completed_generation(prim);
				_superblock_dirty        = false;
				_secure_superblock       = false;

				_sync_sb.cxx_drop_completed_primitive(prim);
				progress |= true;

				/*
				 * (XXX same was with sealing the generation, it might make
				 *  sense to set the trigger only when a write operation
				 *  was performed.)
				 */
				_last_secure_time = _time.timestamp();
				_time.schedule_secure_timeout(_secure_interval);
			}

			/*
			 * Use I/O module to write super-block to the block device.
			 */
			while (true) {

				Cbe::Primitive prim = _sync_sb.cxx_peek_generated_primitive();
				if (!prim.valid()) { break; }
				if (!_io.primitive_acceptable()) { break; }

				uint64_t   const  id      = _sync_sb.cxx_peek_generated_id(prim);
				Cbe::Super_block &sb      = _super_block[id];
				Cbe::Block_data  &sb_data = *reinterpret_cast<Cbe::Block_data*>(&sb);

				_io.submit_primitive(Tag::SYNC_SB_TAG, prim, _io_data, sb_data);
				_sync_sb.cxx_drop_generated_primitive(prim);
				progress |= true;
			}

			/*********************
			 ** Crypto handling **
			 *********************
			 *
			 * The Crypto module has its own internal buffer, data has to be
			 * copied in and copied out.
			 */

			bool const crypto_progress = _crypto.cxx_execute();
			progress |= crypto_progress;
			LOG_PROGRESS(crypto_progress);

			/*
			 * Only writes primitives (encrypted data) are handled here,
			 * read primitives (decrypred data) are handled in 'give_read_data'.
			 */
			while (true) {

				Cbe::Primitive prim = _crypto.cxx_peek_completed_primitive();
				if (!prim.valid() || prim.read()) { break; }

				if (prim.success != Cbe::Primitive::Success::TRUE) {
					DBG(prim);
					throw Primitive_failed();
				}

				Write_back_data_index const idx = _write_back.peek_generated_crypto_data(prim);
				Cbe::Block_data &data = _write_back_data.item[idx.value];
				// XXX instead of copying the data just ask the crypto module for the resulting
				//     hash and omit further processing in case the operation failed
				_crypto.cxx_copy_completed_data(prim, data);
				_write_back.mark_completed_crypto_primitive(prim, data);

				_crypto.cxx_drop_completed_primitive(prim);
				progress |= true;
			}

			/*
			 * Since encryption is performed when calling 'execute' and decryption
			 * is handled differently, all we have to do here is to drop and mark
			 * complete.
			 */
			while (true) {

				Cbe::Primitive prim = _crypto.cxx_peek_generated_primitive();
				if (!prim.valid()) { break; }

				_crypto.cxx_drop_generated_primitive(prim);
				_crypto.cxx_mark_completed_primitive(prim);

				progress |= true;
			}

			/********************
			 ** Cache handling **
			 ********************/

			/*
			 * Pass the data used by the module in by reference so that it
			 * can be shared by the other modules. The method will internally
			 * copy read job data into the chosen entry. In doing so it might
			 * evict an already populated entry.
			 */
			bool const cache_progress = _cache.execute(_cache_data, _cache_job_data,
			                                           _time.timestamp());
			progress |= cache_progress;
			LOG_PROGRESS(cache_progress);

			/*
			 * Read data from the block device to fill the cache.
			 */
			while (true) {

				Cbe::Primitive prim = _cache.peek_generated_primitive();
				if (!prim.valid()) { break; }
				if (!_io.primitive_acceptable()) { break; }

				Cache_Index const idx = _cache.peek_generated_data_index(prim);
				Cbe::Block_data &data = _cache_job_data.item[idx.value];

				_cache.drop_generated_primitive(prim);

				_io.submit_primitive(Tag::CACHE_TAG, prim, _io_data, data);
				progress |= true;
			}

			/******************
			 ** I/O handling **
			 ******************/

			bool const io_progress = _io.execute(_io_data);
			progress |= io_progress;
			LOG_PROGRESS(io_progress);

			while (true) {

				Cbe::Primitive prim = _io.peek_completed_primitive();
				if (!prim.valid()) { break; }

				if (prim.success != Cbe::Primitive::Success::TRUE) {
					DBG(prim);
					throw Primitive_failed();
				}

				bool _progress = true;
				switch (prim.tag) {
				case Tag::CRYPTO_TAG_DECRYPT:
					if (!_crypto.cxx_primitive_acceptable()) {
						_progress = false;
					} else {
						// Genode::uint32_t const idx = _io.peek_completed_data_index(prim);
						// Cbe::Block_data   &data = _io_data.item[idx];
						Cbe::Block_data &data = _io.peek_completed_data(prim);
						Cbe::Tag const orig_tag = _io.peek_completed_tag(prim);

						prim.tag = orig_tag;
						_crypto.cxx_submit_primitive(prim, data, _crypto_data);
					}
					break;
				case Tag::CACHE_TAG:
					// XXX proper solution pending
					// Genode::memcpy(&_cache_job_data.item[0], &_io_data.item[0], sizeof (Cbe::Block_data));
					_cache.cxx_mark_completed_primitive(prim);
					break;
				case Tag::CACHE_FLUSH_TAG:
					_cache_flusher.cxx_mark_generated_primitive_complete(prim);
					break;
				case Tag::WRITE_BACK_TAG:
					_write_back.mark_completed_io_primitive(prim);
					break;
				case Tag::SYNC_SB_TAG:
					_sync_sb.cxx_mark_generated_primitive_complete(prim);
					break;
				// XXX check for FREE_TREE_TAG
				case Tag::FREE_TREE_TAG_WB:
					prim.tag = Tag::WRITE_BACK_TAG;
					_free_tree->mark_generated_primitive_complete(prim);
					break;
				case Tag::FREE_TREE_TAG_IO:
					prim.tag = Tag::IO_TAG;
					_free_tree->mark_generated_primitive_complete(prim);
					break;
				default: break;
				}
				if (!_progress) { break; }

				_io.drop_completed_primitive(prim);
				progress |= true;
			}

			return progress;
		}

		/**
		 * Check if the CBE can accept a new requeust
		 *
		 * \return true if a request can be accepted, otherwise false
		 */
		bool request_acceptable() const
		{
			return _request_pool.request_acceptable();
		}

		/**
		 * Submit a new request
		 *
		 * This method must only be called after executing 'request_acceptable'
		 * returned true.
		 *
		 * \param request  block request
		 */
		void submit_request(Cbe::Request const &request)
		{
			Number_of_primitives const num = _splitter.number_of_primitives(request);
			_request_pool.submit_request(request, num);
		}

		/**
		 * Check for any completed request
		 *
		 * \return a valid block request will be returned if there is an
		 *         completed request, otherwise an invalid one
		 */
		Cbe::Request peek_completed_request() const
		{
			return _request_pool.peek_completed_request();
		}

		/**
		 * Drops the completed request
		 *
		 * This method must only be called after executing
		 * 'peek_completed_request' returned a valid request.
		 *
		 */
		void drop_completed_request(Cbe::Request const &req)
		{
			_request_pool.drop_completed_request(req);
		}

		/* just an interims solution */
		struct Req_prim
		{
			Cbe::Request   req;
			Cbe::Primitive prim;
			Cbe::Tag       tag;
		};
		Req_prim _req_prim { };

		/**
		 * Return a request that needs access to the block data
		 *
		 * \return valid request in case the is one pending that
		 *         needs data, otherwise an invalid one is returned
		 */
		Cbe::Request need_data() /* const */
			// XXX move req_prim allocation into execute
		{
			if (_req_prim.prim.valid()) { return Cbe::Request { }; }

			/* Crypto module */
			{
				Cbe::Primitive prim = _crypto.cxx_peek_completed_primitive();
				if (prim.valid() && prim.read()) {
					Cbe::Request const req = _request_pool.request_for_tag(prim.tag);
					_req_prim = Req_prim {
						.req  = req,
						.prim = prim,
						.tag  = Cbe::Tag::CRYPTO_TAG
					};
					return req;
				}
			}

			/* Vbd module */
			{
				Cbe::Primitive prim = _vbd->peek_completed_primitive();
				if (prim.valid()) {

					Cbe::Request const req = _request_pool.request_for_tag(prim.tag);
					_req_prim = Req_prim {
						.req  = req,
						.prim = prim,
						.tag  = Cbe::Tag::VBD_TAG
					};
					return req;
				}
			}

			return Cbe::Request { };
		}

		/**
		 *
		 */
		bool give_read_data(Cbe::Request const &request, Cbe::Block_data &data)
		{
			if (!_req_prim.req.equal(request)) { return false; }

			Cbe::Primitive const prim = _req_prim.prim;

			switch (_req_prim.tag) {
			case Cbe::Tag::CRYPTO_TAG:
				_crypto.cxx_copy_completed_data(prim, data);
				_crypto.cxx_drop_completed_primitive(prim);
				_request_pool.mark_completed_primitive(prim);
				DBG("pool complete: ", prim);

				_req_prim = Req_prim { };
				return true;
			case Cbe::Tag::VBD_TAG:
				if (_io.primitive_acceptable()) {
					_io.submit_primitive(Tag::CRYPTO_TAG_DECRYPT, prim, _io_data, data);
					_vbd->drop_completed_primitive(prim);

					_req_prim = Req_prim { };
					return true;
				}
				[[fallthrough]];
			default:
				return false;
			}
		}

		/**
		 *
		 */
		bool give_write_data(Cbe::Request    const &request,
		                     Cbe::Block_data const &data)
		{
			if (!_req_prim.req.equal(request)) { return false; }

			Cbe::Primitive const prim = _req_prim.prim;
			if (_req_prim.tag == Cbe::Tag::VBD_TAG) {

				/*
				 * 1. check free-tree module
				 */
				if (!_free_tree->request_acceptable()) { return false; }

				/*
				 * 2. get old PBA's from Translation module and mark volatile blocks
				 */

				Cbe::Type_1_node_info old_pba[Translation::MAX_LEVELS] { };
				if (!_vbd->trans_get_type_1_info(prim, old_pba)) {
					return false;
				}

				Cbe::Super_block const &sb = _super_block[_cur_sb.value];
				Cbe::Snapshot    const &snap = sb.snapshots[_cur_snap];

				Cbe::Primitive::Number const vba = _vbd->trans_get_virtual_block_address(prim);

				Cbe::Physical_block_address new_pba[Translation::MAX_LEVELS] { };
				Genode::memset(new_pba, 0, sizeof(new_pba));

				uint32_t const trans_height = _vbd->tree_height() + 1;
				uint32_t new_blocks = 0;

				Cbe::Physical_block_address free_pba[Translation::MAX_LEVELS] { };
				uint32_t free_blocks = 0;

				for (uint32_t i = 1; i < trans_height; i++) {
					Cbe::Physical_block_address const pba = old_pba[i].pba;
					Cache_Index     const idx   = _cache.data_index(pba, _time.timestamp());
					Cbe::Block_data const &data = _cache_data.item[idx.value];

					uint32_t const id = _vbd->index_for_level(vba, i);
					Cbe::Type_i_node const *n = reinterpret_cast<Cbe::Type_i_node const*>(&data);

					uint64_t const gen = (n[id].gen & GEN_VALUE_MASK);
					if (gen == _cur_gen || gen == 0) {
						Cbe::Physical_block_address const npba = n[id].pba;
						(void)npba;
						DBG("IN PLACE pba: ", pba, " gen: ", gen, " npba: ", npba);

						new_pba[i-1] = old_pba[i-1].pba;
						continue;
					}

					free_pba[free_blocks] = old_pba[i-1].pba;
					DBG("FREE PBA: ", free_pba[free_blocks]);
					free_blocks++;
					new_blocks++;
				}

				// assert
				if (old_pba[trans_height-1].pba != snap.pba) {
					Genode::error("BUG");
				}

				if (snap.gen == _cur_gen || snap.gen == 0) {
					DBG("IN PLACE root pba: ", old_pba[trans_height-1].pba);
					new_pba[trans_height-1] = old_pba[trans_height-1].pba;
				} else {
					free_pba[free_blocks] = old_pba[trans_height-1].pba;
					DBG("FREE PBA: ", free_pba[free_blocks]);
					free_blocks++;
					new_blocks++;
				}

				DBG("new blocks: ", new_blocks);
				for (uint32_t i = 0; i < trans_height; i++) {
					DBG("new_pba[", i, "] = ", new_pba[i]);
				}

				/* 3. submit list blocks to free tree module... */
				if (new_blocks) {
					_free_tree->submit_request(_cur_gen,
					                           new_blocks,
					                           new_pba, old_pba,
					                           trans_height,
					                           free_pba, free_blocks,
					                           prim, vba, data);
				} else {
					DBG("UPDATE ALL IN PACE");
					/* ... or hand over infos to the Write_back module */
					_write_back.submit_primitive(prim, _cur_gen, vba,
					                             new_pba, old_pba, trans_height,
					                             data, _write_back_data);
				}

				_vbd->drop_completed_primitive(prim);
				_req_prim = Req_prim { };

				// XXX see _write_back.peek_completed_primitive()
				_vbd->trans_inhibit_translation();
				return true;
			}
			return false;
		}
};


class Cbe::Main : Rpc_object<Typed_root<Block::Session>>
{
	private:

		// Main(Main const&) = delete;
		// Main &operator=(Main const &) = delete;

		Env &_env;

		Attached_rom_dataspace _config_rom { _env, "config" };

		bool _show_progress    { false };
		bool _show_if_progress { false };

		Constructible<Attached_ram_dataspace>  _block_ds { };
		Constructible<Block_session_component> _block_session { };

		/* backend Block session, used by I/O module */
		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };
		Heap                _heap        { _env.ram(), _env.rm() };
		Allocator_avl       _block_alloc { &_heap };
		Block::Connection<> _block       { _env, &_block_alloc, TX_BUF_SIZE };

		Cbe::Time _time { _env };

		Constructible<Cbe::Library> _cbe { };

		Cbe::Super_block_index _cur_sb { Cbe::Super_block_index::INVALID };
		Cbe::Super_block       _super_block[Cbe::NUM_SUPER_BLOCKS] { };

		Signal_handler<Main> _request_handler {
			_env.ep(), *this, &Main::_handle_requests };

		void _handle_requests()
		{
			if (!_block_session.constructed()) { return; }

			Block_session_component &block_session = *_block_session;

			uint32_t loop_count = 0;
			for (;;) {

				bool progress = false;

				if (_show_if_progress) {
					Genode::log("\033[33m", ">>> loop_count: ", ++loop_count);
				}

				/*
 				 * Block client request handling
				 */

				block_session.with_requests([&] (Block::Request request) {

					using namespace Genode;

					Cbe::Virtual_block_address const vba = request.operation.block_number;

					if (vba > _cbe->max_vba()) {
						warning("reject request with out-of-range virtual block address ", vba);
						return Block_session_component::Response::REJECTED;
					}

					if (!request.operation.valid()) {
						warning("reject invalid request for virtual block address ", vba);
						return Block_session_component::Response::REJECTED;
					}

					if (!_cbe->request_acceptable()) {
						return Block_session_component::Response::RETRY;
					}

					Cbe::Request req = convert_to(request);
					_cbe->submit_request(req);

					// if (_show_progress || _show_if_progress) {
						Genode::log("NEW request: ", req);
					// }

					progress |= true;
					return Block_session_component::Response::ACCEPTED;
				});

				block_session.try_acknowledge([&] (Block_session_component::Ack &ack) {

					Cbe::Request const &req = _cbe->peek_completed_request();
					if (!req.operation_defined()) { return; }

					_cbe->drop_completed_request(req);

					Block::Request request = convert_from(req);
					ack.submit(request);

					// if (_show_progress || _show_if_progress) {
						Genode::log("ACK request: ", req);
					// }

					progress |= true;
				});

				/*
				 * CBE handling
				 */

				progress |= _cbe->execute(_show_progress, _show_if_progress);

				using Payload = Block::Request_stream::Payload;
				block_session.with_payload([&] (Payload const &payload) {

					Cbe::Request const creq = _cbe->need_data();
					if (!creq.valid()) { return; }

					uint32_t const prim_index = 0; // XXX make sure there is no prim with index > 0

					Block::Request request { };
					request.offset = creq.offset + (prim_index * BLOCK_SIZE);
					request.operation.count = 1;

					payload.with_content(request, [&] (void *addr, Genode::size_t) {

						Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(addr);

						if (creq.read()) {
							progress |= _cbe->give_read_data(creq, data);
						} else

						if (creq.write()) {
							progress |= _cbe->give_write_data(creq, data);
						}
					});
				});

				if (!progress) {
					if (_show_if_progress) {
						Genode::log("\033[33m", ">>> break, no progress");
					}
					break;
				}
			}

			block_session.wakeup_client_if_needed();
		}

		Cbe::Super_block_index _read_superblocks()
		{
			Cbe::Generation        last_gen = 0;
			Cbe::Super_block_index most_recent_sb { Cbe::Super_block_index::INVALID };

			/*
			 * Read all super block slots and use the most recent one.
			 */
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				Util::Block_io io(_block, BLOCK_SIZE, i, 1);
				void const       *src = io.addr<void*>();
				Cbe::Super_block &dst = _super_block[i];
				Genode::memcpy(&dst, src, BLOCK_SIZE);

				/*
				 * For now this always selects the last SB if the generation
				 * is the same and is mostly used for finding the initial SB
				 * with generation == 0.
				 */
				if (dst.valid() && dst.last_secured_generation >= last_gen) {
					most_recent_sb.value = i;
					last_gen = dst.last_secured_generation;
				}

				Sha256_4k::Hash hash { };
				Sha256_4k::Data const &data = *reinterpret_cast<Sha256_4k::Data const*>(&dst);
				Sha256_4k::hash(data, hash);
				Genode::log("SB[", i, "] hash: ", hash);
			}

			return most_recent_sb;
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env) : _env(env)
		{
			/*
			 * We first parse the configuration here which is used to control the
			 * verbosity and the time intervals so we do not have to do that in
			 * the CBE library.
			 */
			_show_progress =
				_config_rom.xml().attribute_value("show_progress", false);

			Cbe::Time::Timestamp const sync = 1000 *
				_config_rom.xml().attribute_value("sync_interval",
				                                  (uint64_t)Cbe::Library::SYNC_INTERVAL);
			Cbe::Time::Timestamp const secure = 1000 *
				_config_rom.xml().attribute_value("sync_interval",
				                                  (uint64_t)Cbe::Library::SECURE_INTERVAL);

			/*
			 * We read all super-block information (at the moment the first 8 blocks
			 * of the block device, one for each SB) synchronously and store the blocks
			 * in the super-block array. The index of the current SB is returned. It
			 * is used by the CBE library to access the right array item.
			 *
			 *
			 * (In the future, to decouple the CBE library from the TA and the securing
			 *  of the SB, I think it makes sense to only give the CBE library just one
			 *  SB to start from and whenever it wants to write a new one, it should pass
			 *  the block on to the outside.)
			 */
			Cbe::Super_block_index curr_sb = _read_superblocks();
			if (curr_sb.value == Cbe::Super_block_index::INVALID) {
				Genode::error("no valid super block found");
				throw -1;
			}

			/*
			 * Set the timout handler directly on the time object which is actually
			 * the same that is used by the Block session.
			 *
			 *
			 * (At the moment the same handler as for the Block session is used for
			 *  both timeouts - I'm not sure if we need this kind of flexibilty but
			 *  it does not hurt us for now.)
			 */
			_time.sync_sigh(_request_handler);
			_time.secure_sigh(_request_handler);

			/*
			 * We construct the CBE library. For now it contains all modules needed
			 * to read and write to the virtual-block-device (VBD). We pass on the
			 * Cbe::Time object that is used to collect timestamp whenever we need it.
			 * It is polled at least once every complete loop to check the sync and
			 * secure interval (and whenever a cache entries is accessed/modified).
			 * In addition the current I/O module requires a Block::Connection and
			 * since we do not want to give the Genode::Env to the CBE library, we
			 * pass on a reference as well. For obvious reasons the SBs it also needs
			 * access to the SBs.
			 *
			 */
			_cbe.construct(_time, sync, secure, _block, _super_block, curr_sb);

			/* finally announce Block session */
			_env.parent().announce(_env.ep().manage(*this));
		}

		/********************
		 ** Root interface **
		 ********************/

		Capability<Session> session(Root::Session_args const &args,
		                            Affinity const &) override
		{
			log("new block session: ", args.string());

			size_t const ds_size =
				Arg_string::find_arg(args.string(), "tx_buf_size").ulong_value(0);

			Ram_quota const ram_quota = ram_quota_from_args(args.string());

			if (ds_size >= ram_quota.value) {
				warning("communication buffer size exceeds session quota");
				throw Insufficient_ram_quota();
			}

			_block_ds.construct(_env.ram(), _env.rm(), ds_size);

			/*
			 * The call to the CBE gives us the highest virtual-block-address.
			 * By adding 1 we can use that as 'number of blocks', i.e., the
			 * size of the block device, for the announced Block session.
			 */
			_block_session.construct(_env.rm(), _block_ds->cap(), _env.ep(),
			                         _request_handler, _cbe->max_vba() + 1);

			_block.tx_channel()->sigh_ack_avail(_request_handler);
			_block.tx_channel()->sigh_ready_to_submit(_request_handler);

			/*
			 * Dump the current SB info for diagnostic reasons.
			 */
			_cbe->dump_cur_sb_info();

			return _block_session->cap();
		}

		void upgrade(Capability<Session>, Root::Upgrade_args const &) override { }

		void close(Capability<Session>) override
		{
			_block.tx_channel()->sigh_ack_avail(Signal_context_capability());
			_block.tx_channel()->sigh_ready_to_submit(Signal_context_capability());

			_block_session.destruct();
			_block_ds.destruct();
		}
};


extern "C" void print_size(Genode::size_t sz) {
	Genode::log(sz);
}


extern "C" void print_u64(unsigned long long const u) { Genode::log(u); }
extern "C" void print_u32(unsigned int const u) { Genode::log(u); }
extern "C" void print_u16(unsigned short const u) { Genode::log(u); }
extern "C" void print_u8(unsigned char const u) { Genode::log(u); }



Genode::Env *__genode_env;
Terminal::Connection *__genode_terminal;


extern "C" void adainit();


void Component::construct(Genode::Env &env)
{
	/* make ada-runtime happy */
	__genode_env = &env;

	static Terminal::Connection term { env };
	__genode_terminal = &term;

	env.exec_static_constructors();

	adainit();

	static Cbe::Main inst(env);
}
