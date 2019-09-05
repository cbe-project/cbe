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


/* repo includes */
#include <util/sha256_4k.h>

/* cbe includes */
#include <cbe/library.h>

/* local includes */
#define DEBUG 1
#if defined(DEBUG) && DEBUG > 0

namespace Cbe {

	Genode::uint32_t object_size(Tree_helper const &);
}


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

#include "library.h"


/*********************************
 ** Cbe::Library implementation **
 *********************************/

bool Cbe::Library::_discard_snapshot(Cbe::Snapshot active[Cbe::NUM_SNAPSHOTS],
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

void Cbe::Library::ack_sync_timeout_request()
{
	_sync_timeout_request = { false, 0 };
}

void Cbe::Library::ack_secure_timeout_request()
{
	_secure_timeout_request = { false, 0 };
}

Cbe::Timeout_request Cbe::Library::peek_sync_timeout_request() const
{
	return _sync_timeout_request;
}

Cbe::Timeout_request Cbe::Library::peek_secure_timeout_request() const
{
	return _secure_timeout_request;
}


void Cbe::Library::_dump_cur_sb_info() const
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


Cbe::Library::Library(Time::Timestamp  const  now,
                      Time::Timestamp  const  sync,
                      Time::Timestamp  const  secure,
                      Cbe::Super_block        sbs[Cbe::NUM_SUPER_BLOCKS],
                      Cbe::Super_block_index  current_sb)
:
	_sync_interval(sync),
	_last_time(now),
	_secure_interval(secure),
	_last_secure_time(now)
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
	if (_sync_interval)   { _sync_timeout_request = { true, _sync_interval }; }
	if (_secure_interval) { _secure_timeout_request = { true, _secure_interval }; }

	/* for diagnostic reasons */
	_dump_cur_sb_info();
}


void Cbe::Library::dump_cur_sb_info() const
{
	_dump_cur_sb_info();
}


Cbe::Virtual_block_address Cbe::Library::max_vba() const
{
	return _super_block[_cur_sb.value].snapshots[_cur_snap].leaves - 1;
}


void Cbe::Library::execute(Time::Timestamp now, bool show_progress, bool show_if_progress)
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
	Cbe::Time::Timestamp const diff_time = now - _last_time;
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
			_last_time = now;
			_sync_timeout_request = { true, _sync_interval };
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
	Cbe::Time::Timestamp const diff_secure_time = now - _last_secure_time;
	if (diff_secure_time >= _secure_interval && !_secure_superblock) {

		if (_superblock_dirty) {
			Genode::log("\033[93;44m", __func__,
			            " SEAL current super-block: ", _cur_sb);
			_secure_superblock = true;
		} else {
			DBG("no snapshots created, re-arm trigger");
			_last_secure_time = now;
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

		_free_tree->execute(sb.snapshots,
		                    _last_secured_generation,
		                    _free_tree_trans_data,
		                    _cache, _cache_data,
		                    _free_tree_query_data,
		                    now);
		bool const ft_progress = _free_tree->execute_progress();
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

		if (prim.success != Cbe::Primitive::Success::FALSE) { break; }

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
		DBG("-----------------------> current primitive: ", current_primitive, " FINISHED");
		current_primitive = Cbe::Primitive { };
		// XXX
		_vbd->trans_resume_translation();

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

		_io.submit_primitive(tag, prim, _io_data, *data, true);

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

		current_primitive = prim;

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

	if (current_primitive.valid()) {
		DBG("-----------------------> current primitive: ", current_primitive);
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
		_vbd->execute(_trans_data, _cache, _cache_data, now);
		bool const vbd_progress = _vbd->execute_progress();
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
			_last_time = now;
			_sync_timeout_request = { true, _sync_interval };
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
		DBG("-----------------------> current primitive: ", current_primitive, " FINISHED");
		current_primitive = Cbe::Primitive { };
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
		Cbe::Block_data           &data = _write_back_data.item[idx.value];
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
		Cache_Index const idx        = _cache.data_index(pba, now);
		Cache_Index const update_idx = _cache.data_index(update_pba, now);

		Cbe::Block_data const &data        = _cache_data.item[idx.value];
		Cbe::Block_data       &update_data = _cache_data.item[update_idx.value];

		/*
		 * (Later on we can remove the tree_helper here as the outer degree,
		 *  which is used to calculate the entry in the inner node from the
		 *  VBA is set at compile-time.)
		 */
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
			.value = (uint8_t)((_cur_sb.value + 1) % Cbe::NUM_SUPER_BLOCKS)
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
		_last_secure_time = now;
		_secure_timeout_request = { true, _secure_interval };
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
	                                           now);
	progress |= cache_progress;
	LOG_PROGRESS(cache_progress);

	/*
	 * Read data from the block device to fill the cache.
	 *
	 * (The Cache module has no 'peek_completed_primitive()' method,
	 *  all modules using the cache have to poll and might be try to
	 *  submit the same request multiple times (see its acceptable
	 *  method). It makes sense to change the Cache module so that it
	 *  works the rest of modules. That would require restructing
	 *  the modules, though.)
	 */
	while (true) {

		Cbe::Primitive prim = _cache.peek_generated_primitive();
		if (!prim.valid()) { break; }
		if (!_io.primitive_acceptable()) { break; }

		Cache_Index const idx = _cache.peek_generated_data_index(prim);
		Cbe::Block_data &data = _cache_job_data.item[idx.value];

		_cache.drop_generated_primitive(prim);

		_io.submit_primitive(Tag::CACHE_TAG, prim, _io_data, data, true);
		progress |= true;
	}

	/******************
	 ** I/O handling **
	 ******************
	 *
	 * This module handles all the block backend I/O and has to
	 * work with all most all modules. IT uses the 'Tag' field
	 * to differentiate the modules.
	 */

	while (true) {

		Cbe::Primitive prim = _io.peek_completed_primitive();
		if (!prim.valid()) { break; }

		if (prim.success != Cbe::Primitive::Success::TRUE) {
			DBG(prim);
			throw Primitive_failed();
		}

		Genode::uint32_t const idx = _io.peek_completed_data_index(prim);
		Cbe::Block_data      &data = _io_data.item[idx];

		/*
		 * Whenever we cannot hand a successful primitive over
		 * to the corresponding module, leave the loop but keep
		 * the completed primitive so that it might be processed
		 * next time.
		 */
		bool mod_progress = true;
		switch (prim.tag) {

		case Tag::CRYPTO_TAG_DECRYPT:
			if (!_crypto.cxx_primitive_acceptable()) {
				mod_progress = false;
			} else {
				Cbe::Tag const orig_tag = _io.peek_completed_tag(prim);

				/*
				 * Having to override the tag is needed because of the way
				 * the Crypto module is hooked up in the overall data flow.
				 * Since it is the one that acknowledges the primitive to the
				 * pool in the read case, we have to use the tag the pool
				 * module uses.
				 */
				prim.tag = orig_tag;
				_crypto.cxx_submit_primitive(prim, data, _crypto_data);
			}
			break;

		case Tag::CACHE_TAG:
			// XXX we need a proper method for getting the right cache job
			//     data index, for now rely on the knowledge that there is
			//     only one item
			Genode::memcpy(&_cache_job_data.item[0], &data, sizeof (Cbe::Block_data));
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

		case Tag::FREE_TREE_TAG_WB:
			prim.tag = Tag::WRITE_BACK_TAG;
			_free_tree->mark_generated_primitive_complete(prim);
			break;

		case Tag::FREE_TREE_TAG_IO:
			prim.tag = Tag::IO_TAG;
			// XXX we need a proper method for getting the right query
			//     data index, for now rely on the knowledge that there
			//     is only one item
			Genode::memcpy(&_free_tree_query_data.item[0], &data, sizeof (Cbe::Block_data));
			_free_tree->mark_generated_primitive_complete(prim);
			break;

		default: break;
		}
		if (!mod_progress) { break; }

		_io.drop_completed_primitive(prim);
		progress |= true;
	}

	_execute_progress = progress;
}


bool Cbe::Library::request_acceptable() const
{
	return _request_pool.request_acceptable();
}


void Cbe::Library::submit_request(Cbe::Request const &request)
{
	Number_of_primitives const num = _splitter.number_of_primitives(request);
	_request_pool.submit_request(request, num);
}


Cbe::Request Cbe::Library::peek_completed_request() const
{
	return _request_pool.peek_completed_request();
}


void Cbe::Library::drop_completed_request(Cbe::Request const &req)
{
	_request_pool.drop_completed_request(req);
}


Cbe::Request Cbe::Library::need_data()
{
	if (_backend_req_prim.prim.valid()) { return Cbe::Request { }; }

	/* I/O module */
	{
		Cbe::Primitive prim = _io.peek_generated_primitive();
		if (prim.valid()) {
			Cbe::Request req = Cbe::convert_from(prim);

			_backend_req_prim = Req_prim {
				.req         = req,
				.prim        = prim,
				.tag         = Cbe::Tag::IO_TAG,
				.in_progress = false,
			};
			return req;
		}
	}

	return Cbe::Request { };
}


bool Cbe::Library::take_read_data(Cbe::Request const &request)
{
	/*
	 * For now there is only one request pending.
	 */
	if (!_backend_req_prim.req.equal(request)
	    || _backend_req_prim.in_progress) { return false; }

	Cbe::Primitive prim = _backend_req_prim.prim;

	if (_backend_req_prim.tag == Cbe::Tag::IO_TAG) {
		_io.drop_generated_primitive(prim);

		_backend_req_prim.in_progress = true;
		return true;
	}
	return false;
}


bool Cbe::Library::ack_read_data(Cbe::Request    const &request,
                                 Cbe::Block_data const &data)
{
	/*
	 * For now there is only one request pending.
	 */
	if (!_backend_req_prim.req.equal(request)
	    || !_backend_req_prim.in_progress) { return false; }

	Cbe::Primitive prim = _backend_req_prim.prim;

	if (_backend_req_prim.tag == Cbe::Tag::IO_TAG) {

		bool const success = request.success == Cbe::Request::Success::TRUE;

		if (success) {

			Genode::uint32_t const idx = _io.peek_generated_data_index(prim);
			Cbe::Block_data   &io_data = _io_data.item[idx];
			Genode::memcpy(&io_data, &data, sizeof (Cbe::Block_data));
		}

		prim.success = success ? Cbe::Primitive::Success::TRUE
		                       : Cbe::Primitive::Success::FALSE;
		_io.mark_generated_primitive_complete(prim);

		_backend_req_prim = Req_prim { };
		return true;
	}

	return false;
}


bool Cbe::Library::take_write_data(Cbe::Request    const &request,
                                   Cbe::Block_data       &data)
{
	/*
	 * For now there is only one request pending.
	 */
	if (!_backend_req_prim.req.equal(request)
	    || _backend_req_prim.in_progress) { return false; }

	Cbe::Primitive const prim = _backend_req_prim.prim;

	if (_backend_req_prim.tag == Cbe::Tag::IO_TAG) {

		Genode::uint32_t const idx = _io.peek_generated_data_index(prim);
		Cbe::Block_data   &io_data = _io_data.item[idx];
		Genode::memcpy(&data, &io_data, sizeof (Cbe::Block_data));

		_io.drop_generated_primitive(prim);

		_backend_req_prim.in_progress = true;
		return true;
	}

	return false;
}


bool Cbe::Library::ack_write_data(Cbe::Request const &request)
{
	/*
	 * For now there is only one request pending.
	 */
	if (!_backend_req_prim.req.equal(request)
	    || !_backend_req_prim.in_progress) { return false; }

	Cbe::Primitive prim = _backend_req_prim.prim;

	if (_backend_req_prim.tag == Cbe::Tag::IO_TAG) {

		bool const success = request.success == Cbe::Request::Success::TRUE;

		prim.success = success ? Cbe::Primitive::Success::TRUE
		                       : Cbe::Primitive::Success::FALSE;
		_io.mark_generated_primitive_complete(prim);

		_backend_req_prim = Req_prim { };
		return true;
	}

	return false;
}


Cbe::Request Cbe::Library::have_data()
{
	// XXX move req_prim allocation into execute
	if (_frontend_req_prim.prim.valid()) { return Cbe::Request { }; }

	/*
	 * When it was a read request, we need the location to
	 * where the Crypto should copy the decrypted data.
	 */
	{
		Cbe::Primitive prim = _crypto.cxx_peek_completed_primitive();
		if (prim.valid() && prim.read()) {
			Cbe::Request const req = _request_pool.request_for_tag(prim.tag);
			_frontend_req_prim = Req_prim {
				.req  = req,
				.prim = prim,
				.tag  = Cbe::Tag::CRYPTO_TAG,
				.in_progress = false,
			};
			return req;
		}
	}

	/*
	 * When it was a read request, we need access to the data the Crypto
	 * module should decrypt and if it was a write request we need the location
	 * from where to read the new leaf data.
	 */
	{
		Cbe::Primitive prim = _vbd->peek_completed_primitive();
		if (prim.valid()) {

			Cbe::Request const req = _request_pool.request_for_tag(prim.tag);
			_frontend_req_prim = Req_prim {
				.req  = req,
				.prim = prim,
				.tag  = Cbe::Tag::VBD_TAG,
				.in_progress = false,
			};
			return req;
		}
	}

	/*
	 * The free-tree needs the data to give to the Write_back module.
	 */
	{
		Cbe::Primitive prim = _free_tree->peek_completed_primitive();
		if (prim.valid() && (prim.success == Cbe::Primitive::Success::TRUE)) {

			Cbe::Request const req = _request_pool.request_for_tag(prim.tag);
			_frontend_req_prim = Req_prim {
				.req  = req,
				.prim = prim,
				.tag  = Cbe::Tag::FREE_TREE_TAG,
				.in_progress = false,
			};
			return req;
		}
	}

	return Cbe::Request { };
}


Genode::uint64_t Cbe::Library::give_data_index(Cbe::Request const &request)
{
	/*
	 * For now there is only one request pending.
	 */
	if (!_frontend_req_prim.req.equal(request)) { return ~0ull; }

	return _frontend_req_prim.prim.index;
}


void Cbe::Library::give_read_data(Cbe::Request const &request, Cbe::Block_data &data, bool &processable)
{
	/*
	 * For now there is only one request pending.
	 */
	if (!_frontend_req_prim.req.equal(request)) { processable = false; return; }

	Cbe::Primitive const prim = _frontend_req_prim.prim;

	switch (_frontend_req_prim.tag) {
	case Cbe::Tag::CRYPTO_TAG:
		_crypto.cxx_copy_completed_data(prim, data);
		_crypto.cxx_drop_completed_primitive(prim);
		_request_pool.mark_completed_primitive(prim);
		DBG("-----------------------> current primitive: ", current_primitive, " FINISHED");
		current_primitive = Cbe::Primitive { };
		DBG("pool complete: ", prim);

		_frontend_req_prim = Req_prim { };
		processable = true; return;
	case Cbe::Tag::VBD_TAG:
		/*
		 * We have reset _frontend_req_prim before because in case there is currently
		 * I/O pending, we have to make sure 'have_data' is called again.
		 */
		_frontend_req_prim = Req_prim { };
		if (_io.primitive_acceptable()) {
			_io.submit_primitive(Tag::CRYPTO_TAG_DECRYPT, prim, _io_data, data, true);
			_vbd->drop_completed_primitive(prim);

			processable = true; return;
		}
		[[fallthrough]];
	default:
		processable = false; return;
	}
}


bool Cbe::Library::give_write_data(Time::Timestamp const now,
                                   Cbe::Request    const &request,
                                   Cbe::Block_data const &data)
{
	/*
	 * For now there is only one request pending.
	 */
	if (!_frontend_req_prim.req.equal(request)) { return false; }

	Cbe::Primitive const prim = _frontend_req_prim.prim;

	if (_frontend_req_prim.tag == Cbe::Tag::FREE_TREE_TAG) {

		if (!_write_back.primitive_acceptable()) { return false; }

		if (_free_tree_retry_count) {
			DBG("reset retry count: ", _free_tree_retry_count);
			_free_tree_retry_count = 0;
		}

		/*
		 * Accessing the write-back data in this manner is still a shortcut
		 * and probably will not work with SPARK - we have to get rid of
		 * the 'block_data' pointer.
		 */
		Free_tree::Write_back_data const wb = _free_tree->peek_completed_wb_data(prim);

		_write_back.submit_primitive(wb.prim, wb.gen, wb.vba,
		                             wb.new_pba, wb.old_pba, wb.tree_height,
		                             data, _write_back_data);

		_free_tree->drop_completed_primitive(prim);
		_frontend_req_prim = Req_prim { };
		return true;
	} else

	/*
	 * The VBD module translated a write request, writing the data
	 * now to disk involves multiple steps:
	 *
	 *  1. Gathering of all nodes in the branch and looking up the
	 *     volatile ones (those, which belong to the current generation
	 *     and will be updated in place).
	 *  2. Allocate new blocks if needed by consulting the FT
	 *  3. Updating all entries in the nodes
	 *  4. Writing the branch back to the block device.
	 *
	 * Those steps are handled by different modules, depending on
	 * the allocation of new blocks.
	 */
	if (_frontend_req_prim.tag == Cbe::Tag::VBD_TAG) {

		/*
		 * As usual check first we can submit new requests.
		 */
		if (!_free_tree->request_acceptable()) { return false; }

		/*
		 * Then (ab-)use the Translation module and its still pending
		 * request to get all old PBAs, whose generation we then check.
		 * The order of the array items corresponds to the level within
		 * the tree.
		 */
		Cbe::Type_1_node_info old_pba[Translation::MAX_LEVELS] { };
		if (!_vbd->trans_can_get_type_1_info(prim, old_pba)) {
			return false;
		}
		_vbd->trans_get_type_1_info(old_pba);

		uint32_t const trans_height = _vbd->tree_height() + 1;

		/*
		 * Make sure we work with the proper snapshot.
		 *
		 * (This check may be removed at some point.)
		 */
		Cbe::Super_block const &sb = _super_block[_cur_sb.value];
		Cbe::Snapshot    const &snap = sb.snapshots[_cur_snap];
		if (old_pba[trans_height-1].pba != snap.pba) {
			Genode::error("BUG");
		}

		/*
		 * The array of new_pba will either get populated from the old_pba
		 * content or from newly allocated blocks.
		 * The order of the array items corresponds to the level within
		 * the tree.
		 */
		Cbe::Physical_block_address new_pba[Translation::MAX_LEVELS] { };
		Genode::memset(new_pba, 0, sizeof(new_pba));
		uint32_t new_blocks = 0;

		/*
		 * This array contains all blocks that will get freed or rather
		 * marked as reserved in the FT as they are still referenced by
		 * an snapshot.
		 */
		Cbe::Physical_block_address free_pba[Translation::MAX_LEVELS] { };
		uint32_t free_blocks = 0;

		/*
		 * Get the corresponding VBA that we use to calculate the index
		 * for the edge in the node for a given level within the tree.
		 */
		Cbe::Primitive::Number const vba = _vbd->trans_get_virtual_block_address(prim);

		/*
		 * Here only the inner nodes, i.e. all nodes excluding root and leaf,
		 * are considered. The root node is checked afterwards as we need the
		 * information of the current snapshot for that.
		 */
		for (uint32_t i = 1; i < trans_height; i++) {

			/*
			 * Use the old PBA to get the node's data from the cache and
			 * use it check how we have to handle the node.
			 */
			Cbe::Physical_block_address const pba = old_pba[i].pba;
			Cache_Index     const idx   = _cache.data_index(pba, now);
			Cbe::Block_data const &data = _cache_data.item[idx.value];

			uint32_t const id = _vbd->index_for_level(vba, i);
			Cbe::Type_i_node const *n = reinterpret_cast<Cbe::Type_i_node const*>(&data);

			uint64_t const gen = n[id].gen;
			/*
			 * In case the generation of the entry is the same as the current
			 * generation OR if the generation is 0 (which means it was never
			 * used before) the block is volatile and we change it in place
			 * and store it directly in the new_pba array.
			 */
			if (gen == _cur_gen || gen == 0) {
				Cbe::Physical_block_address const npba = n[id].pba;
				(void)npba;
				DBG("IN PLACE pba: ", pba, " gen: ", gen, " npba: ", npba);

				new_pba[i-1] = old_pba[i-1].pba;
				continue;
			}

			/*
			 * Otherwise add the block to the free_pba array so that the
			 * FT will reserved it and note that we need another new block.
			 */
			free_pba[free_blocks] = old_pba[i-1].pba;
			DBG("FREE PBA: ", free_pba[free_blocks]);
			free_blocks++;
			new_blocks++;
		}

		/* check root node */
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

		/*
		 * Since there are blocks we cannot change in place, use the
		 * FT module to allocate the blocks. As we have to reserve
		 * the blocks we implicitly will free (free_pba items), pass
		 * on the current generation.
		 */
		if (new_blocks) {
			_free_tree->submit_request(_cur_gen,
			                           new_blocks,
			                           new_pba, old_pba,
			                           trans_height,
			                           free_pba,
			                           prim, vba);
		} else {
			/*
			 * The complete branch is still part of the current generation,
			 * call the Write_back module directly.
			 *
			 * (We would have to check if the module can acutally accept
			 *  the request...)
			 */
			DBG("UPDATE ALL IN PACE");
			_write_back.submit_primitive(prim, _cur_gen, vba,
			                             new_pba, old_pba, trans_height,
			                             data, _write_back_data);
		}

		_vbd->drop_completed_primitive(prim);
		_frontend_req_prim = Req_prim { };

		/*
		 * Inhibit translation which effectively will suspend the
		 * Translation modules operation and will stall all other
		 * pending requests to make sure all following request will
		 * use the newest tree.
		 *
		 * (It stands to reasons whether we can remove this check
		 *  if we make sure that only the requests belonging to
		 *  the same branch are serialized.)
		 */
		_vbd->trans_inhibit_translation();
		return true;
	}
	return false;
}


/****************************************
 ** Cbe::Public_Library implementation **
 ****************************************/

static Genode::Constructible<Cbe::Library> _cbe_library { };


Cbe::Public_Library::Public_Library(Time::Timestamp  const  now,
                                    Time::Timestamp  const  sync,
                                    Time::Timestamp  const  secure,
                                    Cbe::Super_block        sbs[Cbe::NUM_SUPER_BLOCKS],
                                    Cbe::Super_block_index  current_sb)
{
	if (_cbe_library.constructed()) {
		Genode::error("cannot construct CBE library multiple times");
		throw Genode::Exception();
	}

	_cbe_library.construct(now, sync, secure, sbs, current_sb);
}

void Cbe::Public_Library::ack_sync_timeout_request()
{
	_cbe_library->ack_sync_timeout_request();
}

void Cbe::Public_Library::ack_secure_timeout_request()
{
	_cbe_library->ack_secure_timeout_request();
}

Cbe::Timeout_request Cbe::Public_Library::peek_sync_timeout_request() const
{
	return _cbe_library->peek_sync_timeout_request();
}

Cbe::Timeout_request Cbe::Public_Library::peek_secure_timeout_request() const
{
	return _cbe_library->peek_secure_timeout_request();
}

void Cbe::Public_Library::dump_cur_sb_info() const
{
	_cbe_library->dump_cur_sb_info();
}


Cbe::Virtual_block_address Cbe::Public_Library::max_vba() const
{
	return _cbe_library->max_vba();
}


void Cbe::Public_Library::execute(Time::Timestamp now, bool show_progress, bool show_if_progress)
{
	_cbe_library->execute(now, show_progress, show_if_progress);
}


bool Cbe::Public_Library::execute_progress() const
{
	return _cbe_library->execute_progress();
}


bool Cbe::Public_Library::request_acceptable() const
{
	return _cbe_library->request_acceptable();
}


void Cbe::Public_Library::submit_request(Cbe::Request const &r)
{
	_cbe_library->submit_request(r);
}


Cbe::Request Cbe::Public_Library::peek_completed_request() const
{
	return _cbe_library->peek_completed_request();
}


void Cbe::Public_Library::drop_completed_request(Cbe::Request const &r)
{
	_cbe_library->drop_completed_request(r);
}


Cbe::Request Cbe::Public_Library::need_data()
{
	return _cbe_library->need_data();
}


bool Cbe::Public_Library::take_read_data(Cbe::Request const &r)
{
	return _cbe_library->take_read_data(r);
}


bool Cbe::Public_Library::ack_read_data(Cbe::Request    const &r,
                                        Cbe::Block_data const &d)
{
	return _cbe_library->ack_read_data(r, d);
}


bool Cbe::Public_Library::take_write_data(Cbe::Request    const &r,
                                          Cbe::Block_data       &d)
{
	return _cbe_library->take_write_data(r, d);
}


bool Cbe::Public_Library::ack_write_data(Cbe::Request const &r)
{
	return _cbe_library->ack_write_data(r);
}


Cbe::Request Cbe::Public_Library::have_data()
{
	return _cbe_library->have_data();
}


Genode::uint64_t Cbe::Public_Library::give_data_index(Cbe::Request const &r)
{
	return _cbe_library->give_data_index(r);
}


void Cbe::Public_Library::give_read_data(Cbe::Request const &r,
                                         Cbe::Block_data    &d,
                                         bool               &p)
{
	return _cbe_library->give_read_data(r, d, p);
}


bool Cbe::Public_Library::give_write_data(Time::Timestamp const now,
                                          Cbe::Request    const &r,
                                          Cbe::Block_data const &d)
{
	return _cbe_library->give_write_data(now, r, d);
}
