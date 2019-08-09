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
		if (_show_progress || (_show_if_progress && v)) { \
			Genode::log(#v, ": ", v); \
		} \
	} while (0)


/*************
 ** modules **
 *************/

#include <cache_module.h>
#include <crypto_module.h>
#include <io_module.h>
#include <request_pool_module.h>
#include <splitter_module.h>
#include <translation_module.h>
#include <write_through_cache_module.h>
#include <write_back_module.h>
#include <sync_sb_module.h>
// #include <reclaim_module.h>
#include <flusher_module.h>

#include "free_tree.h"
#include "virtual_block_device.h"

class Cbe::Main : Rpc_object<Typed_root<Block::Session>>
{
	private:

		// Main(Main const&) = delete;
		// Main &operator=(Main const &) = delete;

		Env &_env;

		Attached_rom_dataspace _config_rom { _env, "config" };

		Constructible<Attached_ram_dataspace>  _block_ds { };
		Constructible<Block_session_component> _block_session { };

		/* back end Block session */
		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };
		Heap                _heap        { _env.ram(), _env.rm() };
		Allocator_avl       _block_alloc { &_heap };
		Block::Connection<> _block       { _env, &_block_alloc, TX_BUF_SIZE };

		/* modules */

		/*
		 * Check if we provided enough memory for all the SPARK objects.
		 * We use the check of '_object_sizes_match' in the constructor
		 * to prevent the compiler from optimizing the call away.
		 */

		bool _check_object_sizes()
		{
			Cbe::assert_valid_object_size<Module::Cache>();
			Cbe::assert_valid_object_size<Module::Crypto>();
			Cbe::assert_valid_object_size<Module::Request_pool>();
			Cbe::assert_valid_object_size<Module::Splitter>();

			return true;
		}

		bool const _object_sizes_match { _check_object_sizes() };

		enum {
			MAX_REQS      = 16,
			IO_ENTRIES    = 1,
		};

		Cbe::Virtual_block_address _max_vba { 0 };

		using Pool        = Module::Request_pool;
		using Splitter    = Module::Splitter;
		using Translation = Module::Translation;
		using Translation_Data = Module::Translation_Data;

		using Crypto      = Module::Crypto;
		using Io          = Module::Block_io<IO_ENTRIES, BLOCK_SIZE>;
		using Io_index    = Module::Block_io<IO_ENTRIES, BLOCK_SIZE>::Index;
		using Write_back       = Module::Write_back<Translation::MAX_LEVELS, Cbe::Type_i_node>;
		using Write_back_Index = Module::Write_back<Translation::MAX_LEVELS, Cbe::Type_i_node>::Index;
		using Sync_sb     = Module::Sync_sb;

		Pool     _request_pool { };
		Splitter _splitter     { };

		Time _time { _env };
		enum {
			SYNC_INTERVAL   = 0ull,
			SECURE_INTERVAL = 5ull,
		};
		uint64_t _sync_interval         { SYNC_INTERVAL };
		Cbe::Time::Timestamp _last_time { _time.timestamp() };
		uint64_t _secure_interval              { SECURE_INTERVAL };
		Cbe::Time::Timestamp _last_secure_time { _time.timestamp() };

		Crypto   _crypto       { "All your base are belong to us  " };
		Block_data _crypto_data { };

		Io         _io                  { _block };
		Block_data _io_data[IO_ENTRIES] { };

		using Cache          = Module::Cache;
		using Cache_Index    = Module::Cache_Index;
		using Cache_Data     = Module::Cache_Data;
		using Cache_Job_Data = Module::Cache_Job_Data;
		using Flusher        = Module::Flusher;

		Cache           _cache          { };
		Cache_Data      _cache_data     { };
		Cache_Job_Data  _cache_job_data { };
		Flusher         _flusher        { };
		bool            _cache_dirty    { false };

		Translation_Data       _trans_data     { };
		Constructible<Cbe::Virtual_block_device> _vbd { };

		Write_back _write_back { };
		Block_data _write_back_data[Translation::MAX_LEVELS] { };
		Sync_sb    _sync_sb { };

		Constructible<Cbe::Free_tree> _free_tree { };
		enum { FREE_TREE_RETRY_LIMIT = 3u, };
		uint32_t                      _free_tree_retry_count { 0 };
		Translation_Data         _free_tree_trans_data { };
		Query_data               _free_tree_query_data { };

		Block_data* _data(Block::Request_stream const &request_stream,
		                  Cbe::Request          const &creq,
		                  Cbe::Primitive        const &p)
		{
			using Payload = Block::Request_stream::Payload;

			Block_data *data { nullptr };
			request_stream.with_payload([&] (Payload const &payload) {

				Block::Request request { };
				request.offset = creq.offset + (p.index * BLOCK_SIZE);
				request.operation.count  = 1;

				payload.with_content(request, [&] (void *addr, Genode::size_t) {

					data = reinterpret_cast<Block_data*>(addr);
				});
			});

			return data;
		}

		uint64_t         _current_sb { ~0ull };
		Cbe::Super_block _super_block[Cbe::NUM_SUPER_BLOCKS] { };
		Cbe::Generation  _current_generation { 0 };
		Cbe::Generation  _last_secured_generation { 0 };
		uint32_t         _current_snapshot { 0 };
		uint32_t         _last_snapshot_id { 0 };
		bool             _need_to_sync { false };
		bool             _need_to_secure { false };
		bool             _snaps_dirty { false };

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

		bool _show_progress    { false };
		bool _show_if_progress { false };

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

				/*******************
				 ** Time handling **
				 *******************/

				Cbe::Time::Timestamp const diff_time = _time.timestamp() - _last_time;
				if (diff_time >= _sync_interval && !_need_to_sync) {

					// XXX move
					bool cache_dirty = false;
					for (size_t i = 0; i < _cache.cxx_cache_slots(); i++) {
						Cache_Index const idx = Cache_Index { .value = (uint32_t)i };
						if (_cache.dirty(idx)) {
							cache_dirty |= true;
							break;
						}
					}

					_cache_dirty = cache_dirty;

					if (_cache_dirty) {
						Genode::log("\033[93;44m", __func__, " SEAL current generation: ", _current_generation);
						_need_to_sync = true;
					} else {
						DBG("cache is not dirty, re-arm trigger");
						_last_time = _time.timestamp();
						_time.schedule_sync_timeout(_sync_interval);
					}
				}

				Cbe::Time::Timestamp const diff_secure_time = _time.timestamp() - _last_secure_time;
				if (diff_secure_time >= _secure_interval && !_need_to_secure) {

					if (_snaps_dirty) {
						Genode::log("\033[93;44m", __func__,
						            " SEAL current super-block: ", _current_sb);
						_need_to_secure = true;
					} else {
						DBG("no snapshots created, re-arm trigger");
						_last_secure_time = _time.timestamp();
					}
				}

				/***********************
				 **  Request handling **
				 ***********************/

				block_session.with_requests([&] (Block::Request request) {

					using namespace Genode;

					Cbe::Virtual_block_address const vba = request.operation.block_number;

					if (vba > _max_vba) {
						warning("reject request with out-of-range virtual block address ", vba);
						return Block_session_component::Response::REJECTED;
					}

					if (!_request_pool.request_acceptable()) {
						return Block_session_component::Response::RETRY;
					}

					if (!request.operation.valid()) {
						warning("reject invalid request for virtual block address ", vba);
						return Block_session_component::Response::REJECTED;
					}

					Cbe::Request req = convert_to(request);

					Number_of_primitives const num = _splitter.number_of_primitives(req);
					_request_pool.submit_request(req, num);

					if (_show_progress || _show_if_progress) {
						Genode::log("NEW request: ", req);
					}

					progress |= true;
					return Block_session_component::Response::ACCEPTED;
				});

				block_session.try_acknowledge([&] (Block_session_component::Ack &ack) {

					Cbe::Request const &req = _request_pool.peek_completed_request();
					if (!req.operation_defined()) { return; }

					_request_pool.drop_completed_request(req);

					Block::Request request = convert_from(req);
					ack.submit(request);

					if (_show_progress || _show_if_progress) {
						Genode::log("ACK request: ", req);
					}

					progress |= true;
				});

				/************************
				 ** Free-tree handling **
				 ************************/

				{
					Cbe::Super_block const &sb = _super_block[_current_sb];

					bool const ft_progress = _free_tree->execute(sb.snapshots,
					                                             _last_secured_generation,
					                                             _free_tree_trans_data,
					                                             _cache, _cache_data,
					                                             _free_tree_query_data,
					                                             _time);
					progress |= ft_progress;
					LOG_PROGRESS(ft_progress);
				}

				while (true) {

					Cbe::Primitive prim = _free_tree->peek_completed_primitive();
					if (!prim.valid()) { break; }

					if (prim.success == Cbe::Primitive::Success::FALSE) {

						DBG("allocating new blocks failed: ", _free_tree_retry_count);
						if (_free_tree_retry_count < FREE_TREE_RETRY_LIMIT) {

							Cbe::Super_block &sb = _super_block[_current_sb];
							uint32_t const current = sb.snapshots[_current_snapshot].id;
							if (_discard_snapshot(sb.snapshots, current)) {
								_free_tree_retry_count++;
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

						Free_tree::Write_back_data const &wb = _free_tree->peek_completed_wb_data(prim);

						_write_back.submit_primitive(wb.prim, wb.gen, wb.vba,
						                             wb.new_pba, wb.old_pba, wb.tree_height,
						                             *wb.block_data);
					}
					_free_tree->drop_completed_primitive(prim);
					progress |= true;
				}

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
						data = &_cache_data.item[idx.value];
						break;
					case Tag::IO_TAG:
						tag  = Tag::FREE_TREE_TAG_IO;
						data = &_free_tree_query_data.item[idx.value];
						break;
					default: break;
					}

					_io.submit_primitive(tag, prim, *data);

					_free_tree->drop_generated_primitive(prim);
					progress |= true;
				}

				/*******************************
				 ** Put request into splitter **
				 *******************************/

				while (true) {

					Cbe::Request const &req = _request_pool.peek_pending_request();
					if (!req.operation_defined()) { break; }

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

					if (_need_to_secure) {
						DBG("prevent processing new primitives while securing super-block");
						break;
					}

					_splitter.drop_generated_primitive(prim);

					Cbe::Super_block const &sb = _super_block[_current_sb];
					Cbe::Snapshot    const &snap = sb.snapshots[_current_snapshot];

					Cbe::Physical_block_address const  pba  = snap.pba;
					Cbe::Hash                   const &hash = snap.hash;
					Cbe::Generation             const  gen  = snap.gen;

					_vbd->submit_primitive(pba, gen, hash, prim);
					progress |= true;
				}

				/**************************
				 ** VBD handling **
				 **************************/

				bool const vbd_progress = _vbd->execute(_trans_data,
				                                        _cache, _cache_data,
				                                        _time);
				progress |= vbd_progress;
				LOG_PROGRESS(vbd_progress);

				while (true) {

					Cbe::Primitive prim = _vbd->peek_completed_primitive();
					if (!prim.valid()) { break; }

					Cbe::Request const creq = _request_pool.request_for_tag(prim.tag);
					Cbe::Block_data *data_ptr = _data(block_session, creq, prim);

					if (prim.read()) {
						if (!_io.primitive_acceptable()) { break; }

						_io.submit_primitive(Tag::CRYPTO_TAG_DECRYPT, prim, *data_ptr);
					} else

					if (prim.write()) {

						/* 1. check if Free_tree module is idle */
						if (!_free_tree->request_acceptable()) { break; }

						/*
						 * 2. get old PBA's from Translation module and mark volatile blocks
						 */

						Cbe::Type_1_node_info old_pba[Translation::MAX_LEVELS] { };
						if (!_vbd->trans_get_type_1_info(prim, old_pba)) {
							break;
						}

						Cbe::Super_block const &sb = _super_block[_current_sb];
						Cbe::Snapshot    const &snap = sb.snapshots[_current_snapshot];

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
							if (gen == _current_generation || gen == 0) {
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

						if (snap.gen == _current_generation || snap.gen == 0) {
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
							_free_tree->submit_request(_current_generation,
							                           new_blocks,
							                           new_pba, old_pba,
							                           trans_height,
							                           free_pba, free_blocks,
							                           prim, vba, *data_ptr);
						} else {
							DBG("UPDATE ALL IN PACE");
							/* ... or hand over infos to the Write_back module */
							_write_back.submit_primitive(prim, _current_generation, vba,
							                             new_pba, old_pba, trans_height, *data_ptr);
						}

						// XXX see _write_back.peek_completed_primitive()
						_vbd->trans_inhibit_translation();
					}

					_vbd->drop_completed_primitive(prim);
					progress |= true;
				}

				/**********************
				 ** Flusher handling **
				 **********************/

				while (true) {

					Cbe::Primitive prim = _flusher.peek_completed_primitive();
					if (!prim.valid()) { break; }

					Cbe::Physical_block_address const pba = prim.block_number;
					_cache.mark_clean(pba);
					DBG("mark_clean: ", pba);

					_flusher.drop_completed_primitive(prim);

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _flusher.peek_generated_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					Cache_Index     const  idx  = _flusher.peek_generated_data_index(prim);
					Cbe::Block_data       &data = _cache_data.item[idx.value];

					_io.submit_primitive(Tag::CACHE_FLUSH_TAG, prim, data);

					_flusher.drop_generated_primitive(prim);

					progress |= true;
				}

				/*************************
				 ** Write-back handling **
				 *************************/

				bool const write_back_progress = _write_back.execute();
				progress |= write_back_progress;
				LOG_PROGRESS(write_back_progress);

				while (true) {

					Cbe::Primitive prim = _write_back.peek_completed_primitive();
					if (!prim.valid()) { break; }
					if (!_flusher.request_acceptable()) { break; }

					bool cache_dirty = false;
					for (uint32_t i = 0; i < _cache.cxx_cache_slots(); i++) {
						Cache_Index const idx = Cache_Index { .value = (uint32_t)i };
						if (_cache.dirty(idx)) {
							cache_dirty |= true;

							Cbe::Physical_block_address const pba = _cache.flush(idx);
							DBG(" i: ", idx.value, " pba: ", pba, " needs flushing");

							_flusher.submit_request(pba, idx);
						}
					}

					if (cache_dirty) {
						DBG("CACHE FLUSH NEEDED: progress: ", progress);
						progress |= true;
						break;
					}

					if (_need_to_sync) {

						Cbe::Super_block &sb = _super_block[_current_sb];
						uint32_t next_snap = _current_snapshot;
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
						if (next_snap == _current_snapshot) {
							Genode::error("could not find free snapshot slot");
							break;
						}

						Cbe::Snapshot &snap = sb.snapshots[next_snap];
						snap.pba = _write_back.peek_completed_root(prim);
						Cbe::Hash *snap_hash = &snap.hash;
						_write_back.peek_competed_root_hash(prim, *snap_hash);
						Cbe::Tree_helper const &tree = _vbd->tree_helper();
						snap.height = tree.height();
						snap.leaves = tree.leafs();
						snap.gen = _current_generation;
						snap.id  = ++_last_snapshot_id;

						DBG("create new snapshot for generation: ", _current_generation,
						    " snap: ", snap);

						_current_generation++;
						_current_snapshot++;

						_need_to_sync = false;
						_last_time = _time.timestamp();
						_time.schedule_sync_timeout(_sync_interval);
					} else {
						Cbe::Super_block &sb = _super_block[_current_sb];
						Cbe::Snapshot    &snap = sb.snapshots[_current_snapshot];

						/* update snapshot */
						Cbe::Physical_block_address const pba = _write_back.peek_completed_root(prim);
						if (snap.pba != pba) {
							snap.gen = _current_generation;
							snap.pba = pba;
						}

						Cbe::Hash *snap_hash = &snap.hash;
						_write_back.peek_competed_root_hash(prim, *snap_hash);
					}

					_snaps_dirty |= true;

					_request_pool.mark_completed_primitive(prim);

					_write_back.drop_completed_primitive(prim);

					/*
					 * XXX stalling translation as long as the write-back takes places
					 *     is not a good idea
					 */
					_vbd->trans_resume_translation();

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _write_back.peek_generated_primitive();
					if (!prim.valid()) { break; }

					bool _progress = false;
					switch (prim.tag) {
					case Tag::CRYPTO_TAG_ENCRYPT:
						if (_crypto.primitive_acceptable()) {
							Cbe::Block_data &data = _write_back.peek_generated_crypto_data(prim);
							_crypto.submit_primitive(prim, data, _crypto_data);
							_progress = true;
							_write_back.drop_generated_primitive(prim);
						}
						break;
					case Tag::CACHE_TAG:
					{
						if (!_write_back.crypto_done()) {
							DBG("XXX stall write-back until crypto is done");
							break;
						}
						Cbe::Physical_block_address const pba        = prim.block_number;
						Cbe::Physical_block_address const update_pba = _write_back.peek_generated_pba(prim);

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

						DBG("cache hot pba: ", pba, " update_pba: ", update_pba);

						Cache_Index const idx        = _cache.data_index(pba, _time.timestamp());
						Cache_Index const update_idx = _cache.data_index(update_pba, _time.timestamp());

						Cbe::Block_data const &data        = _cache_data.item[idx.value];
						Cbe::Block_data       &update_data = _cache_data.item[update_idx.value];

						_write_back.update(pba, _vbd->tree_helper(), data, update_data);
						_cache.mark_dirty(update_pba);

						_write_back.drop_generated_primitive(prim);
						_progress = true;
						break;
					}
					default: break;
					}

					if (!_progress) { break; }

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _write_back.peek_generated_io_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					Cbe::Block_data &data = _write_back.peek_generated_io_data(prim);
					_io.submit_primitive(Tag::WRITE_BACK_TAG, prim, data);

					_write_back.drop_generated_io_primitive(prim);
					progress |= true;
				}

				/**************************
				 ** Super-block handling **
				 **************************/

				if (_need_to_secure && _sync_sb.primitive_acceptable()) {

					Cbe::Super_block &sb = _super_block[_current_sb];

					sb.last_secured_generation = _current_generation;
					sb.snapshot_id             = _current_snapshot;

					DBG("secure current super-block gen: ", _current_generation,
					    " snap_id: ", _current_snapshot);

					_sync_sb.submit_primitive(_current_sb, _current_generation);
				}

				bool const sync_sb_progress = _sync_sb.execute();
				progress |= sync_sb_progress;
				LOG_PROGRESS(sync_sb_progress);

				while (true) {

					Cbe::Primitive prim = _sync_sb.peek_completed_primitive();
					if (!prim.valid()) { break; }

					DBG("primitive: ", prim);

					_last_secured_generation = _sync_sb.peek_completed_generation(prim);

					uint32_t next_sb = (_current_sb + 1) % Cbe::NUM_SUPER_BLOCKS;
					Cbe::Super_block       &next = _super_block[next_sb];
					Cbe::Super_block const &curr = _super_block[_current_sb];
					Genode::memcpy(&next, &curr, sizeof (Cbe::Super_block));

					_current_sb = next_sb;

					_snaps_dirty = false;
					_need_to_secure = false;
					_last_secure_time = _time.timestamp();
					_time.schedule_secure_timeout(_secure_interval);

					_sync_sb.drop_completed_primitive(prim);
					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _sync_sb.peek_generated_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					uint64_t   const  id      = _sync_sb.peek_generated_id(prim);
					Cbe::Super_block &sb      = _super_block[id];
					Cbe::Block_data  &sb_data = *reinterpret_cast<Cbe::Block_data*>(&sb);

					_io.submit_primitive(Tag::SYNC_SB_TAG, prim, sb_data);
					_sync_sb.drop_generated_primitive(prim);
					progress |= true;
				}

				/*********************
				 ** Crypto handling **
				 *********************/

				bool const crypto_progress = _crypto.foobar();
				progress |= crypto_progress;
				LOG_PROGRESS(crypto_progress);

				while (true) {

					Cbe::Primitive prim = _crypto.peek_completed_primitive();
					if (!prim.valid()) { break; }

					_crypto.drop_completed_primitive(prim);
					if (prim.read()) {

						Cbe::Request const creq = _request_pool.request_for_tag(prim.tag);
						Cbe::Block_data *data_ptr = _data(block_session, creq, prim);
						_crypto.copy_completed_data(prim, *data_ptr);
						_request_pool.mark_completed_primitive(prim);
						DBG("pool complete: ", prim);

					} else if (prim.write()) {

						Cbe::Block_data &data = _write_back.peek_generated_crypto_data(prim);
						_crypto.copy_completed_data(prim, data);
						_write_back.mark_completed_crypto_primitive(prim);
					}

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _crypto.peek_generated_primitive();
					if (!prim.valid())     { break; }

					_crypto.drop_generated_primitive(prim);
					_crypto.mark_completed_primitive(prim);

					progress |= true;
				}

				/********************
				 ** Cache handling **
				 ********************/

				bool const cache_progress = _cache.execute(_cache_data, _cache_job_data,
				                                           _time.timestamp());
				progress |= cache_progress;
				LOG_PROGRESS(cache_progress);

				while (true) {

					Cbe::Primitive prim = _cache.peek_generated_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					Cache_Index const idx = _cache.peek_generated_data_index(prim);
					Cbe::Block_data &data = _cache_job_data.item[idx.value];

					_cache.drop_generated_primitive(prim);

					_io.submit_primitive(Tag::CACHE_TAG, prim, data);
					progress |= true;
				}

				/******************
				 ** I/O handling **
				 ******************/

				bool const io_progress = _io.execute();
				progress |= io_progress;
				LOG_PROGRESS(io_progress);

				while (true) {

					Cbe::Primitive prim = _io.peek_completed_primitive();
					if (!prim.valid()) { break; }

					bool _progress = true;
					switch (prim.tag) {
					case Tag::CRYPTO_TAG_DECRYPT:
						if (!_crypto.primitive_acceptable()) {
							_progress = false;
						} else {
							Cbe::Block_data   &data = _io.peek_completed_data(prim);
							Cbe::Tag const orig_tag = _io.peek_completed_tag(prim);

							prim.tag = orig_tag;
							_crypto.submit_primitive(prim, data, _crypto_data);
						}
						break;
					case Tag::CACHE_TAG:
						_cache.cxx_mark_completed_primitive(prim);
						break;
					case Tag::CACHE_FLUSH_TAG:
						_flusher.mark_generated_primitive_complete(prim);
						break;
					case Tag::WRITE_BACK_TAG:
						_write_back.mark_completed_io_primitive(prim);
						break;
					case Tag::SYNC_SB_TAG:
						_sync_sb.mark_generated_primitive_complete(prim);
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

				if (!progress) {
					if (_show_if_progress) {
						Genode::log("\033[33m", ">>> break, no progress");
					}
					break;
				}
			}

			block_session.wakeup_client_if_needed();
		}

		void _dump_current_sb_info() const
		{
			Cbe::Super_block const &sb = _super_block[_current_sb];
			Cbe::Snapshot    const &snap = sb.snapshots[_current_snapshot];

			Cbe::Physical_block_address const root_number = snap.pba;
			Cbe::Height                 const height      = snap.height;
			Cbe::Number_of_leaves       const leaves      = snap.leaves;

			Cbe::Degree                 const degree      = sb.degree;
			Cbe::Physical_block_address const free_number = sb.free_number;
			Cbe::Number_of_leaves       const free_leaves = sb.free_leaves;
			Cbe::Height                 const free_height = sb.free_height;

			Genode::log("Virtual block-device info in SB[", _current_sb, "]: ",
			            " SNAP[", _current_snapshot, "]: ",
			            "tree height: ", height, " ",
			            "edges per node: ", degree, " ",
			            "leaves: ", leaves, " ",
			            "root block address: ", root_number, " ",
			            "free block address: ", free_number, " ",
			            "free leaves: (", free_leaves, "/", free_height, ")"
			);
		}

		void _setup()
		{
			Cbe::Generation last_gen = 0;
			uint64_t most_recent_sb = ~0ull;

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
					most_recent_sb = i;
					last_gen = dst.last_secured_generation;
				}

				Sha256_4k::Hash hash { };
				Sha256_4k::Data const &data = *reinterpret_cast<Sha256_4k::Data const*>(&dst);
				Sha256_4k::hash(data, hash);
				Genode::log("SB[", i, "] hash: ", hash);
			}

			if (most_recent_sb == ~0ull) {
				Genode::error("no valid super block found");
				throw -1;
			}

			_current_sb = most_recent_sb;

			using SB = Cbe::Super_block;
			using SS = Cbe::Snapshot;

			SB const &sb = _super_block[_current_sb];

			uint32_t snap_slot = ~0u;
			for (uint32_t i = 0; i < Cbe::NUM_SNAPSHOTS; i++) {
				SS const &snap = sb.snapshots[i];
				if (!snap.valid()) { continue; }

				if (snap.id == sb.snapshot_id) {
					snap_slot = i;
					break;
				}
			}
			if (snap_slot == ~0u) {
				Genode::error("snapshot slot not found");
				throw -1;
			}

			_current_snapshot = snap_slot;

			SS const &snap = sb.snapshots[_current_snapshot];

			_last_snapshot_id = snap.id;

			Cbe::Degree           const degree = sb.degree;
			Cbe::Height           const height = snap.height;
			Cbe::Number_of_leaves const leaves = snap.leaves;

			_last_secured_generation = sb.last_secured_generation;
			_current_generation      = _last_secured_generation + 1;

			_max_vba = leaves - 1;

			if (height > Module::Translation::MAX_LEVELS) {
				Genode::error("tree height of ", height, " not supported");
				throw -1;
			}

			_vbd.construct(height, degree, leaves);

			Cbe::Physical_block_address const free_number = sb.free_number;
			Cbe::Generation             const free_gen    = sb.free_gen;
			Cbe::Hash                   const free_hash   = sb.free_hash;
			Cbe::Height const free_height                 = sb.free_height;
			Cbe::Degree const free_degree                 = sb.free_degree;
			Cbe::Number_of_leaves const free_leafs        = sb.free_leaves;

			_free_tree.construct(free_number, free_gen, free_hash, free_height,
			                     free_degree, free_leafs);
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env) : _env(env)
		{
			if (!_object_sizes_match) {
				error("object size mismatch");
			}

			if (_config_rom.valid()) {
				_show_progress =
					_config_rom.xml().attribute_value("show_progress", false);

				_sync_interval = 1000 *
					_config_rom.xml().attribute_value("sync_interval", (uint64_t)SYNC_INTERVAL);
				_secure_interval = 1000 *
					_config_rom.xml().attribute_value("sync_interval", (uint64_t)SECURE_INTERVAL);
			}

			_time.sync_sigh(_request_handler);
			_time.schedule_sync_timeout(_sync_interval);

			_time.secure_sigh(_request_handler);
			_time.schedule_secure_timeout(_secure_interval);

			_setup();
			_dump_current_sb_info();

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
			_block_session.construct(_env.rm(), _block_ds->cap(), _env.ep(),
			                         _request_handler, _max_vba + 1);

			_block.tx_channel()->sigh_ack_avail(_request_handler);
			_block.tx_channel()->sigh_ready_to_submit(_request_handler);

			_dump_current_sb_info();

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
