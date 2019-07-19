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
	struct Block_manager;
	struct Main;

} /* namespace Ceb */


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


/**********************************
 ** Temporary free block manager **
 **********************************/

#include <util/bit_array.h>

struct Cbe::Block_manager
{
	/* gives us 128MiB */
	enum { ENTRIES = 32u * 1024, };
	struct Entry /* Cbe::Type_ii_node */
	{
		bool reserved { false }; /* reserved => true */
		Cbe::Generation g_a { 0 };
		Cbe::Generation g_f { 0 };
	};
	Entry _entries[ENTRIES] { };
	Genode::Bit_array<ENTRIES> _array { };
	Genode::size_t             _used  { 0 };

	Cbe::Physical_block_address _start { 0 };
	Genode::size_t _size { 0 };

	Block_manager(Cbe::Physical_block_address start, size_t size)
	: _start(start), _size(size)
	{
		if (_size > ENTRIES) {
			Genode::warning("limit entries to ", (unsigned)ENTRIES);
			_size = ENTRIES;
		}

		Genode::log("Free Block manager entries: ", _size,
		            " start block address: ", _start);
	}

	void dump() const
	{

		for (Genode::size_t i = 0; i < ENTRIES; i++) {
			Entry const &e = _entries[i];
			if (!e.reserved) { continue; }

			Cbe::Generation const f_gen = e.g_f;
			Cbe::Generation const a_gen = e.g_a;

			Genode::log("\033[35;1m", __func__, ": ", i, ": (", _start + i, ") ", " a: ", a_gen, " f: ", f_gen);
		}
	}

	Cbe::Physical_block_address alloc(Cbe::Super_block const *sb,
	                                  Cbe::Generation const s_gen,
	                                  Cbe::Generation const curr_gen,
	                                  size_t count)
	{
		for (size_t i = 0; i < ENTRIES; i++) {
			bool found = false;
			try { found = !_array.get(i, count); }
			catch (...) { }

			if (!found) { continue; }

			bool free = true;
			for (size_t j = i; j < (i+count); j++) {
				Entry &e = _entries[j];
				if (!e.reserved) {
					continue;
				} else {
					Cbe::Generation const f_gen = e.g_f;
					Cbe::Generation const a_gen = e.g_a;

					if (f_gen <= s_gen) {

						bool in_use = false;
						for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
							Cbe::Super_block const &b = sb[i];
							if (!b.active) {
								continue;
							}
							Cbe::Generation const b_gen = b.generation;

							bool const is_free = ((f_gen <= s_gen) && (f_gen <= b_gen || a_gen >= (b_gen + 1)));

							in_use |= !is_free;
						}

						if (in_use) {
							free = false;
							break;
						}
						Genode::error("REUSE PBA: ", _start + j, " f: ", f_gen, " a: ", a_gen);
					} else {
						free = false;
						break;
					}
				}
			}
			if (!free) { continue; }

			_array.set(i, count);
			_used += count;

			for (size_t j = i; j < (i+count); j++) {
				Entry &e = _entries[j];
				e.reserved = true;
				e.g_a = curr_gen;
				e.g_f = 0;
			}
			Cbe::Physical_block_address const pba = _start + i;
			return pba;
		}

		struct Alloc_failed { };
		throw Alloc_failed();
	}

	void free(Cbe::Generation const curr_gen,
	          Cbe::Physical_block_address pba)
	{
		if (pba < _start) {
			Genode::warning("attempt to free invalid pba: ", pba);
			return;
		}

		size_t const i = pba - _start;

		Entry &e = _entries[i];
		e.g_f = curr_gen;

		try {
			_array.clear(i, 1);
			_used--;
		} catch (...) { Genode::warning(i, " already cleared"); }
	}
};


struct Cbe::Time
{
	Timer::Connection _timer;

	using Timestamp = Genode::uint64_t;

	Time(Genode::Env &env)
	: _timer(env) { }

	Timestamp timestamp() const
	{
		return _timer.elapsed_ms();
	}

	void sigh(Genode::Signal_context_capability cap)
	{
		_timer.sigh(cap);
	}

	void trigger(uint64_t msec)
	{
		_timer.trigger_once(msec * 1000);
	}
};


/*************
 ** modules **
 *************/

#define MDBG(mod, ...) do { Genode::log("\033[36m" #mod "> ", __VA_ARGS__); } while (0)

#include <cache_module.h>
#include <crypto_module.h>
#include <io_module.h>
#include <request_pool_module.h>
#include <splitter_module.h>
#include <translation_module.h>
#include <write_through_cache_module.h>
#include <write_back_module.h>
#include <sync_sb_module.h>
#include <reclaim_module.h>

#include "free_tree.h"

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
			MAX_PRIM      = 1,
			MAX_LEVEL     = 6,
			CACHE_ENTRIES = 16,
			IO_ENTRIES    = 1,
		};

		Cbe::Virtual_block_address _max_vba { 0 };

		using Pool        = Module::Request_pool;
		using Splitter    = Module::Splitter;
		using Translation = Module::Translation;
		using Translation_Data = Module::Translation_Data;

		using Cache          = Module::Cache;
		using Cache_Index    = Module::Cache_Index;
		using Cache_Data     = Module::Cache_Data;
		using Cache_Job_Data = Module::Cache_Job_Data;

		using Crypto      = Module::Crypto;
		using Io          = Module::Block_io<IO_ENTRIES, BLOCK_SIZE>;
		using Io_index    = Module::Block_io<IO_ENTRIES, BLOCK_SIZE>::Index;
		using Write_back       = Module::Write_back<MAX_LEVEL, Cbe::Type_i_node>;
		using Write_back_Index = Module::Write_back<MAX_LEVEL, Cbe::Type_i_node>::Index;
		using Sync_sb     = Module::Sync_sb;

		using Reclaim       = Module::Reclaim;
		using Reclaim_Index = Module::Reclaim_Index;
		using Reclaim_Data  = Module::Reclaim_Data;

		Pool     _request_pool { };
		Splitter _splitter     { };

		Time _time { _env };
		enum { SYNC_INTERVAL = 0ull, };
		uint64_t _sync_interval { SYNC_INTERVAL };
		Cbe::Time::Timestamp _last_time { _time.timestamp() };

		Cache          _cache          { };
		Cache_Data     _cache_data     { };
		Cache_Job_Data _cache_job_data { };

		Crypto   _crypto       { "All your base are belong to us  " };
		Block_data _crypto_data { };

		Io         _io                  { _block };
		Block_data _io_data[IO_ENTRIES] { };

		Constructible<Tree_helper> _trans_helper { };
		Constructible<Translation> _trans { };
		Translation_Data           _trans_data { };
		Bit_allocator<MAX_REQS> _tag_alloc { };

		Write_back _write_back { };
		Block_data _write_back_data[MAX_LEVEL] { };
		Sync_sb    _sync_sb { };

		Reclaim      _reclaim { };
		Reclaim_Data _reclaim_data { };

		Constructible<Cbe::Free_tree> _free_tree { };
		Translation_Data         _free_tree_trans_data { };
		Cache_Data               _free_tree_cache_data { };
		Cache_Job_Data           _free_tree_cache_job_data { };
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

				payload.with_content(request, [&] (void *addr, Genode::size_t size) {

					if (size != BLOCK_SIZE) {
						Genode::error("content size and block size differ");
						return;
					}

					data = reinterpret_cast<Block_data*>(addr);
				});
			});

			if (data == nullptr) {
				Genode::error("BUG: data_ptr is nullptr");
				Genode::sleep_forever();
			}

			return data;
		}

		uint64_t         _current_sb { ~0ull };
		Cbe::Super_block _super_block[Cbe::NUM_SUPER_BLOCKS] { };
		Cbe::Generation  _current_generation { 0 };
		Cbe::Generation  _last_secured_generation    { 0 };
		uint64_t         _sync_cb_count { 0 };
		bool             _need_to_sync { false };


		bool _show_progress { false };

		Signal_handler<Main> _request_handler {
			_env.ep(), *this, &Main::_handle_requests };

		void _handle_requests()
		{
			if (!_block_session.constructed()) { return; }

			Block_session_component &block_session = *_block_session;

			for (;;) {

				bool progress = false;

				/*******************
				 ** Time handling **
				 *******************/

				Cbe::Time::Timestamp const curr_time = _time.timestamp();
				Cbe::Time::Timestamp const diff_time = curr_time - _last_time;
				if (diff_time >= _sync_interval && !_need_to_sync) {
					Genode::log("\033[93;44m", __func__, " seal current ", _current_generation, " generation");
					_need_to_sync = true;
				}


				/************************
				 ** Free-tree handling **
				 ************************/

				bool const ft_progress = _free_tree->execute(_free_tree_trans_data,
				                                             _free_tree_cache_data,
				                                             _free_tree_cache_job_data,
				                                             _free_tree_query_data,
				                                             _time);
				progress |= ft_progress;
				if (_show_progress) {
					Genode::log("Free-tree progress: ", ft_progress);
				}

				while (true) {

					Cbe::Primitive prim = _free_tree->peek_completed_primitive();
					if (!prim.valid()) { break; }
					if (!_write_back.primitive_acceptable()) { break; }

					Free_tree::Write_back_data const &wb = _free_tree->peek_completed_wb_data(prim);

					_write_back.submit_primitive(wb.prim, wb.gen, wb.vba,
					                             wb.new_pba, wb.old_pba, wb.tree_height,
					                             *wb.block_data);

					MDBG(FT, __func__, ":", __LINE__, " drop_completed_primitive");
					_free_tree->drop_completed_primitive(prim);
					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _free_tree->peek_generated_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					Index const idx = _free_tree->peek_generated_data_index(prim);
					if (idx.value == ~0u) {
						Genode::error("BUG: invalid data index");
						throw -1;
					}

					Cbe::Block_data *data = nullptr;
					Cbe::Tag tag { Tag::INVALID_TAG };
					switch (prim.tag) {
					case Tag::CACHE_TAG:
						tag = Tag::FREE_TREE_TAG_CACHE;
						data = &_free_tree_cache_job_data.item[idx.value];
						break;
					case Tag::WRITE_BACK_TAG: tag = Tag::FREE_TREE_TAG_WB;    break;
					case Tag::IO_TAG:         tag = Tag::FREE_TREE_TAG_IO;
						data = &_free_tree_query_data.item[idx.value];
						break;
					default: break;
					}

					_io.submit_primitive(tag, prim, *data);

					_free_tree->drop_generated_primitive(prim);
					progress |= true;
				}

				/***********************
				 **  Request handling **
				 ***********************/

				block_session.with_requests([&] (Block::Request request) {

					using namespace Genode;

					if (request.operation.block_number > _max_vba) {
						Cbe::Virtual_block_address const vba = request.operation.block_number;
						warning("reject request with out-of-range virtual block address ", vba);

						return Block_session_component::Response::REJECTED;
					}

					if (!_request_pool.request_acceptable()) {
						return Block_session_component::Response::RETRY;
					}

					if (!request.operation.valid()) {
						Cbe::Virtual_block_address const vba = request.operation.block_number;
						warning("reject invalid request for virtual block address ", vba);
						return Block_session_component::Response::REJECTED;
					}

					Cbe::Request req = convert_to(request);
					req.tag = _tag_alloc.alloc();

					Number_of_primitives const num = _splitter.number_of_primitives(req);
					_request_pool.submit_request(req, num);

					progress |= true;
					return Block_session_component::Response::ACCEPTED;
				});

				block_session.try_acknowledge([&] (Block_session_component::Ack &ack) {

					Cbe::Request const &req = _request_pool.peek_completed_request();
					if (!req.operation_defined()) { return; }

					_request_pool.drop_completed_request(req);
					_tag_alloc.free(req.tag);

					Block::Request request = convert_from(req);
					ack.submit(request);

					progress |= true;
				});

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

				while (_splitter.peek_generated_primitive().valid()) {

					if (!_trans->acceptable()) { break; }

					Cbe::Primitive p = _splitter.peek_generated_primitive();
					_splitter.drop_generated_primitive(p);

					Cbe::Physical_block_address root = _super_block[_current_sb].root_number;
					Cbe::Hash const &root_hash = _super_block[_current_sb].root_hash;
					Cbe::Generation const root_gen = _super_block[_current_sb].root_gen;

					_trans->submit_primitive(root, root_gen, root_hash, p);
					progress |= true;
				}

				/**************************
				 ** Translation handling **
				 **************************/

				bool const trans_progress = _trans->execute(_trans_data);
				progress |= trans_progress;
				if (_show_progress) {
					Genode::log("Translation progress: ", trans_progress);
				}

				while (true) {
					Cbe::Primitive p = _trans->peek_generated_primitive();
					if (!p.valid()) { break; }

					Cbe::Physical_block_address const pba = p.block_number;
					if (!_cache.data_available(pba)) {

						if (_cache.request_acceptable(pba)) {
							_cache.submit_request(pba);
						}
						break;
					} else {

						Cache_Index     const idx   = _cache.data_index(pba,
						                                                _time.timestamp());
						Cbe::Block_data const &data = _cache_data.item[idx.value];
						_trans->mark_generated_primitive_complete(p, data, _trans_data);

						_trans->discard_generated_primitive(p);
					}

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _trans->peek_completed_primitive();
					if (!prim.valid()) { break; }

					// XXX defer all operations for now
					// if (_need_to_sync) { break; }

					Cbe::Request const creq = _request_pool.request_for_tag(prim.tag);
					Cbe::Block_data *data_ptr = _data(block_session, creq, prim);

					if (prim.read()) {
						if (!_io.primitive_acceptable()) { break; }

						Cbe::Physical_block_address const pba = prim.block_number;
						Genode::error("_trans->peek_completed_primitive(): pba: ", pba);
						_io.submit_primitive(Tag::CRYPTO_TAG_DECRYPT, prim, *data_ptr);
					} else

					if (prim.write()) {

						/* 1. check if Free_tree module is idle */
						if (!_free_tree->request_acceptable()) { break; }

						/*
						 * 2. get old PBA's from Translation module and mark volatile blocks
						 */
						// Cbe::Physical_block_address old_pba[Translation::MAX_LEVELS] { };
						// if (!_trans->get_physical_block_addresses(prim, old_pba, Translation::MAX_LEVELS)) {
						// 	break;
						// }

						Cbe::Type_1_node_info old_pba[Translation::MAX_LEVELS] { };
						if (!_trans->get_type_1_info(prim, old_pba, Translation::MAX_LEVELS)) {
							break;
						}

						Cbe::Primitive::Number const vba = _trans->get_virtual_block_address(prim);

						Cbe::Physical_block_address new_pba[Translation::MAX_LEVELS] { };
						Genode::memset(new_pba, 0, sizeof(new_pba));

						uint32_t const trans_height = _trans->height() + 1;
						uint32_t new_blocks = 0;

						Cbe::Physical_block_address free_pba[Translation::MAX_LEVELS] { };
						uint32_t free_blocks = 0;

						for (uint32_t i = 1; i < trans_height; i++) {
							Cbe::Physical_block_address const pba = old_pba[i].pba;
							Cache_Index     const idx   = _cache.data_index(pba, _time.timestamp());
							Cbe::Block_data const &data = _cache_data.item[idx.value];

							uint32_t const id = _trans->index(vba, i);
							Cbe::Type_i_node const *n = reinterpret_cast<Cbe::Type_i_node const*>(&data);

							uint64_t const gen = (n[id].gen & GEN_VALUE_MASK);
							if (gen == _current_generation || gen == 0) {
								Cbe::Physical_block_address const npba = n[id].pba;
								Genode::error("in place pba: ", pba, " gen: ", gen, " npba: ", npba);

								new_pba[i-1] = old_pba[i-1].pba;
								continue;
							}

							free_pba[free_blocks++] = old_pba[i-1].pba;

							new_blocks++;
						}

						// assert
						if (old_pba[trans_height-1].pba != _super_block[_current_sb].root_number) {
							Genode::error("BUG");
						}

						Cbe::Super_block const &sb = _super_block[_current_sb];
						Cbe::Generation const sb_gen = sb.generation;
						Genode::error("sb_gen: ", sb_gen, " _current_generation: ", _current_generation);
						if (sb.generation == _current_generation || sb.generation == 0) {
							new_pba[trans_height-1] = old_pba[trans_height-1].pba;
						} else {
							free_pba[free_blocks++] = old_pba[trans_height-1].pba;
							new_blocks++;
						}

						Genode::error("new_blocks: ", new_blocks);
						for (uint32_t i = 0; i < trans_height; i++) {
							Genode::error("new_pba[", i, "] = ", new_pba[i]);
						}

						/* 3. submit list blocks to free tree module... */
						if (new_blocks) {
							_free_tree->submit_request(_super_block,
							                           _last_secured_generation,
							                           _current_generation,
							                           new_blocks,
							                           new_pba, old_pba,
							                           trans_height,
							                           free_pba, free_blocks,
							                           prim, vba, *data_ptr);
						} else {

							/* ... or hand over infos to the Write_back module */
							_write_back.submit_primitive(prim, _current_generation, vba,
							                             new_pba, old_pba, trans_height, *data_ptr);
						}

						// XXX see _write_back.peek_completed_primitive()
						_trans->suspend();
					}

					_trans->drop_completed_primitive(prim);
					progress |= true;
				}

				/********************
				 ** Cache handling **
				 ********************/

				bool const cache_progress = _cache.execute(_cache_data, _cache_job_data,
				                                           _time.timestamp());
				progress |= cache_progress;
				if (_show_progress) {
					Genode::log("Cache progress: ", cache_progress);
				}

				while (true) {

					Cbe::Primitive prim = _cache.peek_generated_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					Cache_Index const idx = _cache.peek_generated_data_index(prim);
					Cbe::Block_data &data = _cache_job_data.item[idx.value];

					_cache.drop_generated_primitive(prim);
					Cbe::Physical_block_address const pba = prim.block_number;
					Genode::error("_cache.peek_generated_primitive(): pba: ", pba);
					_io.submit_primitive(Tag::CACHE_TAG, prim, data);

					progress |= true;
				}

				/*************************
				 ** Write-back handling **
				 *************************/

				bool const write_back_progress = _write_back.execute();
				progress |= write_back_progress;
				if (_show_progress) {
					Genode::log("Write-back progress: ", write_back_progress);
				}

				while (true) {

					Cbe::Primitive prim = _write_back.peek_completed_primitive();
					if (!prim.valid()) { break; }

					Genode::error("SYNC SUPER-BLOCK currently BROKEN!!!!");

					if (_need_to_sync) {

						if (!_sync_sb.primitive_acceptable()) { break; }

						uint64_t         const  next_sb = (_current_sb + 1) % Cbe::NUM_SUPER_BLOCKS;
						Cbe::Super_block const &last_sb = _super_block[_current_sb];
						Cbe::Super_block       &sb      = _super_block[next_sb];

						/* update super block */
						sb.root_number = _write_back.peek_completed_root(prim);
						Cbe::Hash *root_hash = &sb.root_hash;
						_write_back.peek_competed_root_hash(prim, *root_hash);

						sb.free_leaves = last_sb.free_leaves - _trans->height() + 1;
						sb.generation = _current_generation;

						sb.active = true;

						_sync_sb.submit_primitive(prim, next_sb, _current_generation);

						_current_sb = next_sb;
						_current_generation = _current_generation + 1;

						_need_to_sync = false;
						_last_time = curr_time;
						_time.trigger(_sync_interval);
					} else {

						Cbe::Super_block &sb = _super_block[_current_sb];
						Cbe::Physical_block_address const sb_root_number = sb.root_number;

						/* update super block */
						Cbe::Physical_block_address const root_number = _write_back.peek_completed_root(prim);
						if (sb_root_number != root_number) {
							sb.generation  = _current_generation;
						}
						sb.root_number = root_number;
						Cbe::Hash *root_hash = &sb.root_hash;
						_write_back.peek_competed_root_hash(prim, *root_hash);

						// XXX wrong
						sb.free_leaves = sb.free_leaves - _trans->height() + 1;

						// XXX for now we re-use the sync path to trigger the request ack
						//     in the request pool as well as tree dumping within the
						//     cbe_block
						_sync_sb.submit_primitive(prim, _current_sb, _current_generation);
					}

					_write_back.drop_completed_primitive(prim);

					/*
					 * XXX stalling translation as long as the write-back takes places
					 *     is not a good idea
					 */
					_trans->resume();

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _write_back.peek_generated_primitive();
					if (!prim.valid()) { break; }

					bool _progress = false;
					switch (prim.tag) {
					case Tag::CRYPTO_TAG_ENCRYPT:
						if (_crypto.primitive_acceptable()) {
							Cbe::Block_data &data = _write_back.peek_generated_data(prim);
							_crypto.submit_primitive(prim, data, _crypto_data);
							_progress = true;
						}
						break;
					case Tag::CACHE_TAG:
					{
						Cbe::Physical_block_address const pba = prim.block_number;
						if (_cache.data_available(pba)) {

							Cache_Index     const idx   = _cache.data_index(pba,
							                                                _time.timestamp());
							Cbe::Block_data const &data = _cache_data.item[idx.value];
							if (_write_back.copy_and_update(pba, data, *_trans)) {
								Genode::error("updated cached entry, cache invalidate: ", pba);
								_cache.invalidate(pba);
							}

							_progress = true;
						} else {

							if (_cache.request_acceptable(pba)) {
								_cache.submit_request(pba);
							}
						}
						break;
					}
					default: break;
					}

					if (!_progress) { break; }

					_write_back.drop_generated_primitive(prim);
					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _write_back.peek_generated_io_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					Cbe::Block_data &data = _write_back.peek_generated_io_data(prim);
					Cbe::Physical_block_address const pba = prim.block_number;
					Genode::error("_write_back.peek_generated_io_primitive(): pba: ", pba);
					_io.submit_primitive(Tag::WRITE_BACK_TAG, prim, data);

					_write_back.drop_generated_io_primitive(prim);
					progress |= true;
				}

				/**************************
				 ** Super-block handling **
				 **************************/

				bool const sync_sb_progress = _sync_sb.execute();
				progress |= sync_sb_progress;
				if (_show_progress) {
					Genode::log("Sync-sb progress: ", sync_sb_progress);
				}

				while (true) {

					Cbe::Primitive prim = _sync_sb.peek_completed_primitive();
					if (!prim.valid()) { break; }

					Cbe::Primitive req_prim = _sync_sb.peek_completed_request_primitive(prim);
					if (!req_prim.valid()) { Genode::error("BUG"); }

					_last_secured_generation = _sync_sb.peek_completed_generation(prim);

					_sync_sb.drop_completed_primitive(prim);

					_request_pool.mark_completed_primitive(req_prim);

					_sync_cb_count++;

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _sync_sb.peek_generated_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					uint64_t   const  id      = _sync_sb.peek_generated_id(prim);
					Cbe::Super_block &sb      = _super_block[id];
					Cbe::Block_data  &sb_data = *reinterpret_cast<Cbe::Block_data*>(&sb);

					Cbe::Physical_block_address const pba = prim.block_number;
					Genode::error("_sync_sb.peek_generated_primitive(): pba: ", pba);
					_io.submit_primitive(Tag::SYNC_SB_TAG, prim, sb_data);
					_sync_sb.drop_generated_primitive(prim);
					progress |= true;
				}

				/**********************
				 ** reclaim handling **
				 **********************/

				bool reclaim_progress = _reclaim.execute(_reclaim_data);
				progress |= reclaim_progress;
				if (_show_progress) {
					Genode::log("Reclaim progress: ", reclaim_progress);
				}

				while (true) {

					if (!_reclaim.peek_completed_request()) { break; }

					// _reclaim.reclaim_completed_request(*_block_manager, _current_generation);
					_reclaim.drop_completed_request();
					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _reclaim.peek_generated_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					Reclaim_Index   const idx   = _reclaim.peek_generated_data_index(prim);
					Cbe::Block_data &data = _reclaim_data.item[idx.value];

					Genode::log("Reclaim I/O idx: ", idx.value, " ", (void*)&data);

					Cbe::Physical_block_address const pba = prim.block_number;
					Genode::error("_reclaim.peek_generated_primitive(): pba: ", pba);
					_io.submit_primitive(Tag::RECLAIM_TAG, prim, data);
					_reclaim.drop_generated_primitive(prim);

					progress |= true;
				}

				/*********************
				 ** Crypto handling **
				 *********************/

				bool const crypto_progress = _crypto.foobar();
				progress |= crypto_progress;
				if (_show_progress) {
					Genode::log("Crypto progress: ", crypto_progress);
				}

				while (true) {

					Cbe::Primitive prim = _crypto.peek_completed_primitive();
					if (!prim.valid()) { break; }

					_crypto.drop_completed_primitive(prim);
					if (prim.read()) {

						Cbe::Request const creq = _request_pool.request_for_tag(prim.tag);
						Cbe::Block_data *data_ptr = _data(block_session, creq, prim);
						_crypto.copy_completed_data(prim, *data_ptr);
						_request_pool.mark_completed_primitive(prim);

					} else if (prim.write()) {

						Cbe::Block_data &data = _write_back.peek_generated_data(prim);
						_crypto.copy_completed_data(prim, data);
						_write_back.mark_completed_crypto_primitive(prim);
					}

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _crypto.peek_generated_primitive();
					if (!prim.valid())     { break; }
					if (!prim.read()) {
						Genode::error("BUG: got wrong primitive");
						throw -12346;
					}

					_crypto.drop_generated_primitive(prim);
					_crypto.mark_completed_primitive(prim);

					progress |= true;
				}

				/******************
				 ** I/O handling **
				 ******************/

				bool const io_progress = _io.execute();
				progress |= io_progress;
				if (_show_progress) {
					Genode::log("Io progress: ", io_progress);
				}

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
						_cache.mark_completed_primitive(prim);
						break;
					case Tag::WRITE_BACK_TAG:
						_write_back.mark_completed_io_primitive(prim);
						break;
					case Tag::SYNC_SB_TAG:
						_sync_sb.mark_generated_primitive_complete(prim);
						break;
					case Tag::RECLAIM_TAG:
						_reclaim.mark_generated_primitive_complete(prim);
						break;
					// XXX check for FREE_TREE_TAG
					case Tag::FREE_TREE_TAG_CACHE:
						prim.tag = Tag::CACHE_TAG;
						_free_tree->mark_generated_primitive_complete(prim);
						break;
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

				if (!progress) { break; }
			}

			block_session.wakeup_client_if_needed();
		}

		void _dump_current_sb_info() const
		{
			Cbe::Super_block const &sb = _super_block[_current_sb];

			Cbe::Physical_block_address const root_number      = sb.root_number;
			Cbe::Height const height           = sb.height;
			Cbe::Degree const degree           = sb.degree;
			Cbe::Number_of_leaves const leaves = sb.leaves;

			Cbe::Physical_block_address const free_number           = sb.free_number;
			Cbe::Number_of_leaves const free_leaves = sb.free_leaves;
			Cbe::Number_of_leaves const free_height = sb.free_height;

			Genode::log("Virtual block-device info in SB[", _current_sb, "]: ",
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
				if (dst.root_number != 0 && dst.generation >= last_gen) {
					most_recent_sb = i;
					last_gen = dst.generation;
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

			SB const &sb = _super_block[_current_sb];

			Cbe::Height const height           = sb.height;
			Cbe::Degree const degree           = sb.degree;
			Cbe::Number_of_leaves const leaves = sb.leaves;

			_last_secured_generation = sb.generation;
			_current_generation      = _last_secured_generation + 1;

			_max_vba = leaves - 1;

			_trans_helper.construct(degree, height);
			_trans.construct(*_trans_helper, false);

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
			}

			_time.sigh(_request_handler);
			_time.trigger(_sync_interval);

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
