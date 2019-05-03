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
#include <util/bit_allocator.h>

/* repo includes */
#include <util/sha256_4k.h>

/* cbe includes */
#include <cbe/types.h>

/* local includes */
#include <util.h>


namespace Cbe {

	struct Time;

	enum {
		INVALID_TAG        = 0x00,
		IO_TAG             = 0x10,
		CACHE_TAG          = 0x20,
		CRYPTO_TAG         = 0x30,
		CRYPTO_TAG_DECRYPT = CRYPTO_TAG | 0x1,
		CRYPTO_TAG_ENCRYPT = CRYPTO_TAG | 0x2,
		POOL_TAG           = 0x40,
		SPLITTER_TAG       = 0x50,
		TRANSLATION_TAG    = 0x60,
		WRITE_BACK_TAG     = 0x70,
		SYNC_SB_TAG        = 0x80,
	};

	struct Block_session_component;
	struct Block_manager;
	struct Main;

} /* namespace Ceb */


struct Cbe::Block_session_component : Rpc_object<Block::Session>,
                                      Block::Request_stream
{
	Entrypoint &_ep;

	Block::sector_t _block_count;

	Block_session_component(Region_map               &rm,
	                        Dataspace_capability      ds,
	                        Entrypoint               &ep,
	                        Signal_context_capability sigh,
	                        Block::sector_t           block_count)
	:
		Request_stream(rm, ds, ep, sigh, BLOCK_SIZE), _ep(ep),
		_block_count(block_count)
	{
		_ep.manage(*this);
	}

	~Block_session_component() { _ep.dissolve(*this); }

	void info(Block::sector_t *count, size_t *block_size, Operations *ops) override
	{
		*count      = _block_count;
		*block_size = Cbe::BLOCK_SIZE;
		*ops        = Operations();

		ops->set_operation(Block::Packet_descriptor::Opcode::READ);
		ops->set_operation(Block::Packet_descriptor::Opcode::WRITE);
	}

	void sync() override { }

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

	Cbe::Physical_block_address alloc(size_t count)
	{
		for (size_t i = 0; i < ENTRIES; i++) {
			bool found = false;
			try { found = !_array.get(i, count); }
			catch (...) { }

			if (!found) { continue; }

			_array.set(i, count);
			_used += count;
			return _start + i;
		}

		struct Alloc_failed { };
		throw Alloc_failed();
	}

	void free(Cbe::Physical_block_address pba)
	{
		_array.clear(pba - _start, 1);
		_used--;
	}

	size_t used()  const { return _used; }
	size_t avail() const { return _size - _used; }
};


struct Cbe::Time
{
	Timestamp timestamp() const
	{
		uint32_t lo, hi;
		/* serialize first */
		__asm__ __volatile__ (
			"xorl %%eax,%%eax\n\t"
			"cpuid\n\t"
			:
			:
			: "%rax", "%rbx", "%rcx", "%rdx"
		);
		__asm__ __volatile__ (
			"rdtsc" : "=a" (lo), "=d" (hi)
		);
		return (uint64_t)hi << 32 | lo;
	}
};


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

class Cbe::Main : Rpc_object<Typed_root<Block::Session>>
{
	private:

		Env &_env;

		Attached_rom_dataspace _config_rom { _env, "config" };

		Constructible<Attached_ram_dataspace>  _block_ds { };
		Constructible<Block_session_component> _block_session { };

		Constructible<Block_manager> _block_manager { };

		/* back end Block session */
		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };
		Heap              _heap        { _env.ram(), _env.rm() };
		Allocator_avl     _block_alloc { &_heap };
		Block::Connection _block       { _env, &_block_alloc, TX_BUF_SIZE };

		/* modules */

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
		using Translation = Module::Translation<Cbe::Type_i_node>;

		using Cache          = Module::Cache<MAX_PRIM, CACHE_ENTRIES>;
		using Cache_Index    = Module::Cache<MAX_PRIM, CACHE_ENTRIES>::Index;
		using Cache_Data     = Module::Cache<MAX_PRIM, CACHE_ENTRIES>::Data;
		using Cache_Job_Data = Module::Cache<MAX_PRIM, CACHE_ENTRIES>::Job_Data;

		using Crypto      = Module::Crypto;
		using Io          = Module::Block_io<IO_ENTRIES, BLOCK_SIZE>;
		using Io_index    = Module::Block_io<IO_ENTRIES, BLOCK_SIZE>::Index;
		using Write_back       = Module::Write_back<MAX_LEVEL, Cbe::Type_i_node>;
		using Write_back_Index = Module::Write_back<MAX_LEVEL, Cbe::Type_i_node>::Index;
		using Sync_sb     = Module::Sync_sb;

		Pool     _request_pool { };
		Splitter _splitter     { };

		Time _time { };

		Cache          _cache          { };
		Cache_Data     _cache_data     { };
		Cache_Job_Data _cache_job_data { };

		Crypto   _crypto       { "All your base are belong to us  " };
		Block_data _crypto_data { };

		Io         _io                  { _block };
		Block_data _io_data[IO_ENTRIES] { };

		Constructible<Translation> _trans { };
		Bit_allocator<MAX_REQS> _tag_alloc { };

		Write_back _write_back { };
		Block_data _write_back_data[MAX_LEVEL] { };
		Sync_sb    _sync_sb { };

		Block_data* _data(Block::Request_stream::Payload const &payload,
		                  Pool const &pool, Primitive const p)
		{
			Block::Request const client_req = pool.request_for_tag(p.tag);
			Block::Request request { };
			request.offset = client_req.offset + (p.index * BLOCK_SIZE);
			request.count  = 1;

			Block_data *data { nullptr };
			payload.with_content(request, [&] (void *addr, Genode::size_t size) {

				if (size != BLOCK_SIZE) {
					Genode::error("content size and block size differ");
					return;
				}

				data = reinterpret_cast<Block_data*>(addr);
			});

			if (data == nullptr) {
				Genode::error("BUG: data_ptr is nullptr");
				Genode::sleep_forever();
			}

			return data;
		}

		uint64_t         _current_sb { ~0ull };
		Cbe::Super_block _super_block[Cbe::NUM_SUPER_BLOCKS] { };


		bool _show_progress { false };

		Signal_handler<Main> _request_handler {
			_env.ep(), *this, &Main::_handle_requests };

		void _handle_requests()
		{
			if (!_block_session.constructed()) { return; }

			Block_session_component &block_session = *_block_session;

			for (;;) {

				bool progress = false;

				/***********************
				 **  Request handling **
				 ***********************/

				block_session.with_requests([&] (Block::Request request) {

					using namespace Genode;

					if (request.block_number > _max_vba) {
						Cbe::Virtual_block_address const vba = request.block_number;
						warning("reject request with out-of-range virtual block address ", vba);

						return Block_session_component::Response::REJECTED;
					}

					if (!_request_pool.request_acceptable()) {
						return Block_session_component::Response::RETRY;
					}

					if (!request.operation_defined()) {
						Cbe::Virtual_block_address const vba = request.block_number;
						warning("reject invalid request for virtual block address ", vba);
						return Block_session_component::Response::REJECTED;
					}

					Number_of_primitives const num = _splitter.number_of_primitives(request);
					request.tag = _tag_alloc.alloc();
					_request_pool.submit_request(request, num);

					progress |= true;
					return Block_session_component::Response::ACCEPTED;
				});

				block_session.try_acknowledge([&] (Block_session_component::Ack &ack) {

					Block::Request const &req = _request_pool.peek_completed_request();
					if (!req.operation_defined()) { return; }

					_request_pool.drop_completed_request(req);
					_tag_alloc.free(req.tag);
					ack.submit(req);

					progress |= true;
				});

				/*******************************
				 ** Put request into splitter **
				 *******************************/

				while (true) {

					Block::Request const &req = _request_pool.peek_pending_request();
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

					_trans->submit_primitive(root, root_hash, p);
					progress |= true;
				}

				/**************************
				 ** Translation handling **
				 **************************/

				bool const trans_progress = _trans->execute();
				progress |= trans_progress;
				if (_show_progress) {
					Genode::log("Translation progress: ", trans_progress);
				}

				while (_trans->peek_generated_primitive()) {

					Cbe::Primitive p = _trans->take_generated_primitive();

					if (!_cache.data_available(p.block_number)) {

						if (_cache.request_acceptable(p.block_number)) {
							_cache.submit_request(p.block_number);
						}
						break;
					} else {

						Cache_Index     const idx   = _cache.data_index(p.block_number,
						                                                _time.timestamp());
						Cbe::Block_data const &data = _cache_data.item[idx.value];
						_trans->mark_generated_primitive_complete(p, data);
					}

					progress |= true;
				}

				while (_trans->peek_completed_primitive()) {

					Cbe::Primitive prim = _trans->take_completed_primitive();

					using Payload = Block::Request_stream::Payload;
					Cbe::Block_data *data_ptr = nullptr;
					block_session.with_payload([&] (Payload const &payload) {
						data_ptr = _data(payload, _request_pool, prim);
					});

					if (prim.read()) {
						if (!_io.primitive_acceptable()) { break; }
						_trans->dump();

						_io.submit_primitive(CRYPTO_TAG_DECRYPT, prim, *data_ptr);
					} else

					if (prim.write()) {

						Cbe::Primitive::Number n = prim.block_number;
						Genode::log("STEP 1: ", n);

						/*
						 * 1. check if Write_back module is idle
						 */
						if (!_write_back.primitive_acceptable()) { break; }

						Genode::log("STEP 2: ", n);

						/*
						 * 2. check if allocator has enough blocks left
						 */
						uint32_t const new_blocks = _trans->height() + 1;
						Cbe::Physical_block_address pba = 0;
						try { pba = _block_manager->alloc(new_blocks); }
						catch (...) { break; }

						Genode::log("STEP 3: ", n);

						/*
						 * 3. get old PBA's from Translation module
						 */
						Cbe::Physical_block_address old_pba[Translation::MAX_LEVELS] { };
						if (!_trans->get_physical_block_addresses(prim, old_pba, Translation::MAX_LEVELS)) {

							/* roll block allocation back */
							for (uint32_t i = 0; i < new_blocks; i++) {
								_block_manager->free(pba + i);
							}
							break;
						}

						Cbe::Primitive::Number const vba = _trans->get_virtual_block_address(prim);
						Genode::log("STEP 4: ", n, " vba: ", vba);

						/*
						 * 4. hand over infos to the Write_back module
						 */
						_write_back.submit_primitive(prim, vba, pba, old_pba, new_blocks, *data_ptr);

						// XXX see _write_back.peek_completed_primitive()
						_trans->suspend();
					}

					_trans->discard_completed_primitive(prim);
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
					_io.submit_primitive(CACHE_TAG, prim, data);

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
					if (!_sync_sb.primitive_acceptable()) { break; }

					Genode::log("Write_back peek_completed_primitive");

					{
						/* update super block */
						Cbe::Super_block const &last_sb = _super_block[_current_sb];

						uint64_t next_sb = (_current_sb + 1) % Cbe::NUM_SUPER_BLOCKS;
						Cbe::Super_block &sb = _super_block[next_sb];
						sb.root_number = _write_back.peek_completed_root(prim);
						Cbe::Hash *root_hash = &sb.root_hash;
						_write_back.peek_competed_root_hash(prim, *root_hash);

						sb.free_leaves = last_sb.free_leaves - _trans->height() + 1;
						sb.generation = last_sb.generation + 1;

						Cbe::Physical_block_address const last_root = last_sb.root_number;
						Cbe::Physical_block_address const root      = sb.root_number;
						Genode::error("last root: ", last_root, " root: ", root);

						Cbe::Super_block::Generation const gen = sb.generation;
						Genode::error("Submit sync super block: ", next_sb, " gen: ", gen);
						_sync_sb.submit_primitive(prim, next_sb);

						Genode::error("Switch super block: ", _current_sb, " -> ", next_sb);
						_current_sb = next_sb;
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
					case CRYPTO_TAG_ENCRYPT:
						Genode::log("Write_back CRYPTO_TAG_ENCRYPT");
						if (_crypto.primitive_acceptable()) {
							Cbe::Block_data &data = _write_back.peek_generated_data(prim);
							_crypto.submit_primitive(prim, data, _crypto_data);
							_progress = true;
						}
						break;
					case CACHE_TAG:
						Genode::log("Write_back CACHE_TAG");
						if (_cache.data_available(prim.block_number)) {

							Cache_Index     const idx   = _cache.data_index(prim.block_number,
							                                                _time.timestamp());
							Cbe::Block_data const &data = _cache_data.item[idx.value];
							_write_back.copy_and_update(prim, data, *_trans);
							_progress = true;
						} else {

							if (_cache.request_acceptable(prim.block_number)) {
								_cache.submit_request(prim.block_number);
							}
						}
						break;
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

					Cbe::Tag const tag = prim.tag;
					Cbe::Primitive::Number const number = prim.block_number;
					Genode::log("Write_back peek_generated_io_primitive: ", tag, " ", number);

					Cbe::Block_data &data = _write_back.peek_generated_io_data(prim);
					_io.submit_primitive(WRITE_BACK_TAG, prim, data);

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

					_sync_sb.drop_completed_primitive(prim);

					_request_pool.mark_completed_primitive(req_prim);

					progress |= true;
				}

				while (true) {

					Cbe::Primitive prim = _sync_sb.peek_generated_primitive();
					if (!prim.valid()) { break; }
					if (!_io.primitive_acceptable()) { break; }

					uint64_t   const  id      = _sync_sb.peek_generated_id(prim);
					Cbe::Super_block &sb      = _super_block[id];
					Cbe::Block_data  &sb_data = *reinterpret_cast<Cbe::Block_data*>(&sb);

					_io.submit_primitive(SYNC_SB_TAG, prim, sb_data);
					_sync_sb.drop_generated_primitive(prim);
					progress |= true;
				}

				/*********************
				 ** Crypto handling **
				 *********************/

				_crypto.execute();
				bool const crypto_progress = _crypto.execute_progress();
				progress |= crypto_progress;
				if (_show_progress) {
					Genode::log("Crypto progress: ", crypto_progress);
				}

				while (true) {

					Cbe::Primitive prim = _crypto.peek_completed_primitive();
					if (!prim.valid()) { break; }

					_crypto.drop_completed_primitive(prim);
					if (prim.read()) {

						using Payload = Block::Request_stream::Payload;
						Cbe::Block_data *data_ptr = nullptr;
						block_session.with_payload([&] (Payload const &payload) {
							data_ptr = _data(payload, _request_pool, prim);
						});
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
					case CRYPTO_TAG_DECRYPT:
						if (!_crypto.primitive_acceptable()) {
							_progress = false;
						} else {
							Cbe::Block_data   &data = _io.peek_completed_data(prim);
							Cbe::Tag const orig_tag = _io.peek_completed_tag(prim);

							prim.tag = orig_tag;
							_crypto.submit_primitive(prim, data, _crypto_data);
						}
						break;
					case CACHE_TAG:
						_cache.mark_completed_primitive(prim);
						break;
					case WRITE_BACK_TAG:
						Genode::log("_write_back.mark_completed_io_primitive");
						_write_back.mark_completed_io_primitive(prim);
						break;
					case SYNC_SB_TAG:
						Genode::log("SYNC_SB_TAG");
						_sync_sb.mark_generated_primitive_complete(prim);
						break;
					default: break;
					}
					if (!_progress) { break; }

					_io.drop_completed_primitive(prim);
					progress |= true;
				}

				if (!progress) { break; }
			}

			block_session.wakeup_client();
		}

		void _dump_current_sb_info() const
		{
			Cbe::Super_block const &sb = _super_block[_current_sb];

			Cbe::Super_block::Number const root_number      = sb.root_number;
			Cbe::Super_block::Height const height           = sb.height;
			Cbe::Super_block::Degree const degree           = sb.degree;
			Cbe::Super_block::Number_of_leaves const leaves = sb.leaves;

			Cbe::Super_block::Number const free_number           = sb.free_number;
			Cbe::Super_block::Number_of_leaves const free_leaves = sb.free_leaves;
			Cbe::Super_block::Number_of_leaves const free_height = sb.free_height;

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
			Cbe::Super_block::Generation last_gen = 0;
			uint64_t most_recent_sb = ~0ull;

			/*
			 * Read all super block slots and use the most recent one.
			 */
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				Util::Block_io io(_block, BLOCK_SIZE, i, 1);
				void const       *src = io.addr<void*>();
				Cbe::Super_block &dst = _super_block[i];
				Genode::memcpy(&dst, src, BLOCK_SIZE);

				if (dst.root_number != 0 && dst.generation > last_gen) {
					most_recent_sb = i;
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

			SB::Height const height           = sb.height;
			SB::Degree const degree           = sb.degree;
			SB::Number_of_leaves const leaves = sb.leaves;

			SB::Number const free_number           = sb.free_number;
			SB::Number_of_leaves const free_leaves = sb.free_leaves;

			_max_vba = leaves - 1;

			_trans.construct(height, degree);
			_block_manager.construct(free_number, free_leaves);
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env) : _env(env)
		{
			if (_config_rom.valid()) {
				_show_progress =
					_config_rom.xml().attribute_value("show_progress", false);
			}

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


Genode::Env *__genode_env;


void Component::construct(Genode::Env &env)
{
	/* make ada-runtime happy */
	__genode_env = &env;
	env.exec_static_constructors();

	static Cbe::Main inst(env);
}
