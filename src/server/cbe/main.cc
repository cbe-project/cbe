/*
 * \brief  CBE C++ prototype
 * \author Josef Soentgen
 * \date   2019-01-21
 */

/*
 * Copyright (C) 2019 Genode Labs GmbH
 *
 * This file is part of the Genode OS framework, which is distributed
 * under the terms of the GNU Affero General Public License version 3.
 */

/* Genode includes */
#include <base/allocator_avl.h>
#include <base/attached_ram_dataspace.h>
#include <base/component.h>
#include <base/heap.h>
#include <block/request_stream.h>
#include <root/root.h>
#include <util/bit_allocator.h>

/* cbe include */
#include <cbe/types.h>

/* local includes */
#include <util.h>


namespace Cbe {

	struct Block_session_component;
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


class Cbe::Main : Rpc_object<Typed_root<Block::Session>>
{
	private:

		Env &_env;

		Constructible<Attached_ram_dataspace>  _block_ds { };
		Constructible<Block_session_component> _block_session { };

		/* back end Block session */
		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };
		Heap              _heap        { _env.ram(), _env.rm() };
		Allocator_avl     _block_alloc { &_heap };
		Block::Connection _block       { _env, &_block_alloc, TX_BUF_SIZE };

		/* modules */

		enum {
			MAX_REQS      = 16,
			MAX_PRIM      = 1,
			CACHE_ENTRIES = 16,
			MAX_LEVEL     = 6,
		};

		enum {
			BLOCK_TAG       = 1,
			CACHE_TAG       = 2,
			CRYPTO_TAG      = 3,
			POOL_TAG        = 4,
			SPLITTER_TAG    = 5,
			TRANSLATION_TAG = 6,
		};

		Cbe::Virtual_block_address _max_vba { 0 };

		using Pool        = Module::Request_pool;
		using Splitter    = Module::Splitter;
		using Translation = Module::Translation<Cbe::Type_i_node>;
		using Cache       = Module::Cache<MAX_PRIM, CACHE_ENTRIES>;
		using Crypto      = Module::Crypto;
		using Block_io    = Module::Block_io<MAX_PRIM, BLOCK_SIZE>;

		Pool     _request_pool { };
		Splitter _splitter     { };
		Cache    _cache        { };
		Crypto   _crypto       { "All your base are belong to us  ", sizeof(Crypto), sizeof(Cbe::Block_data), sizeof(Primitive)};
		Block_io _io           { _block };
		Constructible<Translation> _trans { };
		Bit_allocator<MAX_REQS> _tag_alloc { };

		Block_data* _data_for_primitive(Block::Request_stream::Payload const &payload,
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

			return data;
		}

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

					_trans->submit_primitive(p);

					progress |= true;
				}

				/**************************
				 ** Translation handling **
				 **************************/

				progress |= _trans->execute();

				while (_trans->peek_generated_primitive()) {

					Cbe::Primitive p = _trans->take_generated_primitive();

					if (!_cache.available(p)) {

						if (_cache.acceptable(p)) { _cache.submit_primitive(p); }
						break;
					} else {

						Cbe::Block_data const &data = _cache.data(p);
						_trans->mark_generated_primitive_complete(p, data);
					}

					progress |= true;
				}

				while (_trans->peek_completed_primitive()) {

					Cbe::Primitive prim = _trans->take_completed_primitive();

					if (!_crypto.primitive_acceptable()) { break; }

					_trans->discard_completed_primitive(prim);

					block_session.with_payload([&] (Block::Request_stream::Payload const &payload) {

						Block_data *data_ptr = _data_for_primitive(payload, _request_pool, prim);
						if (data_ptr == nullptr) {
							Genode::error("BUG: data_ptr is nullptr");
							Genode::sleep_forever();
						}

						_crypto.submit_primitive(prim, *data_ptr);
					});

					progress |= true;
				}

				/********************
				 ** Cache handling **
				 ********************/

				progress |= _cache.execute();

				while (_cache.peek_generated_primitive()) {

					if (!_io.acceptable()) { break; }

					Cbe::Primitive   p    = _cache.take_generated_primitive();
					Cbe::Block_data &data = _cache.take_generated_data(p);
					_cache.discard_generated_primitive(p);

					_io.submit_primitive(CACHE_TAG, p, data);

					progress |= true;
				}

				/*********************
				 ** Crypto handling **
				 *********************/

				_crypto.execute();
				progress |= _crypto.execute_progress();

				while (1) {
					Cbe::Primitive prim = _crypto.peek_completed_primitive();
					if (!prim.valid()) { break; }

					_crypto.drop_completed_primitive(prim);
					_request_pool.mark_completed_primitive(prim);

					progress |= true;
				}

				while (1) {

					Cbe::Primitive prim = _crypto.peek_generated_primitive();
					if (!prim.valid())     { break; }
					if (!_io.acceptable()) { break; }

					Cbe::Block_data cipher_data = _crypto.peek_generated_cipher_data(prim);
					_crypto.drop_generated_primitive(prim);

					_io.submit_primitive(CRYPTO_TAG, prim, cipher_data);

					progress |= true;
				}

				/******************
				 ** I/O handling **
				 ******************/

				progress |= _io.execute();

				while (_io.peek_completed_primitive()) {

					Cbe::Primitive p = _io.take_completed_primitive();

					switch (p.tag) {
					case CRYPTO_TAG: _crypto.mark_completed_primitive(p); break;
					case CACHE_TAG:  _cache.mark_completed_primitive(p);  break;
					default: break;
					}

					progress |= true;
				}

				if (!progress) { break; }
			}

			block_session.wakeup_client();
		}

		void _setup()
		{
			Util::Block_io io(_block, BLOCK_SIZE, 0, 1);

			Cbe::Super_block &sb = *io.addr<Cbe::Super_block*>();
			Cbe::Super_block::Number root_number      = sb.root_number;
			Cbe::Super_block::Height height           = sb.height;
			Cbe::Super_block::Degree degree           = sb.degree;
			Cbe::Super_block::Number_of_leaves leaves = sb.leaves;

			Genode::log("Virtual block-device info: ",
			            "tree height: ", height, " ",
			            "edges per node: ", degree, " ",
			            "leaves: ", leaves, " ",
			            "root block address: ", root_number, " "
			);

			_max_vba = leaves - 1;

			_trans.construct(height, degree, root_number);
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env) : _env(env)
		{
			_setup();

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
