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

/* local includes */
#include <types.h>

/* SPARK modules */
#include <request_pool_module.h>
#include <splitter_module.h>
/* C++ modules */
#include <crypto_module.h>
#include <io_module.h>


static bool _verbose;


namespace Cbe {

	struct Block_session_component;
	struct Main;

	using namespace Genode;

} /* namespace Cbe */


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


class Cbe::Main : Rpc_object<Typed_root<Block::Session>>
{
	private:

		Env &_env;

		Attached_rom_dataspace _config_rom { _env, "config" };

		Constructible<Attached_ram_dataspace>  _block_ds { };
		Constructible<Block_session_component> _block_session { };

		/* back end Block session */
		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };
		Heap              _heap        { _env.ram(), _env.rm() };
		Allocator_avl     _block_alloc { &_heap };
		Block::Connection _block       { _env, &_block_alloc, TX_BUF_SIZE };
		Block::sector_t   _block_count { 0 };
		size_t            _block_size  { 0 };
		Block::Session::Operations _block_ops   {   };

		/* modules */

		enum {
			MAX_REQS      = 16,
			MAX_PRIM      = 1,
			MAX_LEVEL     = 6,
		};

		enum {
			POOL_TAG        = 0,
			CRYPTO_TAG      = 1,
		};

		using Pool        = Module::Request_pool;
		using Splitter    = Module::Splitter;
		using Crypto      = Module::Crypto<MAX_PRIM>;
		using Block_io    = Module::Block_io<MAX_PRIM, BLOCK_SIZE>;

		Pool                    _request_pool { };
		Splitter                _splitter     { };
		Constructible<Crypto>   _crypto       { };
		Block_io                _io           { _block };
		Bit_allocator<MAX_REQS> _tag_alloc    { };

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

					if (request.block_number >= _block_count) {
						uint64_t const lba = request.block_number;
						warning("reject request with out-of-range block address ", lba);

						return Block_session_component::Response::REJECTED;
					}

					if (!_request_pool.request_acceptable()) {
						return Block_session_component::Response::RETRY;
					}

					if (!request.operation_defined()) {
						uint64_t const lba = request.block_number;
						warning("reject invalid request for virtual block address ", lba);
						return Block_session_component::Response::REJECTED;
					}

					Number_of_primitives const num = _splitter.number_of_primitives(request);
					request.tag = _tag_alloc.alloc();
					_request_pool.submit_request(request, num);

					if (_verbose) {
						uint32_t const tag    = request.tag;
						uint64_t const offset = request.offset;
						log("New request:", tag, " offset:", Hex(offset), " primitives:", num);
					}

					progress |= true;
					return Block_session_component::Response::ACCEPTED;
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

					if (!_crypto->acceptable()) { break; }

					Cbe::Primitive p = _splitter.peek_generated_primitive();
					_splitter.drop_generated_primitive(p);

					block_session.with_payload([&] (Block::Request_stream::Payload const &payload) {

						Block_data *data_ptr = _data_for_primitive(payload, _request_pool, p);
						if (data_ptr == nullptr) {

							p.success = Cbe::Primitive::Success::FALSE;
							_request_pool.mark_completed_primitive(p);
							return;
						}

						_crypto->submit_primitive(p, *data_ptr);
					});

					progress |= true;
				}

				/*********************
				 ** Crypto handling **
				 *********************/

				progress |= _crypto->execute();

				while (_crypto->peek_completed_primitive().valid()) {

					Cbe::Primitive p = _crypto->take_completed_primitive();
					_request_pool.mark_completed_primitive(p);

					progress |= true;
				}

				while (_crypto->peek_generated_primitive().valid()) {

					if (!_io.acceptable()) { break; }

					Cbe::Primitive   p    = _crypto->take_generated_primitive();
					Cbe::Block_data &data = _crypto->take_generated_data(p);
					_crypto->discard_generated_primitive(p);

					_io.submit_primitive(CRYPTO_TAG, p, data);

					progress |= true;
				}

				/******************
				 ** I/O handling **
				 ******************/

				progress |= _io.execute();

				while (_io.peek_completed_primitive()) {

					Cbe::Primitive p = _io.take_completed_primitive();

					switch (p.tag) {
					case CRYPTO_TAG: _crypto->mark_completed_primitive(p); break;
					default: break;
					}

					progress |= true;
				}

				/*
				 * Acknowledge finished block requests
				 */
				block_session.try_acknowledge([&] (Block_session_component::Ack &ack) {

					Block::Request const &req = _request_pool.peek_completed_request();
					if (!req.operation_defined()) { return; }

					_tag_alloc.free(req.tag);
					ack.submit(req);

					if (_verbose) {
						uint32_t const tag    = req.tag;
						uint64_t const offset = req.offset;
						log("Ack request:", tag, " offset:", Hex(offset));
					}

					_request_pool.drop_completed_request(req);
					progress |= true;
				});

				if (!progress) { break; }
			}

			block_session.wakeup_client();
		}

		Block::sector_t _front_block_count { 0 };

		void _setup_block_info()
		{
			_block.info(&_block_count, &_block_size, &_block_ops);
			if (  _block_count % (Cbe::BLOCK_SIZE / _block_size) != 0
			   || _block_size > Cbe::BLOCK_SIZE) {
				error("back-end block count not multiple of front-end block size");

				struct Invalid_block_session { };
				throw Invalid_block_session();
			}

			_front_block_count = _block_count / (Cbe::BLOCK_SIZE / _block_size);

			log("Create AES-CBC block device with ", _front_block_count * Cbe::BLOCK_SIZE,
			    " bytes and block size ", (unsigned)Cbe::BLOCK_SIZE);
		}

		void _setup_crypto()
		{
			Xml_node config = _config_rom.xml();

			/* include space for \0 */
			using Key = Genode::String<33>;
			Key key = config.attribute_value("key", Key());
			if (!key.valid()) {
				error("missing key attribute");

				struct Invalid_key_attribute { };
				throw Invalid_key_attribute();
			}

			char buffer[32];
			Genode::memset(buffer, 0x55, sizeof (buffer));
			Genode::memcpy(buffer, key.string(), key.length() - 1);

			_crypto.construct(buffer);
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env) : _env(env)
		{
			_verbose = _config_rom.xml().attribute_value("verbose", false);

			_setup_block_info();
			_setup_crypto();

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
			                         _request_handler, _front_block_count);

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


Genode::Env *__genode_env;


void Component::construct(Genode::Env &env)
{
	/* make ada-runtime happy */
	__genode_env = &env;
	env.exec_static_constructors();

	static Cbe::Main inst(env);
}
