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
#include <base/thread.h>
#include <block/request_stream.h>
#include <root/root.h>
#include <terminal_session/connection.h>
#include <util/bit_allocator.h>

/* cbe includes */
#include <cbe/library.h>
#include <cbe/util.h>
#include <cbe/external_crypto.h>

/* repo includes */
#include <util/sha256_4k.h>

/* local includes */
#include <util.h>


namespace Cbe {

	struct Block_session_component;
	struct Main;

	/**
	 * Convert CBE primitive to CBE request
	 *
	 * \param p refrence to primitive
	 *
	 * \return Cbe::Request object
	 */
	static inline Cbe::Request convert_from(Cbe::Primitive const &p)
	{
		auto convert_op = [&] (Cbe::Primitive::Operation o) {
			switch (o) {
			case Cbe::Primitive::Operation::INVALID: return Cbe::Request::Operation::INVALID;
			case Cbe::Primitive::Operation::READ:    return Cbe::Request::Operation::READ;
			case Cbe::Primitive::Operation::WRITE:   return Cbe::Request::Operation::WRITE;
			case Cbe::Primitive::Operation::SYNC:    return Cbe::Request::Operation::SYNC;
			}
			return Cbe::Request::Operation::INVALID;
		};
		return Cbe::Request {
			.operation    = convert_op(p.operation),
			.success      = Cbe::Request::Success::FALSE,
			.block_number = p.block_number,
			.offset       = 0,
			.count        = 1,
			.tag          = 0,
		};
	}

	/**
	 * Convert CBE request
	 *
	 * \param r  reference to CBE request object
	 *
	 * \return  Block request object
	 */
	static inline Block::Request convert_from(Cbe::Request const &r)
	{
		auto convert_op = [&] (Cbe::Request::Operation o) {
			switch (o) {
			case Cbe::Request::Operation::INVALID: return Block::Operation::Type::INVALID;
			case Cbe::Request::Operation::READ:    return Block::Operation::Type::READ;
			case Cbe::Request::Operation::WRITE:   return Block::Operation::Type::WRITE;
			case Cbe::Request::Operation::SYNC:    return Block::Operation::Type::SYNC;
			}
			return Block::Operation::Type::INVALID;
		};
		auto convert_success = [&] (Cbe::Request::Success s) {
			return s == Cbe::Request::Success::TRUE ? true : false;
		};
		return Block::Request {
			.operation = {
				.type         = convert_op(r.operation),
				.block_number = r.block_number,
				.count        = r.count,
			},
			.success   = convert_success(r.success),
			.offset    = (Block::off_t)r.offset,
			.tag       = { .value = r.tag },
		};
	}

	/**
	 * Convert Block request
	 *
	 * \param r  reference to Block request object
	 *
	 * \return  CBE request object
	 */
	static inline Cbe::Request convert_to(Block::Request const &r)
	{
		auto convert_op = [&] (Block::Operation::Type t) {
			switch (t) {
			case Block::Operation::Type::INVALID: return Cbe::Request::Operation::INVALID;
			case Block::Operation::Type::READ:    return Cbe::Request::Operation::READ;
			case Block::Operation::Type::WRITE:   return Cbe::Request::Operation::WRITE;
			case Block::Operation::Type::SYNC:    return Cbe::Request::Operation::SYNC;
			case Block::Operation::Type::TRIM:    return Cbe::Request::Operation::INVALID; // XXX fix
			}
			return Cbe::Request::Operation::INVALID;
		};
		auto convert_success = [&] (bool success) {
			return success ? Cbe::Request::Success::TRUE : Cbe::Request::Success::FALSE;
		};

		return Cbe::Request {
			.operation    = convert_op(r.operation.type),
			.success      = convert_success(r.success),
			.block_number = r.operation.block_number,
			.offset       = (Genode::uint64_t)r.offset,
			.count        = (Genode::uint32_t)r.operation.count,
			.tag          = (Genode::uint32_t)r.tag.value,
		};
	}

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


class Cbe::Main : Rpc_object<Typed_root<Block::Session>>
{
	private:

		// Main(Main const&) = delete;
		// Main &operator=(Main const &) = delete;

		Env &_env;

		Attached_rom_dataspace _config_rom { _env, "config" };

		bool _show_progress    { false };
		bool _show_if_progress { true };

		Constructible<Attached_ram_dataspace>  _block_ds { };
		Constructible<Block_session_component> _block_session { };

		/* backend Block session, used by I/O module */
		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };
		Heap                _heap        { _env.ram(), _env.rm() };
		Allocator_avl       _block_alloc { &_heap };
		Block::Connection<> _block       { _env, &_block_alloc, TX_BUF_SIZE };

		Cbe::Time _time { _env };

		Constructible<Cbe::Library> _cbe               { };
		Io_buffer                   _io_buf            { };
		Crypto_plain_buffer         _crypto_plain_buf  { };
		Crypto_cipher_buffer        _crypto_cipher_buf { };
		External::Crypto            _crypto            { };

		/*
 		 * Store current backend request
		 */
		Cbe::Request _backend_request { };

		Cbe::Superblocks _super_blocks { };

		Signal_handler<Main> _request_handler {
			_env.ep(), *this, &Main::_handle_requests };

		Time::Timestamp _sync_interval;
		Time::Timestamp _secure_interval;
		Time::Timestamp _last_sync_time;
		Time::Timestamp _last_secure_time;

		void _handle_requests()
		{
			if (!_block_session.constructed()) { return; }

			Block_session_component &block_session = *_block_session;

			uint32_t _loop_count = 0;

			for (;;) {

				bool progress = false;

				if (_show_if_progress) {
					Genode::log("\033[33m", ">>> loop_count: ", ++_loop_count);
				}

				/*
				 * Import new Block session requests and convert them to
				 * CBE block requests.
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

					if (!_cbe->client_request_acceptable()) {
						return Block_session_component::Response::RETRY;
					}

					Cbe::Request req = convert_to(request);
					_cbe->submit_client_request(req);

					if (_show_progress || _show_if_progress) {
						log("\033[35m", "> NEW request: ", req);
					}

					progress |= true;
					return Block_session_component::Response::ACCEPTED;
				});

				/*
				 * Acknowledge finished Block session requests.
				 */

				block_session.try_acknowledge([&] (Block_session_component::Ack &ack) {

					Cbe::Request const &req = _cbe->peek_completed_client_request();
					if (!req.valid()) { return; }

					_cbe->drop_completed_client_request();

					Block::Request request = convert_from(req);
					ack.submit(request);

					if (_show_progress || _show_if_progress) {
						Genode::log("\033[35m", "< ACK request: ", req);
					}

					progress |= true;
				});

				/******************
				 ** CBE handling **
				 ******************/


				/*
				 * Time handling
				 */

				/*
				 * Query current time and check if a timeout has triggered
				 */

				/*
				 * Seal the current generation if sealing is not already
				 * in Progress. In case no write operation was performed just set
				 * the trigger for the next interval.
				 *
				 *
				 * (Instead of checking all Cache entries it would be nice if the
				 *  Cache module would provide an interface that would allow us to
				 *  simple check if it contains any dirty entries as it could easily
				 *  track that condition internally itself.)
				 */
				Time::Timestamp const now = _time.timestamp();
				bool is_sealing_generation = _cbe->is_sealing_generation();
				if (now - _last_sync_time >= _sync_interval &&
				    !is_sealing_generation)
				{
					if (_cbe->cache_dirty()) {
						// Genode::log ("\033[93;44m", __Func__, " SEAL current generation: ", Obj.Cur_Gen);
						_cbe->start_sealing_generation();
						is_sealing_generation = true;
					} else {
						// DBG("Cache is not dirty, re-arm trigger");
						_last_sync_time = now;
						_time.schedule_sync_timeout(_sync_interval);
					}
				}

				/*
				 * Secure the current super-block if securing is not already
				 * in Progress. In case no write operation was performed,
				 * i.E., no snapshot was changed, just set the trigger for the
				 * next interval.
				 *
				 *
				 * (Obj.Superblock_Dirty is set whenver the Write_Back module
				 * has done its work
				 * and will be reset when the super-block was secured.)
				 */
				bool is_securing_superblock = _cbe->is_securing_superblock();
				if (now - _last_secure_time >= _secure_interval &&
				    !is_securing_superblock)
				{
					if (_cbe->superblock_dirty()) {
						// Genode::log ("\033[93;44m", __Func__,
						//              " SEALCurr super-block: ", Obj.Cur_SB);
						_cbe->start_securing_superblock();
						is_securing_superblock = true;
					} else {
						// DBG("no snapshots created, re-arm trigger");
						_last_secure_time = now;
					}
				}

				_cbe->execute(_io_buf, _crypto_plain_buf, _crypto_cipher_buf, now);
				progress |= _cbe->execute_progress();

				/* if sealing has finished during 'execute', set new timeout */
				if (is_sealing_generation && !_cbe->is_sealing_generation()) {
					/*
					 * (As already briefly mentioned in the time handling section,
					 *  it would be more reasonable to only set the timeouts when
					 *  we actually perform write Request.)
					 */
					_last_sync_time = now;
					_time.schedule_sync_timeout(_sync_interval);
				}
				/* if securing has finished during 'execute', set new timeout */
				if (is_securing_superblock && !_cbe->is_securing_superblock()) {
					/*
					 * (FIXME same was with sealing the generation, it might make
					 *  sense to set the trigger only when a write operation
					 *  was performed.)
					 */
					_last_secure_time = now;
					_time.schedule_secure_timeout(_secure_interval);
				}

				using Payload = Block::Request_stream::Payload;

				/*
				 * Poll the CBE for pending data requests. This always happens
				 * whenever we need to read from the Block::Request_stream in case
				 * it is a write requests or write to it when it is read request.
				 */
				block_session.with_payload([&] (Payload const &payload) {

					/* read */
					{
						Cbe::Request const cbe_request = _cbe->client_data_ready();
						log("\033[36m INF ", "client_data_ready: ", cbe_request);
						if (!cbe_request.valid()) { return; }

						uint64_t const prim_index = _cbe->give_data_index(cbe_request);
						if (prim_index == ~0ull) {
							Genode::error("prim_index invalid: ", cbe_request);
							return;
						}

						Block::Request request { };
						request.offset = cbe_request.offset + (prim_index * BLOCK_SIZE);
						request.operation.count = 1;

						payload.with_content(request, [&] (void *addr, Genode::size_t) {

							Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(addr);
							Crypto_plain_buffer::Index data_index(~0);
							bool const data_index_valid =
								_cbe->obtain_client_data(cbe_request, data_index);

							if (data_index_valid) {
								progress |= true;
								data = _crypto_plain_buf.item(data_index);
							}
							log("\033[36m INF ", "obtain_client_data: ", cbe_request);
						});
					}
				});


				block_session.with_payload([&] (Payload const &payload) {
					/* write */
					{
						Cbe::Request const cbe_request = _cbe->client_data_required();
						log("\033[36m INF ", "client_data_required: ", cbe_request);
						if (!cbe_request.valid()) { return; }

						uint64_t const prim_index = _cbe->give_data_index(cbe_request);
						if (prim_index == ~0ull) {
							Genode::error("prim_index invalid: ", cbe_request);
							return;
						}

						Block::Request request { };
						request.offset = cbe_request.offset + (prim_index * BLOCK_SIZE);
						request.operation.count = 1;

						payload.with_content(request, [&] (void *addr, Genode::size_t) {

							Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(addr);
							progress |= _cbe->supply_client_data(_time.timestamp(), cbe_request, data);
							log("\033[36m INF ", "supply_client_data: ", cbe_request);
						});
					}
				});

				/*
				 * Backend I/O
				 */

				bool io_progress = false;
				struct Invalid_io_request : Exception { };

				/*
				 * Handle backend I/O requests
				 */
				while (_block.tx()->ready_to_submit()) {

					Io_buffer::Index data_index { 0 };
					Cbe::Request request = _cbe->has_io_request(data_index);

					log("\033[36m INF ", "has_io_request: ", request);
					if (!request.valid()) {
						break;
					}
					if (_backend_request.valid()) {
						break;
					}
					try {
						request.tag = data_index.value;
						Block::Packet_descriptor::Opcode op;
						switch (request.operation) {
						case Request::Operation::READ:
							op = Block::Packet_descriptor::READ;
							break;
						case Request::Operation::WRITE:
							op = Block::Packet_descriptor::WRITE;
							break;
						default:
							throw Invalid_io_request();
						}
						Block::Packet_descriptor packet {
							_block.alloc_packet(Cbe::BLOCK_SIZE), op,
							request.block_number, request.count };

						if (request.operation == Request::Operation::WRITE) {
							*reinterpret_cast<Cbe::Block_data*>(
								_block.tx()->packet_content(packet)) =
									_io_buf.item(data_index);
						}
						_block.tx()->try_submit_packet(packet);
						_backend_request = request;

						log("\033[36m INF ", "io_request_in_progress: ", request);
						_cbe->io_request_in_progress(data_index);

						progress |= true;
						io_progress |= true;
					}
					catch (Block::Session::Tx::Source::Packet_alloc_failed) { break; }
				}

				while (_block.tx()->ack_avail()) {
					Block::Packet_descriptor packet = _block.tx()->try_get_acked_packet();

					if (!_backend_request.valid()) { break; }

					bool const read  = packet.operation() == Block::Packet_descriptor::READ;
					bool const write = packet.operation() == Block::Packet_descriptor::WRITE;
					bool const op_match = (read && _backend_request.read())
					                   || (write && _backend_request.write());
					bool const bn_match = packet.block_number() == _backend_request.block_number;
					// assert packet descriptor belongs to stored backend request
					if (!bn_match || !op_match) { break; }

					_backend_request.success =
						packet.succeeded() ? Cbe::Request::Success::TRUE
						                   : Cbe::Request::Success::FALSE;

					Io_buffer::Index const data_index { _backend_request.tag };
					bool             const success    { _backend_request.success == Request::Success::TRUE };
					if (read && success) {
						_io_buf.item(data_index) =
							*reinterpret_cast<Cbe::Block_data*>(
								_block.tx()->packet_content(packet));
					}
					log("\033[36m INF ", "io_request_completed: ", _backend_request);
					_cbe->io_request_completed(data_index, success);
					progress |= true;

					_block.tx()->release_packet(packet);

					_backend_request = Cbe::Request { };
					io_progress |= true;
				}

				progress |= io_progress;

				/*********************
				 ** Crypto handling **
				 *********************/

				progress |= _crypto.execute();

				/* encrypt */
				while (true) {
					Crypto_plain_buffer::Index data_index(0);
					Cbe::Request request = _cbe->crypto_cipher_data_required(data_index);
					if (!request.valid()) {
						break;
					}
					if (!_crypto.encryption_request_acceptable()) {
						break;
					}
					request.tag = data_index.value;
					_crypto.submit_encryption_request(request, _crypto_plain_buf.item(data_index), 0);
					_cbe->crypto_cipher_data_requested(data_index);
					progress |= true;
				}
				while (true) {
					Cbe::Request const request = _crypto.peek_completed_encryption_request();
					if (!request.valid()) {
						break;
					}
					Crypto_cipher_buffer::Index const data_index(request.tag);
					if (!_crypto.supply_cipher_data(request, _crypto_cipher_buf.item(data_index))) {
						break;
					}
					_cbe->supply_crypto_cipher_data(data_index, request.success == Request::Success::TRUE);
					progress |= true;
				}

				/* decrypt */
				while (true) {
					Crypto_cipher_buffer::Index data_index(0);
					Cbe::Request request = _cbe->crypto_plain_data_required(data_index);
					if (!request.valid()) {
						break;
					}
					if (!_crypto.decryption_request_acceptable()) {
						break;
					}
					request.tag = data_index.value;
					_crypto.submit_decryption_request(request, _crypto_cipher_buf.item(data_index), 0);
					_cbe->crypto_plain_data_requested(data_index);
					progress |= true;
				}
				while (true) {
					Cbe::Request const request = _crypto.peek_completed_decryption_request();
					if (!request.valid()) {
						break;
					}
					Crypto_plain_buffer::Index const data_index(request.tag);
					if (!_crypto.supply_plain_data(request, _crypto_plain_buf.item(data_index))) {
						break;
					}
					_cbe->supply_crypto_plain_data(data_index, request.success == Request::Success::TRUE);
					progress |= true;
				}

				if (!progress)
					break;
			}

			if (_show_if_progress)
				log("\033[33m", ">>> wakeup I/O and client");

			/* notify I/O backend */
			_block.tx()->wakeup();

			/* notify client */
			block_session.wakeup_client_if_needed();
		}

		/**
		 *  Read super-blocks
		 *
		 *  The super-blocks are read one by one using blocking I/O and are
		 *  stored in the super-block array. While reading all blocks, the
		 *  most recent one is determined
		 *
		 *  \param  sb  array where the super-blocks are stored
		 *
		 *  \return  index of the most recent super-block or an INVALID
		 *           index in case the super-block could not be found
		 */
		Cbe::Superblocks_index _read_superblocks(Cbe::Superblocks &sbs)
		{
			Cbe::Generation        last_gen = 0;
			Cbe::Superblocks_index most_recent_sb { 0 };
			bool                   most_recent_sb_valid { false };

			/*
			 * Read all super block slots and use the most recent one.
			 */
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				Util::Block_io io(_block, sizeof (Cbe::Superblock), i, 1);
				void const       *src = io.addr<void*>();
				Cbe::Superblock &dst = sbs.block[i];
				Genode::memcpy(&dst, src, sizeof (Cbe::Superblock));

				/*
				 * For now this always selects the last SB if the generation
				 * is the same and is mostly used for finding the initial SB
				 * with generation == 0.
				 */
				if (dst.valid() && dst.last_secured_generation >= last_gen) {
					most_recent_sb.value = i;
					most_recent_sb_valid = true;
					last_gen = dst.last_secured_generation;
				}

				Sha256_4k::Hash hash { };
				Sha256_4k::Data const &data = *reinterpret_cast<Sha256_4k::Data const*>(&dst);
				Sha256_4k::hash(data, hash);
				Genode::log("SB[", i, "] hash: ", hash);
			}
			struct Failed : Exception { };
			if (!most_recent_sb_valid) {
				throw Failed();
			}
			return most_recent_sb;
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env)
		:
			_env              { env },
			_sync_interval    { 1000 * _config_rom.xml().attribute_value("sync_interval", 5u) },
			_secure_interval  { 1000 * _config_rom.xml().attribute_value("secure_interval", 30u) },
			_last_sync_time   { _time.timestamp() },
			_last_secure_time { _time.timestamp() }
		{
			/*
			 * We first parse the configuration here which is used to control the
			 * verbosity and the time intervals so we do not have to do that in
			 * the CBE library.
			 */
			_show_progress =
				_config_rom.xml().attribute_value("show_progress", false);

			/*
			 * Set initial encryption key
			 */
			External::Crypto::Key_data key { };
			Genode::memcpy(key.value, "All your base are belong to us  ", 32);
			_crypto.set_key(0, 0, key);

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
			Cbe::Superblocks_index const curr_sb = _read_superblocks(_super_blocks);

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
			_cbe.construct(_super_blocks, curr_sb);
			_time.schedule_sync_timeout(_sync_interval);
			_time.schedule_secure_timeout(_secure_interval);

			/*
			 * Install signal handler for the backend Block connection.
			 *
			 * (Hopefully outstanding Block requests will not create problems when
			 *  the frontend session is already gone.)
			 */
			_block.tx_channel()->sigh_ack_avail(_request_handler);
			_block.tx_channel()->sigh_ready_to_submit(_request_handler);

			/* finally announce Block session */
			_env.parent().announce(_env.ep().manage(*this));
		}

		~Main()
		{
			_block.tx_channel()->sigh_ack_avail(Signal_context_capability());
			_block.tx_channel()->sigh_ready_to_submit(Signal_context_capability());
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

			/*
			 * Dump the current SB info for diagnostic reasons.
			 */
//			_cbe->dump_cur_sb_info();

			return _block_session->cap();
		}

		void upgrade(Capability<Session>, Root::Upgrade_args const &) override { }

		void close(Capability<Session>) override
		{
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


extern "C" void print_cstring(char const *s, Genode::size_t len)
{
	Genode::log(Genode::Cstring(s, len));
}

extern "C" void adainit();

void Component::construct(Genode::Env &env)
{
	env.exec_static_constructors();

	/**
	 * We have to call adainit, so, the secondary stack of SPARK
	 * for, e.g., variable-sized return values gets initialized.
	 */
	adainit();

	Cbe::assert_valid_object_size<Cbe::Library>();
	Cbe::assert_valid_object_size<External::Crypto>();

	static Cbe::Main inst(env);
}


/******************************
 ** Cbe::Time implementation **
 ******************************/

void Cbe::Time::_handle_sync_timeout(Genode::Duration)
{
	if (_sync_sig_cap.valid()) {
		Genode::Signal_transmitter(_sync_sig_cap).submit();
	}
}


void Cbe::Time::_handle_secure_timeout(Genode::Duration)
{
	if (_secure_sig_cap.valid()) {
		Genode::Signal_transmitter(_secure_sig_cap).submit();
	}
}


Cbe::Time::Time(Genode::Env &env) : _timer(env) { }


Cbe::Time::Timestamp Cbe::Time::timestamp()
{
	return _timer.curr_time().trunc_to_plain_ms().value;
}


void Cbe::Time::sync_sigh(Genode::Signal_context_capability cap)
{
	_sync_sig_cap = cap;
}


void Cbe::Time::schedule_sync_timeout(uint64_t msec)
{
	if (!msec) {
		return;
	}
	_sync_timeout.schedule(Genode::Microseconds { msec * 1000 });
}


void Cbe::Time::secure_sigh(Genode::Signal_context_capability cap)
{
	_secure_sig_cap = cap;
}


void Cbe::Time::schedule_secure_timeout(uint64_t msec)
{
	if (!msec) {
		return;
	}
	_secure_timeout.schedule(Genode::Microseconds { msec * 1000 });
}
