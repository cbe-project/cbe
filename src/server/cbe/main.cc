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
#include <terminal_session/connection.h>
#include <util/bit_allocator.h>

/* cbe includes */
#include <cbe/library.h>

/* repo includes */
#include <util/sha256_4k.h>

/* local includes */
#include <util.h>


namespace Cbe {

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

		Constructible<Cbe::Public_Library> _cbe { };

		/*
 		 * Store current backend request
		 */
		Cbe::Request _backend_request { };

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

					if (!_cbe->request_acceptable()) {
						return Block_session_component::Response::RETRY;
					}

					Cbe::Request req = convert_to(request);
					_cbe->submit_request(req);

					if (_show_progress || _show_if_progress) {
						log("NEW request: ", req);
					}

					progress |= true;
					return Block_session_component::Response::ACCEPTED;
				});

				/*
				 * Acknowledge finished Block session requests.
				 */

				block_session.try_acknowledge([&] (Block_session_component::Ack &ack) {

					Cbe::Request const &req = _cbe->peek_completed_request();
					if (!req.valid()) { return; }

					_cbe->drop_completed_request(req);

					Block::Request request = convert_from(req);
					ack.submit(request);

					if (_show_progress || _show_if_progress) {
						Genode::log("ACK request: ", req);
					}

					progress |= true;
				});

				/*
				 * CBE handling
				 */

				progress |= _cbe->execute(_show_progress, _show_if_progress);

				using Payload = Block::Request_stream::Payload;
				block_session.with_payload([&] (Payload const &payload) {

					/*
					 * Poll the CBE for pending data requests. This always happens
					 * whenever we need to read from the Block::Request_stream in case
					 * it is a write requests or write to it when it is read request.
					 */
					Cbe::Request const cbe_request = _cbe->have_data();
					if (!cbe_request.valid()) { return; }

					uint64_t const prim_index = _cbe->give_data_index(cbe_request);
					if (prim_index == ~0ull) {
						Genode::error("prim_index invalid: ", cbe_request);
						return;
					}

					/*
					 * Create the Block request used to calculate the proper location
					 * within the shared memory.
					 *
					 * (AFAICT the primitive index should always be 0 as it does not
					 *  get used anymore and it stands to reason if it should be removed.)
					 */
					Block::Request request { };
					request.offset = cbe_request.offset + (prim_index * BLOCK_SIZE);
					request.operation.count = 1;

					payload.with_content(request, [&] (void *addr, Genode::size_t) {

						Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(addr);

						if (cbe_request.read()) {
							progress |= _cbe->give_read_data(cbe_request, data);
						} else

						if (cbe_request.write()) {
							progress |= _cbe->give_write_data(cbe_request, data);
						}
					});
				});

				/*
				 * Backend I/O
				 */

				bool io_progress = false;

				/*
				 * Handle backend I/O requests
				 */
				while (_block.tx()->ready_to_submit()) {

					Cbe::Request const request = _cbe->need_data();
					if (!request.valid())                { break; }
					if (_backend_request.valid())        { break; }

					try {
						Block::Packet_descriptor packet {
							_block.alloc_packet(Cbe::BLOCK_SIZE),
							request.read() ? Block::Packet_descriptor::READ
							               : Block::Packet_descriptor::WRITE,
							request.block_number,
							request.count,
						};

						if (request.read()) {
							// XXX release packet in case of return false
							progress |= _cbe->take_read_data(request);
						}

						if (request.write()) {
							// XXX release packet in case of return false
							Cbe::Block_data &data =
								*reinterpret_cast<Cbe::Block_data*>(_block.tx()->packet_content(packet));
							progress |= _cbe->take_write_data(request, data);
						}

						_block.tx()->submit_packet(packet);

						_backend_request = request;
						io_progress |= true;
					}
					catch (Block::Session::Tx::Source::Packet_alloc_failed) { break; }
				}

				/*
				 * Handle backend I/O results
				 */
				while (_block.tx()->ack_avail()) {
					Block::Packet_descriptor packet = _block.tx()->get_acked_packet();

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

					if (read) {
						Cbe::Block_data &data =
							*reinterpret_cast<Cbe::Block_data*>(_block.tx()->packet_content(packet));
						progress |= _cbe->ack_read_data(_backend_request, data);
					} else

					if (write) {
						progress |= _cbe->ack_write_data(_backend_request);
					}

					_block.tx()->release_packet(packet);

					_backend_request = Cbe::Request { };
					io_progress |= true;
				}

				progress |= io_progress;

				if (!progress) {
					if (_show_if_progress) {
						Genode::log("\033[33m", ">>> break, no progress");
					}
					break;
				}
			}

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
		Cbe::Super_block_index _read_superblocks(Cbe::Super_block sb[Cbe::NUM_SUPER_BLOCKS])
		{
			Cbe::Generation        last_gen = 0;
			Cbe::Super_block_index most_recent_sb { Cbe::Super_block_index::INVALID };

			static_assert(sizeof (Cbe::Super_block) == Cbe::BLOCK_SIZE,
			              "Super-block size mistmatch");

			/*
			 * Read all super block slots and use the most recent one.
			 */
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				Util::Block_io io(_block, sizeof (Cbe::Super_block), i, 1);
				void const       *src = io.addr<void*>();
				Cbe::Super_block &dst = sb[i];
				Genode::memcpy(&dst, src, sizeof (Cbe::Super_block));

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
				_config_rom.xml().attribute_value("sync_interval", 5u);
			Cbe::Time::Timestamp const secure = 1000 *
				_config_rom.xml().attribute_value("secure_interval", 30u);

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
			Cbe::Super_block_index curr_sb = _read_superblocks(_super_block);
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
			_cbe.construct(_time, sync, secure, _super_block, curr_sb);

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
			_cbe->dump_cur_sb_info();

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
