/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

/* Genode includes */
#include <vfs/dir_file_system.h>
#include <vfs/single_file_system.h>
#include <util/xml_generator.h>

/* repo includes */
#include <util/sha256_4k.h>

/* cbe includes */
#include <cbe/library.h>
#include <cbe/external_crypto.h>


namespace Vfs_cbe {
	using namespace Vfs;
	using namespace Genode;

	class Data_file_system;
	class Key_file_system;

	class Create_snapshot_file_system;
	class Discard_snapshot_file_system;
	class Secure_superblock_file_system;

	struct Control_local_factory;
	class  Control_file_system;

	struct Snapshot_local_factory;
	class  Snapshot_file_system;

	struct Snapshots_local_factory;
	class  Snapshots_file_system;

	struct Local_factory;
	class  File_system;

	class Wrapper;
}


#define ERR(...)  Genode::error(__func__, ":", __LINE__, " ", __VA_ARGS__);


extern "C" void adainit();


extern "C" void print_u8(unsigned char const u) { Genode::log(u); }


extern "C" void print_cstring(char const *s, Genode::size_t len)
{
	Genode::log(Genode::Cstring(s, len));
}


struct Snapshot_id_list
{
	Genode::uint32_t id[Cbe::NUM_SNAPSHOTS];
};


class Vfs_cbe::Wrapper
{
	private:

		typedef Genode::String<32> Config;
		Vfs::Env          &_env;
		Vfs_handle *_backend { nullptr };

		Cbe::Io_buffer            _io_data { };
		Cbe::Crypto_cipher_buffer _cipher_data { };
		Cbe::Crypto_plain_buffer  _plain_data { };
		External::Crypto _crypto { };
		bool _key_set { false };

		Constructible<Cbe::Library> _cbe;

		Cbe::Superblocks_index _cur_sb       { 0 };
		bool                   _cur_sb_valid { false };
		Cbe::Superblocks       _super_blocks { };

		Cbe::Request _backend_request { };

		Cbe::Time _time { _env.env() };

		/* configuration options */
		bool                 _show_progress { false };
		Cbe::Time::Timestamp _sync_interval { 1000 * 5};
		Cbe::Time::Timestamp _secure_interval { 1000 * 30 };
		Config               _block_device { "/dev/block" };

		void _read_config(Xml_node config)
		{
			_show_progress   = config.attribute_value("show_progress", false);
			_sync_interval   = config.attribute_value("sync_interval", 5u) * 1000;
			_secure_interval = config.attribute_value("secure_interval", 30u) * 1000;
			_block_device    = config.attribute_value("block", _block_device);
		}

		Cbe::Superblocks_index _read_superblocks(Cbe::Superblocks &sbs)
		{
			_cur_sb_valid = false;
			Cbe::Generation        last_sb_id = 0;
			Cbe::Superblocks_index most_recent_sb { 0 };
			bool                   most_recent_sb_valid { false };

			static_assert(sizeof (Cbe::Superblock) == Cbe::BLOCK_SIZE,
			              "Super-block size mistmatch");

			/*
			 * Read all super block slots and use the most recent one.
			 */
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				file_size bytes = 0;
				_backend->seek(i * Cbe::BLOCK_SIZE);
				Cbe::Superblock &dst = sbs.block[i];

				if (!_backend->fs().queue_read(_backend, Cbe::BLOCK_SIZE)) {
					ERR("queue_read failed");
					return Cbe::Superblocks_index(0);
				}

				if (!_backend->fs().complete_read(_backend, (char*)&dst, Cbe::BLOCK_SIZE, bytes)) {
					ERR("complete_read failed");
					return Cbe::Superblocks_index(0);
				}

				if (dst.valid() && dst.superblock_id >= last_sb_id) {
					if (dst.superblock_id == last_sb_id) {
						Genode::error("superblock id: ", last_sb_id,
						              " not unique - cannot select proper superblock");
						most_recent_sb_valid = false;
						break;
					}
					most_recent_sb.value = i;
					most_recent_sb_valid = true;
					last_sb_id = dst.superblock_id;
				}
			}

			_cur_sb       = most_recent_sb;
			_cur_sb_valid = most_recent_sb_valid;

			return most_recent_sb;
		}

		struct Could_not_open_block_backend : Genode::Exception { };
		struct No_valid_superblock_found    : Genode::Exception { };

		void _initialize_cbe()
		{
			using Result = Vfs::Directory_service::Open_result;

			Result res = _env.root_dir().open(_block_device.string(), 0,
			                                       (Vfs::Vfs_handle **)&_backend,
			                                       _env.alloc());
			if (res != Result::OPEN_OK) {
				error("cbe_fs: Could not open back end block device: '", _block_device, "'");
				throw Could_not_open_block_backend();
			}

			_cur_sb = _read_superblocks(_super_blocks);

			if (!_cur_sb_valid) {
				error("cbe_fs: No valid super block");
				throw No_valid_superblock_found();
			}

			Genode::log("Use superblock[", _cur_sb, "]: ",
			            _super_blocks.block[_cur_sb.value]);

			_cbe.construct(_super_blocks, _cur_sb);
		}

		void _backend_read(Cbe::Request &request,
		                   Cbe::Io_buffer::Index const data_index)
		{
			request.tag = data_index.value;

			file_size count = request.count * Cbe::BLOCK_SIZE;
			file_size out   = 0;

			_backend_request = request;

			_cbe->io_request_in_progress(data_index);

			_backend->seek(request.block_number * Cbe::BLOCK_SIZE);
			if (!_backend->fs().queue_read(_backend, count)) {
				ERR("queue full");
				return;
			}

			char *buf = nullptr;
			_env.alloc().alloc(count, &buf);

			if (_backend->fs().complete_read(_backend, buf, count, out) == File_io_service::READ_QUEUED) {
				_env.alloc().free(buf, count);
				_state = IO_PENDING;
				return;
			}

			Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data *>(buf);
			_io_data.item(data_index) = data;

			_cbe->io_request_completed(data_index, true);
			_env.alloc().free(buf, count);

			_backend_request = Cbe::Request { };
		}

		void _backend_write(Cbe::Request &request,
		                    Cbe::Io_buffer::Index const data_index)
		{
			request.tag = data_index.value;

			file_size count = request.count * Cbe::BLOCK_SIZE;
			file_size out   = 0;

			_backend_request = request;

			char *buf = nullptr;
			_env.alloc().alloc(count, &buf);
			Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data *>(buf);
			data = _io_data.item(data_index);

			_cbe->io_request_in_progress(data_index);

			_backend->seek(request.block_number * Cbe::BLOCK_SIZE);
			try {
				_backend->fs().write(_backend, buf, count, out);
			} catch (Vfs::File_io_service::Insufficient_buffer) {
				_env.alloc().free(buf, count);
				_state = IO_PENDING;
				return;
			}

			_cbe->io_request_completed(data_index, true);
			_env.alloc().free(buf, count);

			_backend_request = Cbe::Request { };
		}


	public:

		Wrapper(Vfs::Env &env, Xml_node config) : _env(env)
		{
			_read_config(config);
		}

		External::Crypto &crypto()
		{
			return _crypto;
		}

		Cbe::Library &cbe()
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}

			return *_cbe;
		}

		enum Request_state { NONE, PENDING, IO_PENDING, ERROR, ERROR_EOF };
		Request_state _state { NONE };

		struct Invalid_Request : Genode::Exception { };

		Cbe::Request cbe_request(Vfs_handle &handle,
		                         char *data, file_size count,
		                         Cbe::Request::Operation operation)
		{
			file_size const offset = handle.seek();

			if ((offset % Cbe::BLOCK_SIZE) != 0) {
				Genode::error("offset not multiple of block size");
				throw Invalid_Request();
			}

			if (count < Cbe::BLOCK_SIZE) {
				Genode::error("count less than block size");
				throw Invalid_Request();
			}

			return Cbe::Request {
				.operation    = operation,
				.success      = Cbe::Request::Success::FALSE,
				.block_number = offset / Cbe::BLOCK_SIZE,
				.offset       = (uint64_t)data,
				.count        = (uint32_t)(count / Cbe::BLOCK_SIZE),
			};
		}

		void handle_request(Cbe::Request *vfs_request, uint32_t snap_id)
		{
			bool progress = false;

			if (vfs_request) {
				Cbe::Request const &request = *vfs_request;
				Cbe::Virtual_block_address const vba = request.block_number;

				if (vba > _cbe->max_vba()) {
					warning("reject request with out-of-range virtual block address ", vba);
					_state = ERROR_EOF;
					return;
				}

				if (_cbe->client_request_acceptable() && _state == NONE) {
					_cbe->submit_client_request(request, snap_id);
					_state = PENDING;
					progress = true;
				}
			}

			while (true) {

				while (true) {
					Cbe::Request const cbe_request = _cbe->peek_completed_client_request();
					if (!cbe_request.valid()) { break; }

					_cbe->drop_completed_client_request(cbe_request);
					_state = NONE;
					progress = true;
				}

				_cbe->execute(_io_data, _plain_data, _cipher_data, _time.timestamp());
				progress |= _cbe->execute_progress();

				/* read */
				Cbe::Request cbe_request = _cbe->client_data_ready();
				if (cbe_request.valid() && cbe_request.read()) {

					uint64_t const prim_index = _cbe->client_data_index(cbe_request);
					if (prim_index == ~0ull) {
						Genode::error("prim_index invalid: ", cbe_request);
						_state = ERROR;
						return;
					}

					Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(
						cbe_request.offset + (prim_index * Cbe::BLOCK_SIZE));

					Cbe::Crypto_plain_buffer::Index data_index(~0);
					bool const data_index_valid =
						_cbe->obtain_client_data(cbe_request, data_index);

					if (data_index_valid) {
						Genode::memcpy(&data, &_plain_data.item(data_index), sizeof (Cbe::Block_data));

						progress |= true;
					}
				}

				/* write */
				cbe_request = _cbe->client_data_required();
				if (cbe_request.valid() && cbe_request.write()) {
					uint64_t const prim_index = _cbe->client_data_index(cbe_request);
					if (prim_index == ~0ull) {
						Genode::error("prim_index invalid: ", cbe_request);
						_state = ERROR;
						return;
					}

					Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(
						cbe_request.offset + (prim_index * Cbe::BLOCK_SIZE));

					progress = _cbe->supply_client_data(_time.timestamp(), cbe_request, data);

					progress = true;
				}

				Cbe::Io_buffer::Index data_index { 0 };
				cbe_request = _cbe->has_io_request(data_index);
				if (cbe_request.valid()
				    && !_backend_request.valid()) {
					if (cbe_request.read()) {
						_backend_read(cbe_request, data_index);
					} else if (cbe_request.write()) {
						_backend_write(cbe_request, data_index);
					} else {
						Genode::error("invalid cbe_request");
						throw -1;
					}

					progress = true;
				}

				progress |= _crypto.execute();

				/* encrypt */
				while (true) {
					Cbe::Crypto_plain_buffer::Index data_index(0);
					Cbe::Request request = _cbe->crypto_cipher_data_required(data_index);
					if (!request.valid()) {
						break;
					}
					if (!_crypto.encryption_request_acceptable()) {
						break;
					}
					request.tag = data_index.value;
					_crypto.submit_encryption_request(request, _plain_data.item(data_index), 0);
					_cbe->crypto_cipher_data_requested(data_index);
					progress |= true;
				}
				while (true) {
					Cbe::Request const request = _crypto.peek_completed_encryption_request();
					if (!request.valid()) {
						break;
					}
					Cbe::Crypto_cipher_buffer::Index const data_index(request.tag);
					if (!_crypto.supply_cipher_data(request, _cipher_data.item(data_index))) {
						break;
					}
					_cbe->supply_crypto_cipher_data(data_index, request.success == Cbe::Request::Success::TRUE);
					progress |= true;
				}

				/* decrypt */
				while (true) {
					Cbe::Crypto_cipher_buffer::Index data_index(0);
					Cbe::Request request = _cbe->crypto_plain_data_required(data_index);
					if (!request.valid()) {
						break;
					}
					if (!_crypto.decryption_request_acceptable()) {
						break;
					}
					request.tag = data_index.value;
					_crypto.submit_decryption_request(request, _cipher_data.item(data_index), 0);
					_cbe->crypto_plain_data_requested(data_index);
					progress |= true;
				}
				while (true) {
					Cbe::Request const request = _crypto.peek_completed_decryption_request();
					if (!request.valid()) {
						break;
					}
					Cbe::Crypto_plain_buffer::Index const data_index(request.tag);
					if (!_crypto.supply_plain_data(request, _plain_data.item(data_index))) {
						break;
					}
					_cbe->supply_crypto_plain_data(data_index, request.success == Cbe::Request::Success::TRUE);
					progress |= true;
				}

				if (_snapshot_creation_id.valid &&
				    _cbe->snapshot_creation_complete(_snapshot_creation_id)) {
					_snapshot_creation_id = Cbe::Snapshot_ID { 0, false };
				}

				if (!progress) break;
				progress = false;
			}
		}

		bool client_request_acceptable() const
		{
			return _key_set ? _cbe->client_request_acceptable()
		                    : false;
		}

		void set_key(unsigned slot, unsigned id,
		             External::Crypto::Key_data const &key)
		{
			_crypto.set_key(slot, id, key);
			_key_set |= true;
		}

		void active_snapshot_ids(Cbe::Active_snapshot_ids &ids)
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}
			_cbe->active_snapshot_ids(ids);
			handle_request(nullptr, 0);
		}

		Cbe::Snapshot_ID _snapshot_creation_id { 0, false };

		void create_snapshot(bool quaratine)
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}
			if (_snapshot_creation_id.valid) { return; }

			_snapshot_creation_id = _cbe->create_snapshot(quaratine);
			if (_snapshot_creation_id.valid) {
				handle_request(nullptr, 0);
			}
		}

		bool snapshot_creation_complete() const
		{
			if (!_snapshot_creation_id.valid) { return true; }

			if (!_cbe->snapshot_creation_complete(_snapshot_creation_id)) { return false; }

			return true;
		}

		bool discard_snapshot(Cbe::Snapshot_ID id)
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}

			bool const res = _cbe->discard_snapshot(id);
			if (res) {
				handle_request(nullptr, 0);
			}
			return res;
		}
};


class Vfs_cbe::Data_file_system : public Single_file_system
{
	private:

		Wrapper &_w;
		uint32_t _snap_id;

	public:

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;
			uint32_t _snap_id { 0 };

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper       &w,
			           uint32_t snap_id)
			:
				Single_vfs_handle(ds, fs, alloc, 0),
				_w(w), _snap_id(snap_id)
			{ }


			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				out_count = 0;

				/* request already pending, try again later */
				if (!_w.client_request_acceptable() || _w._state != Wrapper::Request_state::NONE) {
					return READ_QUEUED;
				}

				using Operation = Cbe::Request::Operation;
				Cbe::Request request { };
				try {
					request = _w.cbe_request(*this, dst, count, Operation::READ);
				} catch (Wrapper::Invalid_Request) {
					_w._state = Wrapper::Request_state::NONE;
					return READ_ERR_IO;
				}

				_w.handle_request(&request, _snap_id);

				/* retry on next I/O signal */
				if (_w._state == Wrapper::Request_state::PENDING) {
					return READ_QUEUED;
				}

				if (_w._state == Wrapper::Request_state::ERROR) {
					_w._state = Wrapper::Request_state::NONE;
					return READ_ERR_IO;
				}

				if (_w._state == Wrapper::Request_state::ERROR_EOF) {
					_w._state = Wrapper::Request_state::NONE;
					return READ_OK;
				}

				_w._state = Wrapper::Request_state::NONE;
				out_count = count;
				return READ_OK;
			}

			Write_result write(char const *src, file_size count,
			                   file_size &out_count) override
			{
				out_count = 0;

				/* request already pending, try again later */
				if (!_w.client_request_acceptable() || _w._state != Wrapper::Request_state::NONE) {
					throw Insufficient_buffer();
				}

				using Operation = Cbe::Request::Operation;
				Cbe::Request request { };

				try {
					request = _w.cbe_request(*this, const_cast<char *>(src),
					                         count, Operation::WRITE);
				} catch (Wrapper::Invalid_Request) {
					_w._state = Wrapper::Request_state::NONE;
					return WRITE_ERR_IO;
				}
				_w.handle_request(&request, _snap_id);

				/* retry on next I/O signal */
				if (_w._state == Wrapper::Request_state::PENDING) {
					throw Insufficient_buffer();
				}

				if (_w._state == Wrapper::Request_state::ERROR) {
					_w._state = Wrapper::Request_state::NONE;
					return WRITE_ERR_IO;
				}

				if (_w._state == Wrapper::Request_state::ERROR_EOF) {
					_w._state = Wrapper::Request_state::NONE;
					return WRITE_ERR_IO;
				}

				_w._state = Wrapper::Request_state::NONE;
				out_count = count;
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

		Data_file_system(Wrapper &w, uint32_t snap_id)
		:
			Single_file_system(Node_type::CONTINUOUS_FILE, type_name(),
			                   Node_rwx::rw(), Xml_node("<data/>")),
			_w(w), _snap_id(snap_id)
		{ }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Stat_result stat(char const *path, Stat &out) override
		{
			try {
				(void)_w.cbe();
			} catch (...) {
				return STAT_ERR_NO_ENTRY;
			}

			Stat_result result = Single_file_system::stat(path, out);

			/* setting a size is not strictly correct, but ... */
			out.size = 0;
			return result;
		}


		/********************************
		 ** File I/O service interface **
		 ********************************/

		Ftruncate_result ftruncate(Vfs::Vfs_handle *handle, file_size) override
		{
			return FTRUNCATE_OK;
		}


		/***************************
		 ** File-system interface **
		 ***************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				(void)_w.cbe();
			} catch (...) {
				return OPEN_ERR_UNACCESSIBLE;
			}

			*out_handle =
				new (alloc) Vfs_handle(*this, *this, alloc, _w, _snap_id);

			return OPEN_OK;
		}

		static char const *type_name() { return "data"; }
		char const *type() override { return type_name(); }
};


class Vfs_cbe::Key_file_system : public Vfs::Single_file_system
{
	private:

		Wrapper &_w;

		static constexpr Genode::size_t MAX_KEY_SIZE = sizeof (External::Crypto::Key_data);

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper           &w)
			:
				Single_vfs_handle(ds, fs, alloc, 0), _w(w)
			{ }

			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				return READ_ERR_IO;
			}

			Write_result write(char const *src, file_size count, file_size &out_count) override
			{
				out_count = 0;

				if (seek() > (file_size)MAX_KEY_SIZE || count > (file_size)MAX_KEY_SIZE)
					return WRITE_ERR_INVALID;

				External::Crypto::Key_data key { }; // XXX clear key material
				Genode::memset(key.value, 0xa5, sizeof (key.value));
				Genode::memcpy(key.value, src, count);

				_w.set_key(0, 0, key);

				out_count = count;
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Key_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::rw(), Xml_node("<key/>")),
			_w(w)
		{ }

		static char const *type_name() { return "key"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Genode::Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				*out_handle = new (alloc)
					Vfs_handle(*this, *this, alloc, _w);
				return OPEN_OK;
			}
			catch (Genode::Out_of_ram)  { return OPEN_ERR_OUT_OF_RAM; }
			catch (Genode::Out_of_caps) { return OPEN_ERR_OUT_OF_CAPS; }
		}

		Stat_result stat(char const *path, Stat &out) override
		{
			Stat_result result = Single_file_system::stat(path, out);
			out.size = sizeof (External::Crypto::Key_data);
			return result;
		}


		/********************************
		 ** File I/O service interface **
		 ********************************/

		Ftruncate_result ftruncate(Vfs::Vfs_handle *handle, file_size) override
		{
			return FTRUNCATE_OK;
		}
};


class Vfs_cbe::Create_snapshot_file_system : public Vfs::Single_file_system
{
	private:

		Wrapper &_w;

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper &w)
			:
				Single_vfs_handle(ds, fs, alloc, 0),
				_w(w)
			{ }

			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				return READ_ERR_IO;
			}

			Write_result write(char const *src, file_size count, file_size &out_count) override
			{
				bool create_snapshot { false };
				Genode::ascii_to(src, create_snapshot);
				Genode::String<64> str(Genode::Cstring(src, count));

				if (!create_snapshot) {
					return WRITE_ERR_IO;
				}
				out_count = count;
				_w.create_snapshot(true);
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Create_snapshot_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::wo(), Xml_node("<create_snapshot/>")),
			_w(w)
		{ }

		static char const *type_name() { return "create_snapshot"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Genode::Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				*out_handle =
					new (alloc) Vfs_handle(*this, *this, alloc, _w);
				return OPEN_OK;
			}
			catch (Genode::Out_of_ram)  { return OPEN_ERR_OUT_OF_RAM; }
			catch (Genode::Out_of_caps) { return OPEN_ERR_OUT_OF_CAPS; }
		}

		Stat_result stat(char const *path, Stat &out) override
		{
			Stat_result result = Single_file_system::stat(path, out);
			return result;
		}


		/********************************
		 ** File I/O service interface **
		 ********************************/

		Ftruncate_result ftruncate(Vfs::Vfs_handle *handle, file_size) override
		{
			return FTRUNCATE_OK;
		}
};


class Vfs_cbe::Discard_snapshot_file_system : public Vfs::Single_file_system
{
	private:

		Wrapper &_w;

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper &w)
			:
				Single_vfs_handle(ds, fs, alloc, 0),
				_w(w)
			{ }

			Read_result read(char *, file_size, file_size &) override
			{
				return READ_ERR_IO;
			}

			Write_result write(char const *src, file_size count,
			                   file_size &out_count) override
			{
				out_count = 0;

				Genode::uint64_t id { 0 };
				Genode::ascii_to(src, id);
				if (id == 0) {
					return WRITE_ERR_IO;
				}
				Cbe::Snapshot_ID snap { .value = id, .valid = true };
				if (!_w.discard_snapshot(snap)) {
					return WRITE_ERR_IO;
				}

				out_count = count;
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Discard_snapshot_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::wo(), Xml_node("<discard_snapshot/>")),
			_w(w)
		{ }

		static char const *type_name() { return "discard_snapshot"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Genode::Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				*out_handle =
					new (alloc) Vfs_handle(*this, *this, alloc, _w);
				return OPEN_OK;
			}
			catch (Genode::Out_of_ram)  { return OPEN_ERR_OUT_OF_RAM; }
			catch (Genode::Out_of_caps) { return OPEN_ERR_OUT_OF_CAPS; }
		}

		Stat_result stat(char const *path, Stat &out) override
		{
			Stat_result result = Single_file_system::stat(path, out);
			return result;
		}

		/********************************
		 ** File I/O service interface **
		 ********************************/

		Ftruncate_result ftruncate(Vfs::Vfs_handle *handle, file_size) override
		{
			return FTRUNCATE_OK;
		}
};


class Vfs_cbe::Secure_superblock_file_system : public Vfs::Single_file_system
{
	private:

		Wrapper &_w;

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper &w)
			:
				Single_vfs_handle(ds, fs, alloc, 0),
				_w(w)
			{ }

			Read_result read(char *, file_size, file_size &) override
			{
				return READ_ERR_IO;
			}

			Write_result write(char const *src, file_size count,
			                   file_size &out_count) override
			{
				bool secure_superblock { false };
				Genode::ascii_to(src, secure_superblock);
				Genode::String<64> str(Genode::Cstring(src, count));

				if (!secure_superblock || true) {
					return WRITE_ERR_IO;
				}
				out_count = count;
				// _w.start_sealing_generation();
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Secure_superblock_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::wo(), Xml_node("<secure_superblock/>")),
			_w(w)
		{ }

		static char const *type_name() { return "secure_superblock"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Genode::Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				*out_handle =
					new (alloc) Vfs_handle(*this, *this, alloc, _w);
				return OPEN_OK;
			}
			catch (Genode::Out_of_ram)  { return OPEN_ERR_OUT_OF_RAM; }
			catch (Genode::Out_of_caps) { return OPEN_ERR_OUT_OF_CAPS; }
		}

		Stat_result stat(char const *path, Stat &out) override
		{
			Stat_result result = Single_file_system::stat(path, out);
			return result;
		}
};


struct Vfs_cbe::Snapshot_local_factory : File_system_factory
{
	Data_file_system _block_fs;

	Snapshot_local_factory(Vfs::Env    &env,
	                       Wrapper &cbe,
	                       uint32_t snap_id)
	: _block_fs(cbe, snap_id) { }

	Vfs::File_system *create(Vfs::Env&, Xml_node node) override
	{
		if (node.has_type(Data_file_system::type_name()))
			return &_block_fs;

		return nullptr;
	}
};


class Vfs_cbe::Snapshot_file_system : private Snapshot_local_factory,
                                      public Vfs::Dir_file_system
{
	private:

		Genode::uint32_t _snap_id;

		typedef String<128> Config;

		static Config _config(Genode::uint32_t snap_id, bool readonly)
		{
			char buf[Config::capacity()] { };

			Xml_generator xml(buf, sizeof(buf), "dir", [&] () {

				xml.attribute("name",
				              !readonly ? String<16>("current")
				                        : String<16>(snap_id));

				xml.node("data", [&] () {
					xml.attribute("readonly", readonly);
				});
			});

			return Config(Cstring(buf));
		}

	public:

		Snapshot_file_system(Vfs::Env        &vfs_env,
		                    Wrapper          &cbe,
		                    Genode::uint32_t  snap_id,
		                    bool              readonly = false)
		:
			Snapshot_local_factory(vfs_env, cbe, snap_id),
			Vfs::Dir_file_system(vfs_env,
			                     Xml_node(_config(snap_id, readonly).string()),
			                     *this),
			_snap_id(snap_id)
		{ }

		static char const *type_name() { return "snapshot"; }

		char const *type() override { return type_name(); }

		Genode::uint32_t snapshot_id() const
		{
			return _snap_id;
		}
};


class Vfs_cbe::Snapshots_file_system : public Vfs::File_system
{
	private:

		Vfs::Env &_vfs_env;

		bool _root_dir(char const *path) { return strcmp(path, "/snapshots") == 0; }
		bool _top_dir(char const *path) { return strcmp(path, "/") == 0; }

		struct Snapshot_registry
		{
			Genode::Allocator &_alloc;
			Wrapper &_w;

			uint32_t _number_of_snapshots { 0 };

			Genode::Registry<Genode::Registered<Snapshot_file_system>> _snap_fs { };

			Snapshot_registry(Genode::Allocator &alloc, Wrapper &w)
			: _alloc(alloc), _w(w)
			{ }

			void update(Vfs::Env &vfs_env)
			{
				Cbe::Active_snapshot_ids list { };
				_w.active_snapshot_ids(list);

				/* alloc new */
				for (size_t i = 0; i < sizeof (list.values) / sizeof (list.values[0]); i++) {
					uint32_t const id = list.values[i];
					if (!id) { continue; }

					bool is_old = false;
					auto find_old = [&] (Snapshot_file_system const &fs) {
						is_old |= (fs.snapshot_id() == id);
					};
					_snap_fs.for_each(find_old);

					if (!is_old) {
						new (_alloc) Genode::Registered<Snapshot_file_system>(
							_snap_fs, vfs_env, _w, id, true);
						++_number_of_snapshots;
					}
				}

				/* destroy old */
				auto find_stale = [&] (Snapshot_file_system const &fs) {
					bool is_stale = true;
					for (size_t i = 0; i < sizeof (list.values) / sizeof (list.values[0]); i++) {
						uint32_t const id = list.values[i];
						if (!id) { continue; }

						if (fs.snapshot_id() == id) {
							is_stale = false;
							break;
						}
					}

					if (is_stale) {
						destroy(&_alloc, &const_cast<Snapshot_file_system&>(fs));
						--_number_of_snapshots;
					}
				};
				_snap_fs.for_each(find_stale);
			}

			uint32_t number_of_snapshots() const { return _number_of_snapshots; }

			Snapshot_file_system const &by_index(uint32_t idx) const
			{
				uint32_t i = 0;
				Snapshot_file_system const *fsp { nullptr };
				auto lookup = [&] (Snapshot_file_system const &fs) {
					if (i == idx) {
						fsp = &fs;
					}
					++i;
				};
				_snap_fs.for_each(lookup);
				return *fsp;
			}

			Snapshot_file_system &by_id(uint32_t id)
			{
				Snapshot_file_system *fsp { nullptr };
				auto lookup = [&] (Snapshot_file_system &fs) {
					if (fs.snapshot_id() == id) {
						fsp = &fs;
					}
				};
				_snap_fs.for_each(lookup);
				return *fsp;
			}
		};

	public:

		struct Snap_vfs_handle : Vfs::Vfs_handle
		{
			using Vfs_handle::Vfs_handle;

			virtual Read_result read(char *dst, file_size count,
			                         file_size &out_count) = 0;

			virtual Write_result write(char const *src, file_size count,
			                           file_size &out_count) = 0;

			virtual Sync_result sync()
			{
				return SYNC_OK;
			}

			virtual bool read_ready() = 0;
		};


		struct Dir_vfs_handle : Snap_vfs_handle
		{
			Snapshot_registry const &_snap_reg;

			bool const _root_dir { false };

			Dir_vfs_handle(Directory_service &ds,
			               File_io_service   &fs,
			               Genode::Allocator &alloc,
			               int                status_flags,
			               Snapshot_registry const &snap_reg,
			               bool root_dir)
			:
				Snap_vfs_handle(ds, fs, alloc, status_flags),
				_snap_reg(snap_reg), _root_dir(root_dir)
			{ }

			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				out_count = 0;

				if (count < sizeof(Dirent))
					return READ_ERR_INVALID;

				file_size index = seek() / sizeof(Dirent);

				Dirent *out = (Dirent*)dst;

				if (!_root_dir) {
					if (index < _snap_reg.number_of_snapshots()) {
						out->fileno = (Genode::addr_t)this | index;
						out->type = Dirent_type::DIRECTORY;
						Snapshot_file_system const &fs = _snap_reg.by_index(index);
						Genode::String<32> name { fs.snapshot_id() };
						out->name = { name.string() };
					} else {
						out->type = Dirent_type::END;
					}
				} else {
					if (index == 0) {
						out->fileno = (Genode::addr_t)this;
						out->type = Dirent_type::DIRECTORY;
						out->name = { "snapshots" };
					} else {
						out->type = Dirent_type::END;
					}
				}

				out_count = sizeof(Dirent);

				return READ_OK;
			}

			Write_result write(char const *, file_size, file_size &) override
			{
				return WRITE_ERR_INVALID;
			}

			bool read_ready() override { return true; }

		};

		Snapshot_registry _snap_reg;

		char const *_sub_path(char const *path) const
		{
			/* skip heading slash in path if present */
			if (path[0] == '/') {
				path++;
			}

			Genode::size_t const name_len = strlen(type_name());
			if (strcmp(path, type_name(), name_len) != 0) {
				return nullptr;
			}

			path += name_len;

			/*
			 * The first characters of the first path element are equal to
			 * the current directory name. Let's check if the length of the
			 * first path element matches the name length.
			 */
			if (*path != 0 && *path != '/') {
				return 0;
			}

			return path;
		}


		Snapshots_file_system(Vfs::Env         &vfs_env,
		                      Genode::Xml_node  node,
		                      Wrapper          &w)
		: _vfs_env(vfs_env), _snap_reg(vfs_env.alloc(), w)
		{ }

		static char const *type_name() { return "snapshots"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory service interface **
		 *********************************/

		Dataspace_capability dataspace(char const *path)
		{
			return Genode::Dataspace_capability();
		}

		void release(char const *path, Dataspace_capability)
		{
		}

		Open_result open(char const       *path,
		                 unsigned          mode,
		                 Vfs::Vfs_handle **out_handle,
		                 Allocator        &alloc) override
		{
			path = _sub_path(path);
			if (!path || path[0] != '/') {
				return OPEN_ERR_UNACCESSIBLE;
			}
			path++;

			uint32_t id { 0 };
			Genode::ascii_to(path, id);
			Snapshot_file_system &fs = _snap_reg.by_id(id);
			return fs.open(path, mode, out_handle, alloc);
		}

		Opendir_result opendir(char const       *path,
		                       bool              create,
		                       Vfs::Vfs_handle **out_handle,
		                       Allocator        &alloc) override
		{
			if (create) {
				return OPENDIR_ERR_PERMISSION_DENIED;;
			}

			bool const root = strcmp(path, "/") == 0;
			if (_root_dir(path) || root) {

				_snap_reg.update(_vfs_env);

				*out_handle = new (alloc) Dir_vfs_handle(*this, *this, alloc, 0, _snap_reg, root);
				return OPENDIR_OK;
			}
			return OPENDIR_ERR_LOOKUP_FAILED;
		}

		void close(Vfs_handle *handle) override
		{
			if (handle && (&handle->ds() == this))
				destroy(handle->alloc(), handle);
		}

		Stat_result stat(char const *path, Stat &out_stat) override
		{
			out_stat = Stat { };
			path = _sub_path(path);

			/* path does not match directory name */
			if (!path)
				return STAT_ERR_NO_ENTRY;

			/*
			 * If path equals directory name, return information about the
			 * current directory.
			 */
			if (strlen(path) == 0 || _top_dir(path)) {

				out_stat.type   = Node_type::DIRECTORY;
				out_stat.inode  = 1;
				out_stat.device = (Genode::addr_t)this;
				return STAT_OK;
			}

			if (!path || path[0] != '/') {
				return STAT_ERR_NO_ENTRY;
			}

			/* strip / */
			path++;

			uint32_t id { 0 };
			Genode::ascii_to(path, id);
			Snapshot_file_system &fs = _snap_reg.by_id(id);
			return fs.stat(path, out_stat);

		}

		Unlink_result unlink(char const *path)
		{
			return UNLINK_ERR_NO_PERM;
		}

		Rename_result rename(char const *from, char const *to) override
		{
			return RENAME_ERR_NO_PERM;
		}

		file_size num_dirent(char const *path) override
		{
			return strcmp(path, "/") == 0 ? 1 : _snap_reg.number_of_snapshots();
		}

		bool directory(char const *path) override
		{
			if (Genode::strcmp(path, "/snapshots") == 0) {
				return true;
			}
			return false;
		}

		char const *leaf_path(char const *path) override
		{
			path = _sub_path(path);
			if (!path || path[0] != '/') {
				return nullptr;
			}
			path++;

			uint32_t id { 0 };
			Genode::ascii_to(path, id);
			Snapshot_file_system &fs = _snap_reg.by_id(id);
			char const *leaf_path = fs.leaf_path(path);
			if (leaf_path) {
				return leaf_path;
			}

			return nullptr;
		}


		/********************************
		 ** File I/O service interface **
		 ********************************/

		Write_result write(Vfs::Vfs_handle *vfs_handle,
		                   char const *buf, file_size buf_size,
		                   file_size &out_count) override
		{
			return WRITE_ERR_IO;
		}

		bool queue_read(Vfs::Vfs_handle *, file_size) override
		{
			return true;
		}

		Read_result complete_read(Vfs::Vfs_handle *vfs_handle,
		                          char *dst, file_size count,
		                          file_size & out_count) override
		{
			Snap_vfs_handle *handle =
				static_cast<Snap_vfs_handle*>(vfs_handle);

			if (handle)
				return handle->read(dst, count, out_count);

			return READ_ERR_IO;
		}

		bool read_ready(Vfs::Vfs_handle *) override
		{
			return true;
		}

		Ftruncate_result ftruncate(Vfs::Vfs_handle *vfs_handle,
		                           file_size len) override
		{
			return FTRUNCATE_OK;
		}

};


struct Vfs_cbe::Control_local_factory : File_system_factory
{
	Key_file_system               _key_fs;
	Create_snapshot_file_system   _create_snapshot_fs;
	Discard_snapshot_file_system  _discard_snapshot_fs;
	Secure_superblock_file_system _secure_superblock_fs;

	Control_local_factory(Vfs::Env     &env,
	                      Xml_node      config,
	                      Wrapper      &cbe)
	:
		_key_fs(cbe),
		_create_snapshot_fs(cbe),
		_discard_snapshot_fs(cbe),
		_secure_superblock_fs(cbe)
	{ }

	Vfs::File_system *create(Vfs::Env&, Xml_node node) override
	{
		if (node.has_type(Key_file_system::type_name())) {
			return &_key_fs;
		}

		if (node.has_type(Create_snapshot_file_system::type_name())) {
			return &_create_snapshot_fs;
		}

		if (node.has_type(Discard_snapshot_file_system::type_name())) {
			return &_discard_snapshot_fs;
		}

		if (node.has_type(Secure_superblock_file_system::type_name())) {
			return &_secure_superblock_fs;
		}

		return nullptr;
	}
};


class Vfs_cbe::Control_file_system : private Control_local_factory,
                                     public Vfs::Dir_file_system
{
	private:

		typedef String<128> Config;

		static Config _config(Xml_node node)
		{
			char buf[Config::capacity()] { };

			Xml_generator xml(buf, sizeof(buf), "dir", [&] () {
				xml.attribute("name", "control");
				xml.node("key", [&] () { });
				xml.node("create_snapshot", [&] () { });
				xml.node("discard_snapshot", [&] () { });
				xml.node("secure_superblock", [&] () { });
			});

			return Config(Cstring(buf));
		}

	public:

		Control_file_system(Vfs::Env         &vfs_env,
		                    Genode::Xml_node  node,
		                    Wrapper          &cbe)
		:
			Control_local_factory(vfs_env, node, cbe),
			Vfs::Dir_file_system(vfs_env, Xml_node(_config(node).string()),
			                     *this)
		{ }

		static char const *type_name() { return "control"; }

		char const *type() override { return type_name(); }
};


struct Vfs_cbe::Local_factory : File_system_factory
{
	Snapshot_file_system _current_snapshot_fs;
	Snapshots_file_system _snapshots_fs;
	Control_file_system _control_fs;

	Local_factory(Vfs::Env &env, Xml_node config,
	              Wrapper &cbe)
	:
		_current_snapshot_fs(env, cbe, 0, false),
		_snapshots_fs(env, config, cbe),
		_control_fs(env, config, cbe)
	{ }

	Vfs::File_system *create(Vfs::Env&, Xml_node node) override
	{
		using Name = String<64>;
		if (node.has_type(Snapshot_file_system::type_name())
		    && node.attribute_value("name", Name()) == "current")
			return &_current_snapshot_fs;

		if (node.has_type(Control_file_system::type_name()))
			return &_control_fs;

		if (node.has_type(Snapshots_file_system::type_name()))
			return &_snapshots_fs;

		return nullptr;
	}
};


class Vfs_cbe::File_system : private Local_factory,
                             public Vfs::Dir_file_system
{
	private:

		Wrapper &_wrapper;

		typedef String<256> Config;

		static Config _config(Xml_node node)
		{
			char buf[Config::capacity()] { };

			Xml_generator xml(buf, sizeof(buf), "dir", [&] () {
				typedef String<64> Name;

				xml.attribute("name", node.attribute_value("name", Name()));

				xml.node("control", [&] () { });

				xml.node("snapshot", [&] () {
					xml.attribute("name", "current");
				});

				xml.node("snapshots", [&] () { });
			});

			return Config(Cstring(buf));
		}

	public:

		File_system(Vfs::Env &vfs_env, Genode::Xml_node node,
		            Wrapper &wrapper)
		:
			Local_factory(vfs_env, node, wrapper),
			Vfs::Dir_file_system(vfs_env, Xml_node(_config(node).string()),
			                     *this),
			_wrapper(wrapper)
		{ }

		~File_system()
		{
			// XXX rather then destroying the wrapper here, it should be
			//     done on the out-side where it was allocated in the first
			//     place but the factory interface does not support that yet
			// destroy(vfs_env.alloc().alloc()), &_wrapper);
		}
};


/******************************
 ** Cbe::Time implementation **
 ******************************/

void Cbe::Time::_handle_sync_timeout(Genode::Duration)
{
	warning("Cbe::Time::_handle_sync_timeout not implemented");
}


void Cbe::Time::_handle_secure_timeout(Genode::Duration)
{
	warning("Cbe::Time::_handle_secure_timeout not implemented");
}


Cbe::Time::Time(Genode::Env &env) : _timer(env) { }


Cbe::Time::Timestamp Cbe::Time::timestamp()
{
	return _timer.curr_time().trunc_to_plain_ms().value;
}


/**************************
 ** VFS plugin interface **
 **************************/

extern "C" Vfs::File_system_factory *vfs_file_system_factory(void)
{
	struct Factory : Vfs::File_system_factory
	{
		Vfs::File_system *create(Vfs::Env &vfs_env,
		                         Genode::Xml_node node) override
		{
			try {
				/* XXX wrapper is not managed and will leak */
				Vfs_cbe::Wrapper *wrapper =
					new (vfs_env.alloc()) Vfs_cbe::Wrapper { vfs_env, node };
				return new (vfs_env.alloc())
					Vfs_cbe::File_system(vfs_env, node, *wrapper);
			} catch (...) {
				Genode::error("could not create 'cbe_fs' ");
			}
			return nullptr;
		}
	};

	/* the CBE library requires a stack larger than the default */
	Genode::Thread::myself()->stack_size(64*1024);

	adainit();

	Cbe::assert_valid_object_size<Cbe::Library>();

	static Factory factory;
	return &factory;
}


/*
 * The SPARK compiler might generate a call to memcmp when it wants to
 * compare objects. For the time being we implement here and hopefully
 * any other memcmp symbol has at least the same semantics.
 */
extern "C" int memcmp(const void *s1, const void *s2, Genode::size_t n)
{
	return Genode::memcmp(s1, s2, n);
}
