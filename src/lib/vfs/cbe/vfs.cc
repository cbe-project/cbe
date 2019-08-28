
#include <vfs/dir_file_system.h>
#include <vfs/single_file_system.h>

#include <util/xml_generator.h>

namespace Vfs_cbe {
	using namespace Vfs;
	using namespace Genode;

  class  Block_file_system;
	struct Local_factory;
	class  File_system;
}


class Vfs_cbe::Block_file_system : public Single_file_system
{
	private:

		typedef String<32> Config;

		static Config _config()
		{
			char buf[Config::capacity()] { };
			Xml_generator xml(buf, sizeof(buf), type_name(), [&] () { });

			return Config(Cstring(buf));
		}

	public:

		struct Vfs_handle : Single_vfs_handle
		{
			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc)
			: Single_vfs_handle(ds, fs, alloc, 0)
			{ }

			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				Genode::warning(__PRETTY_FUNCTION__, " called");
				out_count = 0;
				return READ_ERR_INVALID;
			}

			Write_result write(char const *, file_size,
			                   file_size &out_count) override
			{
				Genode::warning(__PRETTY_FUNCTION__, " called");
				out_count = 0;
				return WRITE_ERR_INVALID;
			}

			bool read_ready() override
			{
				Genode::warning(__PRETTY_FUNCTION__, " called");
				return false;
			}
		};

		Block_file_system(Vfs::Env &env)
		: Single_file_system(NODE_TYPE_BLOCK_DEVICE, type_name(), _config().string()) { }


		/***************************
		 ** File-system interface **
		 ***************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			warning(__func__, " called: ", path);
			*out_handle = new (alloc) Vfs_handle(*this, *this, alloc);

			return OPEN_OK;
		}

		static char const *type_name() { return "block"; }
		char const *type() override { return type_name(); }
};


struct Vfs_cbe::Local_factory : File_system_factory
{
	Block_file_system _block_fs;

	Local_factory(Vfs::Env &env, Xml_node config)
	: _block_fs(env) { }

	Vfs::File_system *create(Vfs::Env&, Xml_node node) override
	{
		Genode::log(__func__, node);

		if (node.has_type(Block_file_system::type_name()))
			return &_block_fs;

		return nullptr;
	}
};


class Vfs_cbe::File_system : private Local_factory,
                             public Vfs::Dir_file_system
{
	private:

		typedef String<200> Config;

		static Config _config(Xml_node node)
		{
			char buf[Config::capacity()] { };
			/*
			 * TODO: Add snapshots
			 */
			Xml_generator xml(buf, sizeof(buf), "dir", [&] () {
				typedef String<64> Name;
				xml.attribute("name", node.attribute_value("name", Name()));
				xml.node("block", [&] () { });
			});

			Genode::log("CBE-dir XML:\n", (char const *) buf);
			return Config(Cstring(buf));
		}

	public:

		File_system(Vfs::Env &vfs_env, Genode::Xml_node node)
		: Local_factory(vfs_env, node),
		  Vfs::Dir_file_system(vfs_env, Xml_node(_config(node).string()), *this)
		{ }
};


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
			try { return new (vfs_env.alloc())
				Vfs_cbe::File_system(vfs_env, node); }
			catch (...) { Genode::error("could not create 'cbe_fs' "); }
			return nullptr;
		}
	};

	static Factory factory;
	return &factory;
}
