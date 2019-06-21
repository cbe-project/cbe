/*
 * \brief  CBE block C++ prototype
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
#include <base/attached_rom_dataspace.h>
#include <base/component.h>
#include <base/heap.h>
#include <base/attached_ram_dataspace.h>
#include <block/request_stream.h>
#include <root/root.h>
#include <util/misc_math.h>
#include <os/reporter.h>

/* repo includes */
#include <util/sha256_4k.h>

/* cbe */
#include <cbe/types.h>

using uint32_t = Genode::uint32_t;
using uint64_t = Genode::uint64_t;

namespace Cbe {

	enum {
		BLOCK_DEVICE_SIZE = 1u << 30,
	};

	struct Size       { uint64_t value; };
	struct Block_size { uint32_t value; };

	struct Block_allocator;

	class Tree;

	template <Genode::size_t, typename> class Stack;

	struct Block_session_component;
	class Main;

	struct Node_type2
	{
		union {
			struct {
				Cbe::Physical_block_address physical_block_address;
				Cbe::Virtual_block_address last_vba;

				Generation alloc_gen;
				Generation free_gen;
				uint32_t last_key_id;
				bool reserved;
			};

			char data[64];
		};
	} __attribute__((packed));


	struct Data
	{
		void *base;
		Genode::size_t size;
		enum { BLOCK_SIZE = Cbe::BLOCK_SIZE };
	};

	class Vbd;
	class Mmu;

	enum { FIRST_PBA = 128, };

	using namespace Genode;

} /* namespace Cbe */


template <Genode::size_t N, typename T>
class Cbe::Stack
{
	private:

		T _data[N];
		Genode::size_t _count;

	public:

		struct Stack_empty { };
		struct Stack_full { };

		Stack() : _count(0) { }

		bool empty() const { return _count == 0; }

		T pop()
		{
			if (!_count) { throw Stack_empty(); }
			return _data[--_count];
		}

		void push(T d)
		{
			if (_count >= N) { throw Stack_full(); }
			_data[_count++] = d;
		}

		T peek() const
		{
			if (empty()) { throw Stack_empty(); }
			return _data[_count-1];
		}
};


struct Cbe::Block_allocator
{
	struct Out_of_blocks { };
	struct Invalid_physical_block_address { };

	Data const _data;

	Cbe::Physical_block_address const _start;

	Cbe::Physical_block_address _current;
	Cbe::Physical_block_address _avail;

	Block_allocator(Data const data, Cbe::Physical_block_address const start, Cbe::Physical_block_address const avail)
	: _data(data), _start(start), _current(start), _avail(avail)
	{
		/* assert _avail > 0 */
		if (!_avail) { throw Out_of_blocks(); }

		if (1) {
			log(__func__, ": avail: ", _avail, " start index: ", _start);
		}
	}

	void *data(Cbe::Physical_block_address const pba)
	{
		Cbe::Physical_block_address const count = _data.size / Data::BLOCK_SIZE;
		/* assert count + start > start */
		if (pba > (count + _start)) {
			error(__func__, ": ", pba, " (", count, ",", _start, ")");
			throw Invalid_physical_block_address();
		}
		return (char*)_data.base + (pba * Data::BLOCK_SIZE);
	}

	Cbe::Physical_block_address alloc()
	{
		if (_avail == 0) { throw Out_of_blocks(); }

		Cbe::Physical_block_address pba = _current;
		++_current;
		--_avail;

		return pba;
	}
};


struct Cbe::Block_session_component : Rpc_object<Block::Session>,
                                      Block::Request_stream
{
	Entrypoint &_ep;

	Block_session_component(Region_map               &rm,
	                        Dataspace_capability      ds,
	                        Entrypoint               &ep,
	                        Signal_context_capability sigh,
	                        uint64_t                  block_count)
	:
		Request_stream(rm, ds, ep, sigh,
		               Info { .block_size  = Cbe::BLOCK_SIZE,
		                      .block_count = block_count,
		                      .align_log2  = Genode::log2((uint64_t)Cbe::BLOCK_SIZE),
		                      .writeable    = true }),
		_ep(ep)
	{
		_ep.manage(*this);
	}

	~Block_session_component() { _ep.dissolve(*this); }

	Info info() const override { return Request_stream::info(); }

	Capability<Tx> tx_cap() override { return Request_stream::tx_cap(); }
};


struct Old_entry
{
	Cbe::Physical_block_address         physical_block_address;
	Cbe::Virtual_block_address         vba;
	Cbe::Generation  g;
};


Cbe::Physical_block_address find_free_type2(Cbe::Virtual_block_address start)
{
	(void)start;
	return 0;
}


#if 0
static uint32_t _global_current_key_id = 42;
static uint64_t _global_current_generation = 1;

bool allocte_new_blocks(uint64_t n, Old_entry e[])
{
	uint64_t n_total = n;
	uint64_t n_found = 0;

	/* we are looking only for 1 branch that satisfies the request */
	Cbe::Virtual_block_address b    = 0;
	uint64_t nvol = 0;

	Cbe::Virtual_block_address start = 0;
	Cbe::Virtual_block_address const step = 0b0100;

	do {

		Cbe::Physical_block_address x = find_free_type2(start);
		start +=step;

		uint64_t free = get_free_blocks(x);
		uint64_t nvol = get_non_volatile_blocks(x);

		n_found += free;
		n_total += nvol;

		b = x;
	} while (n_found < n_total);

	Cbe::Node_type2 &nt2 = *reinterpret_cast<Cbe::Node_type2*>(_block_allocator.data(b));

	uint64_t nvcol_i = 0;
	for (uint32_t i = height; i > 0; i--) {
		if (block_volatile(x, i)) {
			nt2[nvcol_i].pba = 

	}
	/* assert nvcol_i == nvol */

	for (uint64_t i = 0; i < n; i++) {
		nt2[nvol+i].pba         = e[i].pba;
		nt2[nvol+i].last_vba    = e[i].vba;
		nt2[nvol+i].alloc_gen   = e[i].g;
		nt2[nvol+i].free_gen    = _global_current_generation;
		nt2[nvol+i].last_key_id = _global_current_key_id;
		nt2[nvol+i].reserved    = true;
	}
}
#endif


class Cbe::Tree
{
	public:

		struct Invalid_virtual_block_address { };

		struct Outer_degree { uint32_t value; };

		struct Info
		{
			uint32_t outer_degree;
			uint32_t height;
			uint64_t max_size;
			uint32_t block_size;

			uint64_t md_size;
			uint64_t size;
			uint64_t leaves;

			void print(Genode::Output &out) const
			{
				Genode::print(out, "outer_degree: ", outer_degree);
				Genode::print(out, " height: ", height);
				Genode::print(out, " max_size: ", max_size);
				Genode::print(out, " block_size: ", block_size);
				Genode::print(out, " md_size: ", md_size);
				Genode::print(out, " size: ", size);
				Genode::print(out, " leaves: ", leaves);
			};
		};

		struct Node_id { uint64_t value; };

	private:

		Cbe::Block_allocator &_block_allocator;
		Cbe::Physical_block_address              _root;
		Info const            _info;

		static Node_id _get_node(Cbe::Virtual_block_address const  vba,
		                          uint32_t const level,
		                          uint32_t const degree)
		{
			uint32_t const bits = Genode::log2(degree);
			uint32_t const mask = (1u << bits) - 1;
			uint32_t const l    = level-1;
			uint64_t const i    = (vba >> (bits*l)) & mask;

			return Node_id { i };
		}

	public:

		Tree(Cbe::Block_allocator &bc, Cbe::Physical_block_address root, Info const info)
		: _block_allocator(bc), _root(root), _info(info) { }

		static Info calculate(Outer_degree    outer_degree,
		                      Cbe::Block_size block_size,
		                      Cbe::Size       size)
		{
			uint32_t height = 1;
			Size max_size { outer_degree.value * block_size.value };

			while (max_size.value < size.value) {
				max_size.value *= outer_degree.value;
				height++;
			}

			/* crude approximation of the needed meta-data size */
			uint32_t md_size = 1;
			for (uint32_t i = 1; i < height; i++) {
				md_size *= outer_degree.value;
			};
			md_size *= outer_degree.value;
			md_size /= (max_size.value / size.value);

			return Info { .outer_degree = outer_degree.value,
			              .height       = height,
			              .max_size     = max_size.value,
			              .block_size   = block_size.value,
			              .md_size      = md_size,
			              .size         = size.value,
			              .leaves       = size.value / block_size.value
			};
		}

		Cbe::Physical_block_address    root() const { return _root; }
		Info const &info() const { return _info; }
		Cbe::Block_allocator &block_allocator() { return _block_allocator; }

		Cbe::Physical_block_address lookup(Virtual_block_address const vba) const
		{
			if (vba >= _info.leaves) { throw Invalid_virtual_block_address(); }

			uint32_t height = _info.height;
			uint32_t degree = _info.outer_degree;

			Node_id inner_pba[height];
			Cbe::Physical_block_address pba { ~0ull };

			/*
			 * First we collect all ids of the inner nodes by parsing the Virtual_block_address,
			 * then we access all physical blocks starting from the root block
			 * until we end up at the leaf node.
			 */

			for (uint32_t i = height; i > 0; i--) {
				inner_pba[i-1] = _get_node(vba, i, degree);
			}

			Cbe::Type_i_node* node = reinterpret_cast<Cbe::Type_i_node*>(_block_allocator.data(_root));
			pba = node[inner_pba[height-1].value].pba;

			for (uint32_t i = 2; i <= height; i++) {
				Cbe::Type_i_node* node = reinterpret_cast<Cbe::Type_i_node*>(_block_allocator.data(pba));
				pba = node[inner_pba[height-i].value].pba;
			}

			return pba;
		}
};


class Cbe::Vbd
{
	private:

		struct Bad_i_node_child_id : Genode::Exception { };

		Genode::Env                &_env;
		Genode::Expanding_reporter  _reporter      { _env, "state", "state" };
		uint64_t                    _dump_leaves   { 0 };
		uint64_t                    _report_leaves { 0 };
		uint64_t                    _leaves        { 0 };
		Cbe::Tree                  &_tree;
		bool                 const  _verbose;

		/**
		 * Report information about a given leaf node and its sub-nodes
		 */
		bool _report_leaf(Genode::Xml_generator             &xml,
		                  Cbe::Tree::Info             const &info,
		                  Cbe::Physical_block_address const  parent,
		                  Cbe::Block_allocator              &blk_alloc)
		{
			/* finish early in case we already initialized all required leaves */
			if (_report_leaves >= info.leaves) {
				return true; }

			bool finished { false };
			Type_i_node *parent_node {
				reinterpret_cast<Cbe::Type_i_node*>(blk_alloc.data(parent)) };

			for (uint32_t id = 0; id < info.outer_degree; id++) {
				xml.node("leaf", [&] () {
					xml.attribute("id",   id);
					xml.attribute("pba",  parent_node[id].pba);
					xml.attribute("gen",  parent_node[id].gen);
					xml.attribute("vba",  _report_leaves);
					xml.attribute("hash", Hash::String(parent_node[id].hash));
				});
				if ((++_report_leaves) >= info.leaves) {
					finished = true;
					break;
				}
			}
			return finished;
		}

		/**
		 * Report information about a given type-i node and its sub-nodes
		 */
		bool _report_i_node(Genode::Xml_generator             &xml,
		                    Cbe::Tree::Info             const &info,
		                    Cbe::Physical_block_address const  parent,
		                    Cbe::Block_allocator              &blk_alloc,
		                    uint32_t                           height)
		{
			height--;
			bool finished { false };
			Cbe::Type_i_node *node {
				reinterpret_cast<Cbe::Type_i_node*>(blk_alloc.data(parent)) };

			for (uint32_t id = 0; id < info.outer_degree && !finished; id++) {
				xml.node("i-node", [&] () {
					xml.attribute("id",   id);
					xml.attribute("pba",  node[id].pba);
					xml.attribute("gen",  node[id].gen);
					xml.attribute("hash", Hash::String(node[id].hash));

					finished = (height == 1) ?
						_report_leaf(xml, info, node[id].pba, blk_alloc) :
						_report_i_node(xml, info, node[id].pba, blk_alloc, height);
				});
			}
			return finished;
		}

		/**
		 * Report information about a given super-block and its sub-nodes
		 */
		void _report_super_block(Genode::Xml_generator &xml,
		                         uint64_t               sb_id,
		                         uint64_t               curr_sb_id,
		                         Block_allocator       &blk_alloc)
		{
			/* get reference to super-block */
			Cbe::Super_block &sb {
				*reinterpret_cast<Cbe::Super_block*>(blk_alloc.data(sb_id)) };

			/* add super-block tag */
			xml.node("super-block", [&] () {
				xml.attribute("id",          sb_id);
				xml.attribute("is_current",  sb_id == curr_sb_id);
				xml.attribute("generation",  sb.generation);
				xml.attribute("leafs",       sb.leaves);
				xml.attribute("degree",      sb.degree);
				xml.attribute("height",      sb.height);
				xml.attribute("free-number", sb.free_number);
				xml.attribute("free-leafs",  sb.free_leaves);
				xml.attribute("free-height", sb.free_height);
				xml.attribute("root-number", sb.root_number);

				/* skip sub-nodes for invalid super blocks */
				if (!sb.valid()) {
					return; }

				/* report information about sub-nodes */
				xml.attribute("root-hash", Hash::String(sb.root_hash));
				_report_leaves = 0;
				_report_i_node(xml, _tree.info(), sb.root_number,
				               _tree.block_allocator(), _tree.info().height);
			});
		}

		/**
		 * Report state of the super blocks and trees of the block device
		 */
		void _report(Genode::Xml_generator &xml)
		{
			/* determine the most recent super block */
			Block_allocator &ba { _tree.block_allocator() };
			Cbe::Generation most_recent_gen { 0 };
			uint64_t curr_sb_id { ~0ull };
			for (uint64_t sb_id = 0; sb_id < Cbe::NUM_SUPER_BLOCKS; sb_id++) {

				Cbe::Super_block &sb {
					*reinterpret_cast<Cbe::Super_block*>(ba.data(sb_id)) };

				Cbe::Generation const gen { sb.generation };
				if (gen >= most_recent_gen) {
					curr_sb_id = sb_id;
					most_recent_gen = gen;
				}
			}
			/* iterate over super blocks and report each of them */
			for (uint64_t sb_id = 0; sb_id < Cbe::NUM_SUPER_BLOCKS; sb_id++) {
				_report_super_block(xml, sb_id, curr_sb_id, ba); }
		}

		/**
		 * Add a child and its sub-tree given through XML to a type-i node
		 */
		void _write_i_node_child(Cbe::Type_i_node      *node,
		                         Xml_node        const &child_xml,
		                         Cbe::Tree::Info const &info,
		                         uint32_t               height,
		                         Cbe::Block_allocator  &blk_alloc)
		{
			/* get index of child info inside the i-node */
			uint32_t const child_id {
				child_xml.attribute_value("id", info.outer_degree + 1) };

			/* skip children with invalid index */
			if (child_id > info.outer_degree) {
				throw Bad_i_node_child_id(); }

			/* read child info from config to buffer */
			Cbe::Type_i_node child;
			child_xml.attribute("pba").value(&child.pba);
			child_xml.attribute("gen").value(&child.gen);

			/* write child sub-tree */
			if (height) {
				_write_i_node(child_xml, blk_alloc, info, height, child.pba); }

			/* get reference to child node */
			Sha256_4k::Data &data {
				*reinterpret_cast<Sha256_4k::Data*>(
					blk_alloc.data(child.pba)) };

			/* calculate and buffer child hash */
			Sha256_4k::Hash hash { };
			Sha256_4k::hash(data, hash);
			Genode::memcpy(child.hash.values, hash.values, sizeof(hash));

			/* write buffer content to the parent i-node */
			node[child_id] = child;
		}

		/**
		 * Write a type-i node given through XML to the device
		 */
		void _write_i_node(Xml_node                    const &xml,
		                   Cbe::Block_allocator              &blk_alloc,
		                   Cbe::Tree::Info             const &info,
		                   uint32_t                           height,
		                   Cbe::Physical_block_address const  pba)
		{
			/* get base address of i-node */
			Cbe::Type_i_node *node {
				reinterpret_cast<Cbe::Type_i_node*>(blk_alloc.data(pba)) };

			/* write info of all child i-nodes and write their sub-trees */
			height--;
			char const *child_type { height ? "i-node" : "leaf" };
			xml.for_each_sub_node(child_type, [&] (Xml_node const &child_xml) {
				try { _write_i_node_child(node, child_xml, info, height, blk_alloc); }
				catch (...) {
					warning("failed to write ", child_type, " according to config");
				}
			});
		}

		/**
		 * Write a super block and its sub-tree given through XML
		 */
		void _write_super_block(Xml_node        const &xml,
		                        Cbe::Tree::Info const &info,
		                        Cbe::Block_allocator  &blk_alloc)
		{
			/* read super-block information from config to buffer */
			Cbe::Super_block sb;
			xml.attribute("generation" ).value(&sb.generation);
			xml.attribute("leafs"      ).value(&sb.leaves);
			xml.attribute("degree"     ).value(&sb.degree);
			xml.attribute("height"     ).value(&sb.height);
			xml.attribute("free-number").value(&sb.free_number);
			xml.attribute("free-leafs" ).value(&sb.free_leaves);
			xml.attribute("free-height").value(&sb.free_height);
			xml.attribute("root-number").value(&sb.root_number);

			/* consider root node and sub-tree only for valid super blocks */
			if (sb.valid()) {

				/* write root node and its sub-tree */
				_write_i_node(xml, blk_alloc, info, info.height, sb.root_number);

				/* get reference to root */
				Sha256_4k::Data &data {
					*reinterpret_cast<Sha256_4k::Data*>(
						blk_alloc.data(sb.root_number)) };

				/* calculate and buffer root hash */
				Sha256_4k::Hash hash { };
				Sha256_4k::hash(data, hash);
				Genode::memcpy(sb.root_hash.values, hash.values, sizeof(hash));
			}
			/* write buffer content to block device */
			*reinterpret_cast<Cbe::Super_block*>(
				blk_alloc.data(xml.attribute_value(
					"id", (Cbe::Physical_block_address)0))) = sb;
		}

		bool _dump_data(Cbe::Tree::Info             const &info,
		                Cbe::Physical_block_address const  parent,
		                Cbe::Block_allocator              &block_allocator)
		{
			/* finish early in case we already initialized all required leaves */
			if (_dump_leaves >= info.leaves) { return true; }

			bool finished = false;

			Type_i_node *parent_node = reinterpret_cast<Cbe::Type_i_node*>(block_allocator.data(parent));
			for (uint32_t i = 0; i < info.outer_degree; i++) {
				Cbe::Physical_block_address const pba = parent_node[i].pba;
				Cbe::Generation             const v   = parent_node[i].gen;
				Cbe::Hash                   const &hash = parent_node[i].hash;

				log("leave[", i, "]: vba: ", _dump_leaves, " pba: ", pba, " ", Hex(v), " <", hash, ">");

				++_dump_leaves;
				if (_dump_leaves >= info.leaves) {
					finished = true;
					break;
				}
			}
			return finished;
		}

		bool _dump(Cbe::Tree::Info                   const &info,
		                 Cbe::Physical_block_address const  parent,
		                 Cbe::Block_allocator              &block_allocator,
		                 uint32_t                           height)
		{
			log("parent: ", parent);

			height--;
			bool finished = false;
			if (height == 0) {
				for (uint32_t i = 0; i < info.outer_degree; i++) {
					finished = _dump_data(info, parent, block_allocator);
					if (finished) { break; }
				}
				return finished;
			}

			Cbe::Type_i_node *node = reinterpret_cast<Cbe::Type_i_node*>(block_allocator.data(parent));
			for (uint32_t i = 0; i < info.outer_degree; i++) {
				Cbe::Physical_block_address const pba = node[i].pba;
				Cbe::Generation             const v   = node[i].gen;
				Cbe::Hash                   const &hash = node[i].hash;

				log("child[", i, "]: ", pba, " ", Hex(v), " <", hash, ">");

				finished = height == 1 ? _dump_data(info, node[i].pba, block_allocator)
				                       : _dump(info, node[i].pba, block_allocator, height);
				if (finished) { break; }
			}

			return finished;
		}

		bool _initialize_data(Cbe::Tree::Info             const &info,
		                      Cbe::Physical_block_address const  parent,
		                      Cbe::Block_allocator              &block_allocator)
		{
			/* finish early in case we already initialized all required leaves */
			if (_leaves >= info.leaves) { return true; }

			bool finished = false;

			Type_i_node *parent_node = reinterpret_cast<Cbe::Type_i_node*>(block_allocator.data(parent));
			for (uint32_t i = 0; i < info.outer_degree; i++) {

				try {
					Cbe::Physical_block_address const pba = block_allocator.alloc();
					Cbe::Generation             const v   = Cbe::GEN_TYPE_CHILD + 1;

					parent_node[i].pba = pba;
					parent_node[i].gen = v;

					Sha256_4k::Data *data = reinterpret_cast<Sha256_4k::Data*>(block_allocator.data(pba));
					Genode::memset(data, 0x42, sizeof (*data));
					Sha256_4k::Hash hash { };
					Sha256_4k::hash(*data, hash);

					Genode::memcpy(parent_node[i].hash.values, hash.values, sizeof (hash));

					if (_verbose) {
						log("leave[", i, "]: vba: ", _leaves, " pba: ", pba,
						    " ", Hex(v), " <", hash, ">");
					}
				} catch (...) {
					error(": could not allocate leave ", i, " for ", parent);
					throw;
				}

				++_leaves;
				if (_leaves >= info.leaves) {
					finished = true;
					break;
				}
			}
			return finished;
		}

		bool _initialize(Cbe::Tree::Info             const &info,
		                 Cbe::Physical_block_address const  parent,
		                 Cbe::Block_allocator              &block_allocator,
		                 uint32_t                           height)
		{
			if (_verbose) { log("parent: ", parent); }

			Cbe::Type_i_node *node = reinterpret_cast<Cbe::Type_i_node*>(block_allocator.data(parent));

			height--;
			bool finished = false;
			if (height == 0) {
				for (uint32_t i = 0; i < info.outer_degree; i++) {
					finished = _initialize_data(info, parent, block_allocator);
					if (finished) { break; }
				}
				return finished;
			}

			for (uint32_t i = 0; i < info.outer_degree; i++) {
				try {
					Cbe::Physical_block_address const pba = block_allocator.alloc();
					Cbe::Generation             const v   = GEN_TYPE_PARENT + 1;

					node[i].pba = pba;
					node[i].gen = v;

					if (_verbose) { log("child[", i, "]: ", pba, " ", Hex(v)); }
				} catch (...) {
					error(": could not allocate child ", i, " for ", parent);
					throw;
				}

				finished = height == 1 ? _initialize_data(info, node[i].pba, block_allocator)
				                       : _initialize(info, node[i].pba, block_allocator, height);

				Sha256_4k::Data *data = reinterpret_cast<Sha256_4k::Data*>(block_allocator.data(node[i].pba));
				Sha256_4k::Hash hash { };
				Sha256_4k::hash(*data, hash);
				Genode::memcpy(node[i].hash.values, hash.values, sizeof (hash));

				if (_verbose) {
					Cbe::Physical_block_address const pba = node[i].pba;
					Cbe::Generation             const v   = node[i].gen;
					log("child[", i, "]: ", pba, " ", Hex(v), " <", hash, ">");
				}
				if (finished) { break; }
			}

			return finished;
		}

	public:

		Vbd(Genode::Env &env,
		    Cbe::Tree   &tree,
		    bool         verbose,
		    bool         initialize)
		:
			_env     { env },
			_tree    { tree },
			_verbose { verbose }
		{
			if (initialize) {
				_initialize(_tree.info(), _tree.root(), _tree.block_allocator(), _tree.info().height);
			}
		}

		/**
		 * Apply a block-device state given through XML
		 */
		void write_state(Xml_node const &xml)
		{
			/* write all super blocks and their sub-trees */
			xml.for_each_sub_node("super-block", [&] (Xml_node const &sb_xml) {
				try {
					/* write current super block and its sub-tree */
					_write_super_block(sb_xml, _tree.info(),
					                   _tree.block_allocator());
				} catch (...) {
					warning("failed to write super-block according to config");
				}
			});
		}

		/**
		 * Report state of the super blocks and trees of the block device
		 */
		void report()
		{
			try {
				_reporter.generate([&] (Genode::Xml_generator &xml) {
					_report(xml); });
			} catch (Xml_generator::Buffer_exceeded) {
				error("failed to generate report"); }
		}

		void dump(bool all)
		{
			Block_allocator &ba = _tree.block_allocator();
			Cbe::Generation most_recent_gen = 0;
			uint64_t idx = ~0ull;
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				Cbe::Super_block &sb = *reinterpret_cast<Cbe::Super_block*>(ba.data(i));
				Cbe::Generation const gen  = sb.generation;
				if (gen >= most_recent_gen) {
					most_recent_gen = gen;
					idx = i;
				}
			}

			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {


				if (!all && i != idx) { continue; }

				Cbe::Super_block &sb = *reinterpret_cast<Cbe::Super_block*>(ba.data(i));

				Cbe::Generation              const gen  = sb.generation;
				Cbe::Physical_block_address  const root = sb.root_number;

				if (gen == 0 && root == 0) {
					Genode::log("          SB[", i, "] invalid");
					continue;
				}
				Cbe::Hash                    const &root_hash = sb.root_hash;
				Cbe::Number_of_leaves const free_height = sb.free_height;
				Cbe::Number_of_leaves const free_leaves = sb.free_leaves;
				Genode::log(i == idx ? "\033[33;1mCurrent " : "          ", "SB[", i, "]: gen: ", gen, " root: ", root,
				            " free leaves: (", free_leaves, "/", free_height, ")",
				            " root hash: <", root_hash, ">");

				_dump_leaves = 0;
				_dump(_tree.info(), sb.root_number, _tree.block_allocator(), _tree.info().height);
			}
		}

		Cbe::Physical_block_address lookup(Cbe::Virtual_block_address const vba)
		{
			return _tree.lookup(vba);
		}

		void *data(Cbe::Physical_block_address const pba)
		{
			return _tree.block_allocator().data(pba);
		}

		uint64_t block_count() const { return _tree.info().leaves; }
		uint32_t block_size()  const { return Cbe::BLOCK_SIZE; }
};


class Cbe::Mmu
{
	private:

		Cbe::Vbd       &_vbd;
		bool     const  _report;
		bool     const  _verbose;
		bool     const  _dump_all;
		Block::Request  _current_request { };
		bool            _completed       { false };
		bool            _pending         { false };
		void           *_data            { nullptr };

	public:

		Mmu(Cbe::Vbd &vbd,
		    bool      report,
		    bool      verbose,
		    bool      dump_all)
		:
			_vbd      { vbd },
			_report   { report },
			_verbose  { verbose },
			_dump_all { dump_all }
		{ }

		bool acceptable() const
		{
			return !_current_request.operation.valid();
		}

		void submit_request(Block::Request &request, void *data)
		{
			_current_request = request;
			_data            = data;
			_completed       = false;
			_pending         = true;
		}

		bool execute()
		{
			if (!_current_request.operation.valid() || !_pending) { return false; }

			if (_current_request.operation.type == Block::Operation::Type::SYNC) {
				_completed = true;
				return true;
			}

			Cbe::Virtual_block_address vba = _current_request.operation.block_number;
			Cbe::Physical_block_address physical_block_address = 0;

			bool result = true;
			try {
				/* XXX don't do lookups, use it as a normal block device */
				physical_block_address = vba; //_vbd.lookup(vba);

				void *dst = nullptr, *src = nullptr;
				if (_current_request.operation.type == Block::Operation::Type::READ) {
					src = _vbd.data(physical_block_address);
					dst = _data;
				} else if (_current_request.operation.type == Block::Operation::Type::WRITE) {
					src = _data;
					dst = _vbd.data(physical_block_address);
				} else {
					error("BUG");
					throw -1;
				}

				Genode::memcpy(dst, src, _vbd.block_size());
			} catch (Cbe::Tree::Invalid_virtual_block_address) {
				result = false;
			}

			if (vba < 8 && _current_request.operation.type == Block::Operation::Type::WRITE) {
				if (_verbose) {
					Genode::log("Super block changed, dump tree");
					_vbd.dump(_dump_all);
				}
				if (_report) {
					_vbd.report();
				}
			}

			_current_request.success = result ? true : false;

			_pending = false;
			_completed = true;
			return true;
		}

		bool peek_completed_request()
		{
			return _completed;
		}

		Block::Request take_completed_request()
		{
			Block::Request r { };
			if (!_completed) { return r; }

			_completed = false;

			r = _current_request;
			_current_request = Block::Request { };
			return r;
		}
};


class Cbe::Main : Rpc_object<Typed_root<Block::Session>>
{
	private:

		Env                    &_env;
		Attached_rom_dataspace  _config_rom      { _env, "config" };
		Xml_node                _config          { _config_rom.xml() };
		bool             const  _verbose         { _config.attribute_value("verbose",  false) };
		bool             const  _report          { _config.attribute_value("report",   false) };
		bool             const  _dump_all        { _config.attribute_value("dump_all", false) };
		Signal_handler<Main>    _request_handler { _env.ep(), *this, &Main::_handle_requests };
		Block::Request          _current_request { };

		Constructible<Attached_ram_dataspace>  _backing_store   { };
		Constructible<Attached_ram_dataspace>  _block_ds        { };
		Constructible<Block_session_component> _block_session   { };
		Constructible<Cbe::Block_allocator>    _block_allocator { };
		Constructible<Cbe::Tree>               _tree            { };
		Constructible<Cbe::Vbd>                _vbd             { };
		Constructible<Cbe::Mmu>                _mmu             { };

		void _handle_requests()
		{
			if (!_block_session.constructed()) { return; }

			Block_session_component &block_session = *_block_session;

			for (;;) {

				bool progress = false;

				/*
				 * Import new requests.
				 */

				block_session.with_requests([&] (Block::Request request) {

					if (!_mmu->acceptable()) {
						return Block_session_component::Response::RETRY;
					}

					if (!request.operation.valid()) {
						return Block_session_component::Response::REJECTED;
					}

					auto content = [&] (void *addr, Genode::size_t) {
						_mmu->submit_request(request, addr);
					};
					block_session.with_content(request, content);

					progress |= true;
					return Block_session_component::Response::ACCEPTED;
				});

				_mmu->execute();

				/*
				 * Acknowledge finished requests.
				 */

				block_session.try_acknowledge([&] (Block_session_component::Ack &ack) {

					if (_mmu->peek_completed_request()) {
						Block::Request request = _mmu->take_completed_request();
						ack.submit(request);

						progress |= true;
					}
				});

				if (!progress) { break; }
			}

			block_session.wakeup_client_if_needed();
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env) : _env(env)
		{
			_env.parent().announce(_env.ep().manage(*this));

			struct Invalid_config { };
			if (!_config_rom.valid()) { throw Invalid_config();}
			_config_rom.update();

			Xml_node config = _config_rom.xml();
			Number_of_bytes const backing_size   = config.attribute_value("backing_size",   Number_of_bytes(0));
			Number_of_bytes const vbd_size       = config.attribute_value("vbd_size",       Number_of_bytes(0));
			Number_of_bytes const vbd_block_size = config.attribute_value("vbd_block_size", Number_of_bytes(4096));
			uint32_t const outer_degree = config.attribute_value("outer_degree",   0u);
			bool const initialize = config.attribute_value("initialize", false);

			struct Missing_parameters { };
			struct Invalid_parameters { };
			struct Invalid_size       { };
			if (!backing_size || !vbd_size || !vbd_block_size || !outer_degree) { throw Missing_parameters(); }
			if (vbd_block_size < (outer_degree * sizeof (Cbe::Type_i_node)))    { throw Invalid_parameters(); }
			if (backing_size / 2 < vbd_size)                                    { throw Invalid_size(); }

			_backing_store.construct(_env.ram(), _env.rm(), backing_size);
			using Info = Cbe::Tree::Info;
			Info info = Cbe::Tree::calculate(Cbe::Tree::Outer_degree { outer_degree },
			                                 Cbe::Block_size         { (uint32_t)vbd_block_size },
			                                 Cbe::Size               { vbd_size });

			if (_verbose) { log("Info: ", info); }

			Cbe::Physical_block_address const avail = (vbd_size / vbd_block_size) + info.md_size + 1 /* root node */;
			Cbe::Physical_block_address const start_pba = Cbe::FIRST_PBA;

			_block_allocator.construct(Data { .base = _backing_store->local_addr<void*>(),
			                                  .size = _backing_store->size() },
			                           start_pba, avail);

			Cbe::Physical_block_address root_pba = ~0;
			try {
				root_pba = _block_allocator->alloc();
			} catch (...) {
				error(__func__, ": alloc failed");
				throw;
			}
			_tree.construct(*_block_allocator, root_pba, info);
			_vbd.construct(_env, *_tree, _verbose, initialize);

			if (config.has_sub_node("state")) {
				_vbd->write_state(config.sub_node("state")); }

			_mmu.construct(*_vbd, _report, _verbose, _dump_all);

			log("Created virtual block device with size ", info.size);
			if (_report) {
				_vbd->report();
			}
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
			                         _request_handler, _vbd->block_count());
			return _block_session->cap();
		}

		void upgrade(Capability<Session>, Root::Upgrade_args const &) override { }

		void close(Capability<Session>) override
		{
			_block_session.destruct();
			_block_ds.destruct();
		}
};


void Component::construct(Genode::Env &env)
{
	env.exec_static_constructors();
	static Cbe::Main inst(env);
}
