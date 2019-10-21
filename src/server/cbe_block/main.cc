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
#include <block_session/connection.h>

/* repo includes */
#include <util/sha256_4k.h>

/* cbe */
#include <cbe/types.h>

using namespace Genode;

namespace Cbe {

	struct Size       { uint64_t value; };
	struct Block_size { uint32_t value; };

	struct Block_allocator;

	class Tree;

	struct Block_session_component;
	class Main;

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
			md_size /= (max_size.value / size.value);
			md_size += 1; /* root node */

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
		Constructible<Expanding_reporter> _reporter { };
		uint64_t                    _dump_leaves   { 0 };

		Cbe::Tree                  &_tree;

		Cbe::Tree                  &_free_tree;

		bool                 const  _verbose;

		bool _report_type_1_leaf(Genode::Xml_generator             &xml,
		                         Cbe::Degree                 const  degree,
		                         Cbe::Number_of_leaves       const  max_leafs,
		                         Cbe::Physical_block_address const  parent,
		                         Cbe::Block_allocator              &blk_alloc,
		                         uint64_t                          &leafs)
		{
			bool finished { false };
			Type_i_node *parent_node {
				reinterpret_cast<Cbe::Type_i_node*>(blk_alloc.data(parent)) };

			for (uint32_t id = 0; id < degree; id++) {

				xml.node("node", [&] () {
					xml.attribute("type", 3u);
					xml.attribute("id",   id);
					xml.attribute("pba",  parent_node[id].pba);
					xml.attribute("gen",  parent_node[id].gen);
					xml.attribute("vba",  leafs);
					xml.attribute("hash", Hash::String(parent_node[id].hash));
				});
				if ((++leafs) >= max_leafs) {
					finished = true;
					break;
				}
			}
			return finished;
		}

		bool _report_type_2_leaf(Genode::Xml_generator             &xml,
		                         Cbe::Degree                 const  degree,
		                         Cbe::Number_of_leaves       const  max_leafs,
		                         Cbe::Physical_block_address const  parent,
		                         Cbe::Block_allocator              &blk_alloc,
		                         uint64_t                          &leafs)
		{
			bool finished { false };
			Type_ii_node *parent_node {
				reinterpret_cast<Cbe::Type_ii_node*>(blk_alloc.data(parent)) };

			for (uint32_t id = 0; id < degree; id++) {

				xml.node("node", [&] () {
					xml.attribute("type", 4u);
					xml.attribute("id",   id);
					xml.attribute("reserved",    parent_node[id].reserved);
					xml.attribute("pba",         parent_node[id].pba);
					xml.attribute("alloc_gen",   parent_node[id].alloc_gen);
					xml.attribute("free_gen",    parent_node[id].free_gen);
					xml.attribute("last_vba",    parent_node[id].last_vba);
					xml.attribute("last_key_id", parent_node[id].last_key_id.value);
				});
				if ((++leafs) >= max_leafs) {
					finished = true;
					break;
				}
			}
			return finished;
		}

		/**
		 * Report information about a given leaf node and its sub-nodes
		 */
		bool _report_leaf(Genode::Xml_generator             &xml,
		                  Cbe::Degree                 const  degree,
		                  Cbe::Number_of_leaves       const  max_leafs,
		                  Cbe::Physical_block_address const  parent,
		                  Cbe::Block_allocator              &blk_alloc,
		                  uint64_t                          &leafs,
		                  bool                               free_tree)
		{
			/* finish early in case we already initialized all required leaves */
			if (leafs >= max_leafs) { return true; }

			return free_tree ? _report_type_2_leaf(xml, degree, max_leafs, parent, blk_alloc, leafs)
			                 : _report_type_1_leaf(xml, degree, max_leafs, parent, blk_alloc, leafs);
		}

		/**
		 * Report information about a given type-i node and its sub-nodes
		 */
		bool _report_i_node(Genode::Xml_generator             &xml,
		                    Cbe::Degree                 const  degree,
		                    Cbe::Number_of_leaves       const  max_leafs,
		                    Cbe::Physical_block_address const  parent,
		                    Cbe::Block_allocator              &blk_alloc,
		                    uint32_t                           height,
		                    uint64_t                          &leafs,
		                    bool                               free_tree)
		{
			height--;
			bool finished { false };
			if (height == 0) {
				for (uint32_t id = 0; id < degree && !finished; id++) {
					finished = _report_leaf(xml, degree, max_leafs,
					                        parent, blk_alloc, leafs, free_tree);
					if (finished) {
						break;
					}
				}
				return finished;
			}

			Cbe::Type_i_node *node {
				reinterpret_cast<Cbe::Type_i_node*>(blk_alloc.data(parent)) };

			bool const do_leafs = (height == 1);
			for (uint32_t id = 0; id < degree && !finished; id++) {

				xml.node("node", [&] () {
					xml.attribute("type", 1u);
					xml.attribute("id",   id);
					xml.attribute("pba",  node[id].pba);
					xml.attribute("gen",  node[id].gen);
					xml.attribute("hash", Hash::String(node[id].hash));

					finished = do_leafs ? _report_leaf(xml, degree, max_leafs, node[id].pba,
					                                  blk_alloc, leafs, free_tree)
					                    : _report_i_node(xml, degree, max_leafs, node[id].pba,
					                                     blk_alloc, height, leafs, free_tree);
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
			Cbe::Superblock const &sb {
				*reinterpret_cast<Cbe::Superblock const*>(blk_alloc.data(sb_id)) };

			/* add super-block tag */
			xml.node("super-block", [&] () {
				xml.attribute("id",          sb_id);
				xml.attribute("is_current",  sb_id == curr_sb_id);
				xml.attribute("generation",  sb.last_secured_generation);
				xml.attribute("snapshot_id", sb.snapshot_id);
				xml.attribute("degree",      sb.degree);
				xml.attribute("free-number", sb.free_number);
				xml.attribute("free-leafs",  sb.free_leaves);
				xml.attribute("free-height", sb.free_height);

				if (sb_id != curr_sb_id) { return; }

				/* skip sub-nodes for invalid super blocks */
				if (!sb.valid()) { return; }

				for (uint32_t j = 0; j < Cbe::NUM_SNAPSHOTS; j++) {

					Snapshot const &snap = sb.snapshots[j];

					if (!snap.valid() || snap.height == 0) { continue; }

					/* report information about sub-nodes */
					uint64_t leafs = 0;
					xml.node("snapshot", [&] () {
						xml.attribute("id",     snap.id);
						xml.attribute("valid",  snap.valid());
						xml.attribute("gen",    snap.gen);
						xml.attribute("pba",    snap.pba);
						xml.attribute("height", snap.height);
						xml.attribute("leafs",  snap.leaves);
						xml.attribute("hash",   Hash::String(snap.hash));
						// xml.attribute("type", 1U);

						_report_i_node(xml, sb.degree, snap.leaves, snap.pba,
						               _tree.block_allocator(), snap.height,
						               leafs, false);
					});
				}

				uint64_t leafs = 0;
				xml.node("free-list", [&] () {
					xml.attribute("pba", sb.free_number);
					xml.attribute("type", 1U);
					xml.attribute("hash", Hash::String(sb.free_hash));
					_report_i_node(xml, sb.free_degree, sb.free_leaves, sb.free_number,
					               _free_tree.block_allocator(),
					               sb.free_height, leafs, true);
				});
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

				Cbe::Superblock const &sb {
					*reinterpret_cast<Cbe::Superblock const*>(ba.data(sb_id)) };
				Cbe::Generation  const gen { sb.last_secured_generation };

				if (sb.valid() && gen >= most_recent_gen) {

					curr_sb_id = sb_id;
					most_recent_gen = gen;
				}
			}
			/* iterate over super blocks and report each of them */
			for (uint64_t sb_id = 0; sb_id < Cbe::NUM_SUPER_BLOCKS; sb_id++) {
				_report_super_block(xml, sb_id, curr_sb_id, ba); }
		}

#if 0
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
			xml.for_each_sub_node("node", [&] (Xml_node const &child_xml) {
				try { _write_i_node_child(node, child_xml, info, height, blk_alloc); }
				catch (...) {
					warning("failed to write node according to config");
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
			Cbe::Superblock sb;
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
			*reinterpret_cast<Cbe::Superblock*>(
				blk_alloc.data(xml.attribute_value(
					"id", (Cbe::Physical_block_address)0))) = sb;
		}
#endif

		bool _dump_data(Cbe::Degree                 const  degree,
		                Cbe::Number_of_leaves       const  max_leafs,
		                Cbe::Physical_block_address const  parent,
		                Cbe::Block_allocator              &block_allocator)
		{
			/* finish early in case we already initialized all required leaves */
			if (_dump_leaves >= max_leafs) { return true; }

			bool finished = false;

			Type_i_node *parent_node = reinterpret_cast<Cbe::Type_i_node*>(block_allocator.data(parent));
			for (uint32_t i = 0; i < degree; i++) {
				Cbe::Physical_block_address const pba = parent_node[i].pba;
				Cbe::Generation             const v   = parent_node[i].gen;
				Cbe::Hash                   const &hash = parent_node[i].hash;

				log("leave[", i, "]: vba: ", _dump_leaves, " pba: ", pba, " ", Hex(v), " <", hash, ">");

				++_dump_leaves;
				if (_dump_leaves >= max_leafs) {
					finished = true;
					break;
				}
			}
			return finished;
		}

		bool _dump(Cbe::Degree                 const  degree,
		           Cbe::Number_of_leaves       const  max_leafs,
		           Cbe::Physical_block_address const  parent,
		           Cbe::Block_allocator              &block_allocator,
		           uint32_t                           height)
		{
			log("parent: ", parent);

			height--;
			bool finished = false;
			if (height == 0) {
				for (uint32_t i = 0; i < degree; i++) {
					finished = _dump_data(degree, max_leafs, parent, block_allocator);
					if (finished) { break; }
				}
				return finished;
			}

			Cbe::Type_i_node *node = reinterpret_cast<Cbe::Type_i_node*>(block_allocator.data(parent));
			for (uint32_t i = 0; i < degree; i++) {
				Cbe::Physical_block_address const pba = node[i].pba;
				Cbe::Generation             const v   = node[i].gen;
				Cbe::Hash                   const &hash = node[i].hash;

				log("child[", i, "]: ", pba, " ", Hex(v), " <", hash, ">");

				finished = height == 1 ? _dump_data(degree, max_leafs, node[i].pba, block_allocator)
				                       : _dump     (degree, max_leafs, node[i].pba, block_allocator, height);
				if (finished) { break; }
			}

			return finished;
		}

		bool _initialize_type_1_data(Cbe::Degree                 const  degree,
		                             Cbe::Number_of_leaves       const  max_leafs,
		                             Cbe::Physical_block_address const  parent,
		                             Cbe::Block_allocator              &block_allocator,
		                             uint64_t                          &leafs)
		{
			if (leafs >= max_leafs) { return true; }

			bool finished = false;

			Type_i_node *parent_node = reinterpret_cast<Cbe::Type_i_node*>(block_allocator.data(parent));
			for (uint32_t i = 0; i < degree; i++) {

				try {
					Cbe::Physical_block_address const pba = block_allocator.alloc();
					Cbe::Generation             const v   = 0;

					parent_node[i].pba = pba;
					parent_node[i].gen = v;

					Sha256_4k::Data *data = reinterpret_cast<Sha256_4k::Data*>(block_allocator.data(pba));
					Genode::memset(data, 0x42, sizeof (*data));
					Sha256_4k::Hash hash { };
					Sha256_4k::hash(*data, hash);

					Genode::memcpy(parent_node[i].hash.values, hash.values, sizeof (hash));

					if (_verbose) {
						log("leave[", i, "]: vba: ", leafs, " pba: ", pba,
							" ", Hex(v), " <", hash, ">");
					}
				} catch (...) {
					error(": could not allocate leave ", i, " for ", parent);
					throw;
				}

				if (++leafs >= max_leafs) {
					finished = true;
					break;
				}
			}
			return finished;
		}

		bool _initialize_type_2_data(Cbe::Degree                 const  degree,
		                             Cbe::Number_of_leaves       const  max_leafs,
		                             Cbe::Physical_block_address const  parent,
		                             Cbe::Block_allocator              &block_allocator,
		                             uint64_t                          &leafs)
		{
			if (leafs >= max_leafs) { return true; }
			bool finished = false;

			Type_ii_node *parent_node = reinterpret_cast<Cbe::Type_ii_node*>(block_allocator.data(parent));
			for (uint32_t i = 0; i < degree; i++) {

				try {
					Cbe::Physical_block_address const pba = block_allocator.alloc();

					parent_node[i].reserved  = false;
					parent_node[i].pba       = pba;
					parent_node[i].last_vba  = 0;
					parent_node[i].alloc_gen = 0;
					parent_node[i].free_gen  = 0;
					parent_node[i].last_key_id.value = 0;

					if (_verbose) {
						log("leaf[", i, "]: vba: ", leafs, " pba: ", pba);
					}
				} catch (...) {
					error(": could not allocate leaf ", i, " for ", parent);
					throw;
				}

				if (++leafs >= max_leafs) {
					finished = true;
					break;
				}
			}
			return finished;
		}

		bool _initialize_data(Cbe::Degree                 const  degree,
		                      Cbe::Number_of_leaves       const  max_leafs,
		                      Cbe::Physical_block_address const  parent,
		                      Cbe::Block_allocator              &block_allocator,
		                      uint64_t                          &leafs,
		                      bool                               free_tree)
		{
			/* finish early in case we already initialized all required leaves */
			if (leafs >= max_leafs) { return true; }

			return free_tree ? _initialize_type_2_data(degree, max_leafs, parent, block_allocator, leafs)
			                 : _initialize_type_1_data(degree, max_leafs, parent, block_allocator, leafs);
		}

		bool _initialize(Cbe::Degree                 const  degree,
		                 Cbe::Number_of_leaves       const  max_leafs,
		                 Cbe::Physical_block_address const  parent,
		                 Cbe::Block_allocator              &block_allocator,
		                 uint32_t                           height,
		                 uint64_t                          &leafs,
		                 bool                               free_tree)
		{
			if (_verbose) { log("parent: ", parent); }

			height--;
			bool finished = false;
			if (height == 0) {
				for (uint32_t i = 0; i < degree; i++) {
					finished = _initialize_data(degree, max_leafs, parent, block_allocator,
					                            leafs, free_tree);
					if (finished) { break; }
				}
				return finished;
			}

			Cbe::Type_i_node *node = reinterpret_cast<Cbe::Type_i_node*>(block_allocator.data(parent));
			for (uint32_t i = 0; i < degree; i++) {
				try {
					Cbe::Physical_block_address const pba = block_allocator.alloc();
					Cbe::Generation             const v   = 0;

					node[i].pba = pba;
					node[i].gen = v;

					if (_verbose) { log("child[", i, "]: ", pba, " ", Hex(v)); }
				} catch (...) {
					error(": could not allocate child ", i, " for ", parent);
					throw;
				}

				bool const do_leafs = height == 1;
				finished = do_leafs ? _initialize_data(degree, max_leafs, node[i].pba,
				                                       block_allocator, leafs, free_tree)
				                    : _initialize     (degree, max_leafs, node[i].pba,
				                                       block_allocator, height, leafs, free_tree);

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
		    Cbe::Tree   &free_tree,
		    bool         verbose,
		    bool         initialize)
		:
			_env     { env },
			_tree    { tree },
			_free_tree { free_tree },
			_verbose { verbose }
		{
			try {
				_reporter.construct(_env, "state", "state", 64u<<20);
			} catch (...) {
				warning("no state reporting, cannot construct reporter");
			}

			if (initialize) {
				if (_verbose) {
					Genode::log("Initialize VBD tree");
				}

				uint64_t leafs = 0;
				_initialize(_tree.info().outer_degree, _tree.info().leaves,
				            _tree.root(), _tree.block_allocator(),
				            _tree.info().height, leafs, false);

				if (_verbose) {
					Genode::log("Initialize free list tree");
				}
				uint64_t free_leafs = 0;
				_initialize(_free_tree.info().outer_degree, _free_tree.info().leaves,
				            _free_tree.root(), _free_tree.block_allocator(),
				            _free_tree.info().height, free_leafs, true);
			}
		}

#if 0
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
#endif

		/**
		 * Report state of the super blocks and trees of the block device
		 */
		void report()
		{
			if (!_reporter.constructed()) { return; }

			try {
				_reporter->generate([&] (Genode::Xml_generator &xml) {
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
				Cbe::Superblock &sb = *reinterpret_cast<Cbe::Superblock*>(ba.data(i));
				Cbe::Generation const gen  = sb.last_secured_generation;
				if (sb.valid() && gen >= most_recent_gen) {
					most_recent_gen = gen;
					idx = i;
				}
			}

			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {


				if (!all && i != idx) { continue; }

				Cbe::Superblock &sb = *reinterpret_cast<Cbe::Superblock*>(ba.data(i));

				if (!sb.valid()) {
					Genode::log("          SB[", i, "] invalid");
					continue;
				}

				Cbe::Generation const gen     = sb.last_secured_generation;
				uint32_t        const snap_id = sb.snapshot_id;

				Genode::log(i == idx ? "\033[33;1mCurrent " : "          ",
				            "SB[", i, "]: gen: ", gen, " snap_id: ", snap_id);

				for (uint32_t j = 0; j < Cbe::NUM_SNAPSHOTS; j++) {
					if (!sb.snapshots[j].valid()) { continue; }

					Snapshot const &snap = sb.snapshots[j];

					_dump_leaves = 0;
					Genode::log("Snapshot[", j, "] tree:");
					_dump(sb.degree, snap.leaves, snap.pba, _tree.block_allocator(), snap.height);
				}

				Genode::log("Free list tree:");
				_dump(sb.free_degree, sb.free_leaves, sb.free_number,
				      _free_tree.block_allocator(), sb.free_height);
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


class Write_state_to_block
{
	private:

		struct Job;
		typedef Block::Connection<Job> Block_connection;

		struct Job : Block_connection::Job
		{
			unsigned const id;

			Job(Block_connection &connection, Block::Operation operation, unsigned id)
			:
				Block_connection::Job(connection, operation), id(id)
			{ }
		};

		Env                                  &_env;
		addr_t                                _base;
		size_t                                _size;
		Signal_transmitter                    _done;
		Heap                                  _heap          { _env.ram(), _env.rm() };
		Allocator_avl                         _block_alloc   { &_heap };
		Block_connection                      _block         { _env, &_block_alloc, Number_of_bytes(4 * 1024 * 1024) };
		Block::Session::Info                  _info          { _block.info() };
		Signal_handler<Write_state_to_block>  _block_io_sigh { _env.ep(), *this, &Write_state_to_block::_handle_block_io };
		unsigned                              _job_cnt       { 0 };

		void _handle_block_io()
		{
			_block.update_jobs(*this);
		}

	public:

		/**
		 * Block::Connection::Update_jobs_policy
		 */
		void produce_write_content(Job &, off_t offset, char *dst, size_t length)
		{
			struct Bad_write_offset : Exception { };
			if ((size_t)offset >= _size) {
				throw Bad_write_offset();
			}
			size_t copy_length = length;
			if (offset + length > _size) {

				copy_length = _size - offset;
				void *zero_dst = (void*)((addr_t)dst + offset + copy_length);
				memset(zero_dst, 0, length - copy_length);
			}
			addr_t const src =
				_base + offset;

			memcpy(dst, (void *)src, copy_length);
		}

		/**
		 * Block::Connection::Update_jobs_policy
		 */
		void consume_read_result(Job &, off_t, char const *, size_t) { }

		/**
		 * Block_connection::Update_jobs_policy
		 */
		void completed(Job &, bool success)
		{
			struct Writing_state_to_block_failed : Exception { };
			if (!success) {
				throw Writing_state_to_block_failed();
			}
			log("Initial VBD state written to block connection");
			_done.submit();
		}

		/**
		 * Constructor
		 */
		Write_state_to_block(Env                       &env,
		                     addr_t                     base,
		                     size_t                     size,
		                     Signal_context_capability  done)
		:
			_env    { env },
			_base   { base },
			_size   { size },
			_done   { done }
		{
			log("Writing initial VBD state to block connection...");
			size_t count = _size / _info.block_size;
			if (_size - count * _info.block_size) {
				count++;
			}
			Block::Operation const operation {
				Block::Operation::Type::WRITE, 0, count};

			new (_heap) Job(_block, operation, _job_cnt++);
			_block.sigh(_block_io_sigh);
			_handle_block_io();
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
		bool             const  _write_init_state_to_block { _config.attribute_value("write_init_state_to_block", false) };
		Signal_handler<Main>    _request_handler { _env.ep(), *this, &Main::_handle_requests };
		Block::Request          _current_request { };

		Constructible<Attached_ram_dataspace>  _backing_store        { };
		Constructible<Attached_ram_dataspace>  _block_ds             { };
		Constructible<Block_session_component> _block_session        { };
		Constructible<Block_allocator>         _block_allocator      { };
		Constructible<Tree>                    _tree                 { };
		Constructible<Tree>                    _free_tree            { };
		Constructible<Vbd>                     _vbd                  { };
		Constructible<Mmu>                     _mmu                  { };
		Constructible<Write_state_to_block>    _write_state_to_block { };
		Signal_handler<Main>                   _write_state_done     {
			_env.ep(), *this, &Main::_handle_write_state_done};

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

				progress |= _mmu->execute();

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

		void _handle_write_state_done()
		{
			_write_state_to_block.destruct();
			_env.parent().announce(_env.ep().manage(*this));
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env) : _env(env)
		{
			struct Invalid_config { };
			if (!_config_rom.valid()) { throw Invalid_config();}
			_config_rom.update();

			Xml_node config = _config_rom.xml();
			Number_of_bytes const backing_size   = config.attribute_value("backing_size",   Number_of_bytes(0));

			Number_of_bytes const vbd_size         = config.attribute_value("vbd_size",       Number_of_bytes(0));
			Number_of_bytes const vbd_block_size   = config.attribute_value("vbd_block_size", Number_of_bytes(4096));
			uint32_t        const vbd_outer_degree = config.attribute_value("vbd_outer_degree", 0u);

			Number_of_bytes const ft_size         = config.attribute_value("ft_size",       Number_of_bytes(0));
			Number_of_bytes const ft_block_size   = config.attribute_value("ft_block_size", vbd_block_size);
			uint32_t        const ft_outer_degree = config.attribute_value("ft_outer_degree", 0u);

			bool const initialize = config.attribute_value("initialize", false);

			struct Missing_parameters { };
			struct Invalid_parameters { };
			struct Invalid_size       { };

			if (!backing_size
			    || !vbd_size || !vbd_block_size || !vbd_outer_degree
			    || !ft_size  || !ft_block_size  || !ft_outer_degree) {
				throw Missing_parameters();
			}

			if (  vbd_block_size < (vbd_outer_degree * sizeof (Cbe::Type_i_node))
			    || ft_block_size < (ft_outer_degree  * sizeof (Cbe::Type_i_node))) {
				throw Invalid_parameters();
			}

			_backing_store.construct(_env.ram(), _env.rm(), backing_size);

			using Info = Cbe::Tree::Info;
			Info info = Cbe::Tree::calculate(Cbe::Tree::Outer_degree { vbd_outer_degree },
			                                 Cbe::Block_size         { (uint32_t)vbd_block_size },
			                                 Cbe::Size               { vbd_size });

			log("Tree info: ", info);

			Info free_info = Cbe::Tree::calculate(Cbe::Tree::Outer_degree { ft_outer_degree },
			                                      Cbe::Block_size         { (uint32_t)ft_block_size },
			                                      Cbe::Size               { ft_size });

			log("Free tree info: ", free_info);

			if (free_info.md_size > (free_info.leaves / 2)) {
				Genode::error("no enough free leafs in free-list");
				throw Invalid_parameters();
			}

			/*
			 * Make sure there is enough room for all nodes (in case of the FT
			 * twice the size to rewrite the tree completely).
			 */
			Genode::uint64_t const needed_blocks = (vbd_size / vbd_block_size + info.md_size + 1)
			                                     + (ft_size  / ft_block_size  + (free_info.md_size * 2 ) + 1);
			Genode::uint64_t const avail_blocks  = (backing_size / vbd_block_size) - Cbe::FIRST_PBA;

			if (avail_blocks < needed_blocks) {
				Genode::error("avail ", avail_blocks, " blocks but need ", needed_blocks, " blocks");
			   	throw Invalid_size();
			}

			_block_allocator.construct(Data { .base = _backing_store->local_addr<void*>(),
			                                  .size = _backing_store->size() },
			                           Cbe::FIRST_PBA, avail_blocks);

			Cbe::Physical_block_address root_pba = ~0;
			try {
				root_pba = _block_allocator->alloc();
			} catch (...) {
				error(__func__, ": alloc failed");
				throw;
			}
			_tree.construct(*_block_allocator, root_pba, info);

			Cbe::Physical_block_address tree_root_pba = ~0;
			try {
				tree_root_pba = _block_allocator->alloc();
			} catch (...) {
				error(__func__, ": alloc failed");
				throw;
			}
			_free_tree.construct(*_block_allocator, tree_root_pba, free_info);

			_vbd.construct(_env, *_tree, *_free_tree, _verbose, initialize);

			if (config.has_sub_node("state")) {
				Genode::log("Use <state> to initalize VBD");
#if 0
				_vbd->write_state(config.sub_node("state"));
#endif
			} else {
				Genode::log("Use parameters to initalize VBD");

				/* initialise first super block slot */
				Cbe::Superblock &sb = *reinterpret_cast<Cbe::Superblock*>(_block_allocator->data(0));
				sb.degree                  = info.outer_degree;
				sb.last_secured_generation = 0;
				sb.snapshot_id             = 0;

				Snapshot &snap = sb.snapshots[sb.snapshot_id];
				snap.pba       = root_pba;
				snap.height    = info.height;
				snap.leaves    = info.leaves;
				snap.gen       = sb.last_secured_generation;
				snap.id        = sb.snapshot_id;
				snap.flags     = Snapshot::FLAGS_CLEAR;
				snap.valid(true);

				{
					Sha256_4k::Data const &data =
						*reinterpret_cast<Sha256_4k::Data const*>(_block_allocator->data(snap.pba));
					Sha256_4k::Hash hash { };
					Sha256_4k::hash(data, hash);
					Genode::memcpy(snap.hash.values, hash.values, sizeof (hash));
				}

				for (uint32_t i = 1; i < Cbe::NUM_SNAPSHOTS; i++) {
					sb.snapshots[i].valid(false);
				}

				sb.free_number = tree_root_pba;
				sb.free_leaves = free_info.leaves;
				sb.free_height = free_info.height;
				sb.free_degree = free_info.outer_degree;

				{
					Sha256_4k::Data *data =
						reinterpret_cast<Sha256_4k::Data*>(_block_allocator->data(tree_root_pba));
					Sha256_4k::Hash hash { };
					Sha256_4k::hash(*data, hash);
					Genode::memcpy(sb.free_hash.values, hash.values, sizeof (hash));
				}

				/* clear other super block slots */
				for (uint64_t i = 1; i < Cbe::NUM_SUPER_BLOCKS; i++) {

					Cbe::Superblock &sbX = *reinterpret_cast<Cbe::Superblock*>(_block_allocator->data(i));
					Genode::memset(&sbX, 0, sizeof(sbX));

					sbX.last_secured_generation = Cbe::INVALID_GEN;
					sbX.degree                  = sb.degree;

					for (uint32_t i = 0; i < Cbe::NUM_SNAPSHOTS; i++) {
						sbX.snapshots[i].valid(false);
					}

					sbX.free_number = sb.free_number;
					sbX.free_height = sb.free_height;
					sbX.free_degree = sb.free_degree;
					sbX.free_leaves = sb.free_leaves;
				}
			}
			_mmu.construct(*_vbd, _report, _verbose, _dump_all);

			log("Created virtual block device with size ", info.size);
			if (_report) {
				_vbd->report();
			}

			if (_write_init_state_to_block) {
				_write_state_to_block.construct(
					_env, (addr_t)_backing_store->local_addr<void*>(),
					_backing_store->size(), _write_state_done);
			} else {
				_handle_write_state_done();
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
