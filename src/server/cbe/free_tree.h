/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_TREE_TREE_H_
#define _CBE_TREE_TREE_H_

/* local includes */
#include <cbe/types.h>


namespace Cbe {

	struct Query_data
	{
	Cbe::Block_data item[1];
	} __attribute__((packed));

	struct Free_tree;
} /* namespace Cbe */


struct Cbe::Free_tree
{
	using Cache          = Module::Cache;
	using Cache_Index    = Module::Cache_Index;
	using Cache_Data     = Module::Cache_Data;
	using Cache_Job_Data = Module::Cache_Job_Data;

	Cache _cache { };

	using Translation      = Module::Translation;
	using Translation_Data = Module::Translation_Data;

	Constructible<Translation> _trans { };

	// Module::Write_back _write_back { };
	bool _do_update   { false };
	bool _do_wb       { false };
	bool _wb_done     { false };
	Cbe::Type_1_node_info _last_trans_info[Translation::MAX_LEVELS] { };

	Constructible<Block_manager> _block_manager { };

	uint32_t _num_blocks { 0 };
	uint32_t _found_blocks { 0 };

	struct Write_back_data
	{
		Cbe::Primitive  prim;
		Cbe::Generation gen;
		Cbe::Virtual_block_address vba;
		Cbe::Height tree_height;
		Cbe::Block_data const *block_data;

		Cbe::Physical_block_address new_pba[Translation::MAX_LEVELS];
		Cbe::Type_1_node_info old_pba[Translation::MAX_LEVELS];
	};

	Write_back_data _wb_data { };

	Cbe::Physical_block_address _free_pba[Translation::MAX_LEVELS] { };
	// XXX account for n + m blocks
	Cbe::Physical_block_address _found_pba[Translation::MAX_LEVELS*2] { };

	Cbe::Physical_block_address _root      { };
	Cbe::Hash                   _root_hash { };
	Cbe::Generation             _root_gen  { };

	struct Query_type_2
	{
		enum class State : uint32_t { INVALID, PENDING, IN_PROGRESS, COMPLETE };

		Cbe::Physical_block_address pba;
		State state;

		Cache_Index index;

		bool pending()     const { return state == State::PENDING; }
		bool in_progress() const { return state == State::IN_PROGRESS; }
		bool complete()    const { return state == State::COMPLETE; }
	};

	Query_type_2 _current_type_2 { 0, Query_type_2::State::INVALID, 0 };

	Query_type_2 _wb_io[Translation::MAX_LEVELS] { };

	Constructible<Cbe::Tree_helper> _tree_helper { };

	Free_tree(Cbe::Physical_block_address const root,
	          Cbe::Generation             const root_gen,
	          Cbe::Hash                   const hash,
	          Cbe::Height const height,
	          Cbe::Degree const degree,
	          Cbe::Number_of_leaves const leafs)
	: _root(root), _root_gen(root_gen)
	{
		Genode::error(hash);
		_tree_helper.construct(degree, height);
		_trans.construct(*_tree_helper, true);

		_block_manager.construct(root, leafs);

		Genode::memcpy(_root_hash.values, hash.values, sizeof (Cbe::Hash));
	}

	Cbe::Hash const &root_hash() const
	{
		return _root_hash;
	}

	Cbe::Physical_block_address root_number() const
	{
		return _root;
	}

	/**********************
	 ** Module interface **
	 **********************/

	bool request_acceptable() const
	{
		return _num_blocks == 0;
	}

	void submit_request(Cbe::Super_block            const *active_snapshots,
	                    Cbe::Generation             const  last_secured,
	                    Cbe::Generation             const  current,
	                    uint32_t                    const  num_blocks,
	                    /* refer to tree_height for number of valid elements */
	                    Cbe::Physical_block_address const  new_pba[Translation::MAX_LEVELS],
	                    Cbe::Type_1_node_info       const  old_pba[Translation::MAX_LEVELS],
	                    Cbe::Height                 const  tree_height,
	                    Cbe::Physical_block_address const  free_pba[Translation::MAX_LEVELS],
	                    uint32_t                    const  free_blocks,
	                    Cbe::Primitive              const &req_prim,
	                    Cbe::Virtual_block_address  const  vba,
	                    Cbe::Block_data             &data)
	{
		(void)active_snapshots;
		(void)last_secured;
		(void)free_blocks;

		if (_num_blocks) {
			return;
		}

		_do_update = false;
		_do_wb     = false;

		_current_type_2 = { 0, Query_type_2::State::INVALID, 0 };

		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			_wb_io[i].state = Query_type_2::State::INVALID;
		}

		_num_blocks = num_blocks;
		_found_blocks = 0;

		/* assert sizeof (_free_pba) == sizeof (free_pba) */
		Genode::memcpy(_free_pba, free_pba, sizeof (_free_pba));

		_wb_data.prim        = req_prim;
		_wb_data.gen         = current;
		_wb_data.vba         = vba;
		_wb_data.tree_height = tree_height;
		_wb_data.block_data  = &data;

		/* assert sizeof (_wb_data.new_pba) == sizeof (new_pba) */
		Genode::memcpy(_wb_data.new_pba, new_pba, sizeof (_wb_data.new_pba));
		/* assert sizeof (_wb_data.old_pba) == sizeof (old_pba) */
		Genode::memcpy(_wb_data.old_pba, old_pba, sizeof (_wb_data.old_pba));

		Cbe::Primitive prim {
			.tag          = Tag::INVALID_TAG,
			.operation    = Cbe::Primitive::Operation::READ,
			.success      = Cbe::Primitive::Success::FALSE,
			.block_number = 0,
			.index        = 0,
		};

		_trans->submit_primitive(_root, _root_gen, _root_hash, prim);
	}

	bool _leaf_useable(Cbe::Type_ii_node const &node) const
	{
		if (node.reserved) {
			Genode::error("TODO");
			throw -1;
		}

		return true;
	}

	bool execute(Translation_Data &trans_data,
	             Cache_Data       &cache_data,
	             Cache_Job_Data   &cache_job_data,
	             Query_data       &query_data,
	             Time             &time)
	{
		/* nothing to do, return early */
		if (!_num_blocks) {
			return false;
		}

		bool progress = false;

		/**************************
		 ** Translation handling **
		 **************************/

		progress |= _trans->execute(trans_data);
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
				                                                time.timestamp());
				Cbe::Block_data const &data = cache_data.item[idx.value];
				_trans->mark_generated_primitive_complete(p, data, trans_data);

				_trans->discard_generated_primitive(p);
			}

			progress |= true;
		}

		while (true) {

			Cbe::Primitive prim = _trans->peek_completed_primitive();
			if (!prim.valid()) { break; }

			_current_type_2 = {
				.pba   = prim.block_number,
				.state = Query_type_2::State::PENDING,
				.index = Cache_Index { 0 },
			};

			if (!_trans->get_type_1_info(prim, _last_trans_info, Translation::MAX_LEVELS)) {
				Genode::error(__func__, ":", __LINE__);
			}


			_trans->drop_completed_primitive(prim);
			progress |= true;
		}

		/********************
		 ** Cache handling **
		 ********************/

		progress |= _cache.execute(cache_data, cache_job_data,
		                           time.timestamp());

		/***************************
		 ** Query free leaf nodes **
		 ***************************/

		if (_current_type_2.complete()) {

			Cbe::Type_ii_node *node =
				reinterpret_cast<Cbe::Type_ii_node*>(&query_data.item[0]);
			for (size_t i = 0; i < Cbe::TYPE_2_PER_BLOCK; i++) {
				Cbe::Physical_block_address const pba = node[i].pba;
				if (!pba) { continue; }

				bool const useable = _leaf_useable(node[i]);

				if (useable) {
					_found_pba[_found_blocks] = pba;
					node[i].alloc_gen = _wb_data.gen;
					_found_blocks++;
				}

				if (_num_blocks == _found_blocks) {
					_do_update = true;

					for (uint32_t i = 0, j = 0; i < _num_blocks && j < _found_blocks; i++) {
						if (!_wb_data.new_pba[i]) {
							_wb_data.new_pba[i] = _found_pba[j];
							j++;
						}
					}

					break;
				}
			}

			_current_type_2.state = Query_type_2::State::INVALID;
			progress |= true;
		}

		if (_do_update) {

			bool data_available = true;

			// the FT translation only cares about the inner nodes
			for (uint32_t i = 1; i <= _tree_helper->height(); i++) {
				Cbe::Physical_block_address const pba = _last_trans_info[i].pba;

				if (!_cache.data_available(pba)) {

					if (_cache.request_acceptable(pba)) {
						_cache.submit_request(pba);
						progress |= true;
					}
					data_available = false;
					break;
				}
			}

			if (data_available) {

				for (uint32_t i = 1; i <= _tree_helper->height(); i++) {

					Cbe::Physical_block_address const pba = _last_trans_info[i].pba;
					Cache_Index     const idx   = _cache.data_index(pba, time.timestamp());
					Cbe::Block_data &data = cache_data.item[idx.value];
					bool const type2_node = (i == 1);

					if (type2_node) {

						using T = Cbe::Type_ii_node;
						T *t = reinterpret_cast<T*>(&data);
						for (Cbe::Degree i = 0; i < _tree_helper->degree(); i++) {
							T &entry = t[i];

							for (uint32_t i = 0; i < _found_blocks; i++) {
								using Node_info = Cbe::Type_1_node_info;
								Node_info &old_entry = _wb_data.old_pba[i];

								Cbe::Physical_block_address const new_pba = _wb_data.new_pba[i];

								if (entry.pba == new_pba) {
									entry.pba       = old_entry.pba;
									entry.alloc_gen = old_entry.gen;
									entry.free_gen  = _wb_data.gen;
									entry.reserved  = true;
								}
							}
						}
					} else {
						uint32_t const pre_level = i - 1;

						Cbe::Physical_block_address const pre_pba = _last_trans_info[pre_level].pba;

						Cache_Index     const idx = _cache.data_index(pre_pba, time.timestamp());
						Cbe::Block_data &pre_data = cache_data.item[idx.value];

						Sha256_4k::Hash hash { };

						Sha256_4k::Data const &pre_hash_data =
							*reinterpret_cast<Sha256_4k::Data const*>(&pre_data);
						Sha256_4k::hash(pre_hash_data, hash);

						using T = Cbe::Type_i_node;
						T *t = reinterpret_cast<T*>(&data);
						for (Cbe::Degree i = 0; i < _tree_helper->degree(); i++) {
							T &entry = t[i];

							if (entry.pba == pre_pba) {
								Genode::memcpy(entry.hash.values, hash.values, sizeof (Cbe::Hash));
							}
						}

						if (i == _tree_helper->height()) {

							Sha256_4k::Data const &hash_data =
								*reinterpret_cast<Sha256_4k::Data const*>(&data);
							Sha256_4k::hash(hash_data, hash);
							Genode::memcpy(_root_hash.values, hash.values, sizeof (Cbe::Hash));
						}
					}

					_wb_io[i].pba = pba;
					_wb_io[i].state = Query_type_2::State::PENDING;
					_wb_io[i].index = idx;
				}

				_do_wb = true;
				_do_update = false;
				progress |= true;
			}
		}

		if (_do_wb) {
			bool wb_ongoing = false;
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				wb_ongoing |= (_wb_io[i].pending() || _wb_io[i].in_progress());
			}

			if (!wb_ongoing && !_wb_done) {
				_wb_done = true;
				progress |= true;
			}
		}

		return progress;
	}

	Cbe::Primitive peek_generated_primitive() /* const */
	{
		/* cache */
		{
			Cbe::Primitive prim = _cache.peek_generated_primitive();
			if (prim.valid()) { return prim; }
		}

		/* current type 2 node */
		if (_current_type_2.pending()) {
			return Cbe::Primitive {
				.tag          = Tag::IO_TAG,
				.operation    = Cbe::Primitive::Operation::READ,
				.success      = Cbe::Primitive::Success::FALSE,
				.block_number = _current_type_2.pba,
				.index        = 0
			};
		}

		/* write-back I/O */
		if (_do_wb) {
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				if (_wb_io[i].pending()) {
					return Cbe::Primitive {
						.tag          = Tag::WRITE_BACK_TAG,
						.operation    = Cbe::Primitive::Operation::WRITE,
						.success      = Cbe::Primitive::Success::FALSE,
						.block_number = _wb_io[i].pba,
						.index        = 0
					};
				}
			}
		}

		return Cbe::Primitive { };
	}

	Index peek_generated_data_index(Cbe::Primitive const &prim) /* const */
	{
		Index idx { .value = ~0u };

		switch (prim.tag) {
		case Tag::CACHE_TAG:
		{
			idx.value = 0;
			break;
		}
		case Tag::IO_TAG:
			idx.value = 0;
			break;
		case Tag::WRITE_BACK_TAG:
		{
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				if (prim.block_number == _wb_io[i].pba) {
					if (_wb_io[i].pending()) {
						idx.value = _wb_io[i].index.value;
					} else {
						Genode::warning(__func__, ": ignore invalid WRITE_BACK_TAG primitive");
					}
				}
			}
			break;
		}
		default: break;
		}

		return idx;
	}

	void drop_generated_primitive(Cbe::Primitive const &prim)
	{
		switch (prim.tag) {
		case Tag::CACHE_TAG:
		{
			_cache.drop_generated_primitive(prim);
			break;
		}
		case Tag::IO_TAG:
			_current_type_2.state = Query_type_2::State::IN_PROGRESS;
			break;
		case Tag::WRITE_BACK_TAG:
		{
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				if (prim.block_number == _wb_io[i].pba) {
					if (_wb_io[i].pending()) {
						_wb_io[i].state = Query_type_2::State::IN_PROGRESS;
					} else {
						Genode::warning(__func__, ": ignore invalid WRITE_BACK_TAG primitive");
					}
				}
			}
			break;
		}
		default:
			Genode::error(__func__, ": invalid primitive");
		break;
		}
	}

	void mark_generated_primitive_complete(Cbe::Primitive const &prim)
	{
		switch (prim.tag) {
		case Tag::CACHE_TAG:
		{
			_cache.mark_completed_primitive(prim);
			break;
		}
		case Tag::IO_TAG:
			if (_current_type_2.in_progress()) {
				_current_type_2.state = Query_type_2::State::COMPLETE;
			} else {
				Genode::error(__func__, ": I/O primitive not in progress");
			}
			break;
		case Tag::WRITE_BACK_TAG:
		{
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				if (prim.block_number == _wb_io[i].pba) {
					if (_wb_io[i].in_progress()) {
						_wb_io[i].state = Query_type_2::State::COMPLETE;

						// XXX invalidate cache
						_cache.invalidate(_wb_io[i].pba);

						if (prim.success == Cbe::Primitive::Success::FALSE) {
							Genode::error(__func__, ": primitive failed");
						}
					} else {
						Genode::warning(__func__, ": ignore invalid WRITE_BACK_TAG primitive ",
						                i, " pba: ", _wb_io[i].pba, " state: ", (uint32_t)_wb_io[i].state);
					}
				}
			}
			break;
		}
		default:
			Genode::error(__func__, ": invalid primitive");
		break;
		}
	}

	Cbe::Primitive peek_completed_primitive()
	{
		if (_num_blocks && _num_blocks == _found_blocks && _wb_done) {
			return _wb_data.prim;
		}
		return Cbe::Primitive { };
	}

	Write_back_data const &peek_completed_wb_data(Cbe::Primitive const &prim) const
	{
		if (!prim.equal(_wb_data.prim)) {
			Genode::error(__func__, ": invalid primitive");
			throw -1;
		}

		MDBG(FT, __func__, ":", __LINE__);
		return _wb_data;
	}

	void drop_completed_primitive(Cbe::Primitive const &prim)
	{
		if (!prim.equal(_wb_data.prim)) {
			Genode::error(__func__, ": invalid primitive");
			throw -1;
		}

		MDBG(FT, __func__, ":", __LINE__);
		_num_blocks = 0;
	}
};

#endif 	/* _CBE_TREE_TREE_H_ */
