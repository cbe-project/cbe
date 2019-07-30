/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_FREE_TREE_H_
#define _CBE_FREE_TREE_H_

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

	using Translation      = Module::Translation;
	using Translation_Data = Module::Translation_Data;

	Constructible<Translation> _trans { };

	// Module::Write_back _write_back { };
	bool _do_update   { false };
	bool _do_wb       { false };
	bool _wb_done     { false };
	// Cbe::Type_1_node_info _last_trans_info[Translation::MAX_LEVELS] { };

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

		bool finished;

		bool valid() const { return finished; }
	};

	Write_back_data _wb_data { };

	Cbe::Physical_block_address _free_pba[Translation::MAX_LEVELS] { };
	// XXX account for n + m blocks
	// XXX number of pba depends on degree
	enum { MAX_FREE_BLOCKS = 64, };
	struct Query_branch
	{
		Cbe::Type_1_node_info trans_info[Translation::MAX_LEVELS];

		Cbe::Physical_block_address pba[MAX_FREE_BLOCKS];
		uint32_t free_blocks;
		Cbe::Virtual_block_address  vba;
	};
	enum { MAX_QUERY_BRANCHES = 8, };
	Query_branch _query_branch[MAX_QUERY_BRANCHES] { };
	uint32_t     _current_query_branch { 0 };

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

	Cbe::Primitive _current_query_prim { };

	// XXX maybe organize it in a better way?
	enum { MAX_WB_IO = Translation::MAX_LEVELS*MAX_QUERY_BRANCHES, };
	Query_type_2 _wb_io[MAX_WB_IO] { };

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
		_tree_helper.construct(degree, height, leafs);
		_trans.construct(*_tree_helper, true);

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
		_wb_done   = false;

		_current_type_2 = { 0, Query_type_2::State::INVALID, 0 };

		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			_wb_io[i].state = Query_type_2::State::INVALID;
		}

		_num_blocks = num_blocks;
		_found_blocks = 0;

		/* assert sizeof (_free_pba) == sizeof (free_pba) */
		Genode::memcpy(_free_pba, free_pba, sizeof (_free_pba));
		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			if (free_pba[i]) {
				MDBG(FT, __func__, ":", __LINE__, " free[", i, "]: ", free_pba[i]);
			}
		}


		_wb_data.finished    = false;
		_wb_data.prim        = req_prim;
		_wb_data.gen         = current;
		_wb_data.vba         = vba;
		_wb_data.tree_height = tree_height;
		_wb_data.block_data  = &data;

		/* assert sizeof (_wb_data.new_pba) == sizeof (new_pba) */
		Genode::memcpy(_wb_data.new_pba, new_pba, sizeof (_wb_data.new_pba));
		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			if (new_pba[i]) {
				MDBG(FT, __func__, ":", __LINE__, " new[", i, "]: ", new_pba[i]);
			}
		}

		/* assert sizeof (_wb_data.old_pba) == sizeof (old_pba) */
		Genode::memcpy(_wb_data.old_pba, old_pba, sizeof (_wb_data.old_pba));

		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			if (old_pba[i].pba) {
				MDBG(FT, __func__, ":", __LINE__, " old[", i, "]: ", old_pba[i].pba);
			}
		}

		_current_query_prim = Cbe::Primitive {
			.tag          = Tag::INVALID_TAG,
			.operation    = Cbe::Primitive::Operation::READ,
			.success      = Cbe::Primitive::Success::FALSE,
			.block_number = 0,
			.index        = 0,
		};

		/* reset query branches */
		// XXX instead of resetting eager maybe do it lazy after submitting
		//     request to translation module
		_current_query_branch = 0;
		for (uint32_t b = 0; b < MAX_QUERY_BRANCHES; b++) {
			_query_branch[b].vba         = Cbe::INVALID_VBA;
			_query_branch[b].free_blocks = 0;
			for (uint32_t n = 0; n < MAX_FREE_BLOCKS; n++) {
				_query_branch[b].pba[n] = Cbe::INVALID_PBA;
			}
		}
	}

	bool _leaf_useable(Cbe::Type_ii_node const &node) const
	{
		if (node.reserved) {
			Genode::error(__func__, ": reserved leaf: TODO");
			return false;
		}

		return true;
	}

	bool execute(Translation_Data &trans_data,
	             Cache            &cache,
	             Cache_Data       &cache_data,
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

		while (true) {
			if (!_trans->acceptable()) { break; }
			if (!_current_query_prim.valid()) { break; }

			_trans->submit_primitive(_root, _root_gen, _root_hash, _current_query_prim);

			_current_query_prim.operation = Cbe::Primitive::Operation::INVALID;
			progress |= true;
		}

		progress |= _trans->execute(trans_data);
		while (true) {
			Cbe::Primitive p = _trans->peek_generated_primitive();
			if (!p.valid()) { break; }

			Cbe::Physical_block_address const pba = p.block_number;
			if (!cache.data_available(pba)) {

				if (cache.request_acceptable(pba)) {
					cache.submit_request(pba);
				}
				break;
			} else {

				Cache_Index     const idx   = cache.data_index(pba,
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

			if (!_trans->get_type_1_info(prim,
			                             _query_branch[_current_query_branch].trans_info)) {
				Genode::error(__func__, ":", __LINE__);
			}

			_trans->drop_completed_primitive(prim);
			progress |= true;
		}

		/***************************
		 ** Query free leaf nodes **
		 ***************************/

		if (_current_type_2.complete()) {

			_current_query_prim.operation = Cbe::Primitive::Operation::INVALID;

			Cbe::Type_ii_node *node =
				reinterpret_cast<Cbe::Type_ii_node*>(&query_data.item[0]);
			for (size_t i = 0; i < Cbe::TYPE_2_PER_BLOCK; i++) {
				Cbe::Physical_block_address const pba = node[i].pba;
				if (!pba) { continue; }

				bool const useable = _leaf_useable(node[i]);

				if (useable) {
					/* store current VBA */
					if (_query_branch[_current_query_branch].vba == Cbe::INVALID_VBA) {
						_query_branch[_current_query_branch].vba = _current_query_prim.block_number;
					}

					uint32_t &free_blocks = _query_branch[_current_query_branch].free_blocks;
					_query_branch[_current_query_branch].pba[free_blocks] = pba;
					_query_branch[_current_query_branch].free_blocks++;

					_found_blocks++;
				}

				/* break off early */
				if (_num_blocks == _found_blocks) {
					break;
				}
			}
			_current_query_branch++;

			_current_type_2.state = Query_type_2::State::INVALID;

			bool const end_of_tree = (_current_query_prim.block_number + _tree_helper->degree() >= _tree_helper->leafs());
			if (_found_blocks < _num_blocks) {
				if (end_of_tree) {
					Genode::warning("could not find enough usable leafs: ", _num_blocks - _found_blocks, " missing");
					_wb_data.finished = true;
					_wb_data.prim.success = Cbe::Primitive::Success::FALSE;
				} else {

					_current_query_prim.block_number += _tree_helper->degree();
					_current_query_prim.operation = Cbe::Primitive::Operation::READ;
				}
			} else if (_num_blocks == _found_blocks) {

				uint32_t i = 0;
				for (uint32_t b = 0; b < _current_query_branch; b++) {
					for (uint32_t n = 0; n < _query_branch[b].free_blocks; n++) {

						/* store iterator out-side so we start from the last set entry */
						for (; i < Translation::MAX_LEVELS; i++) {
							if (!_wb_data.new_pba[i]) {
								_wb_data.new_pba[i] = _query_branch[b].pba[n];
								break;
							}
						}
					}
				}

				_do_update = true;
			}

			progress |= true;
		}


		/********************************
		 ** Update meta-data in branch **
		 ********************************/

		if (_do_update) {

			bool data_available = true;

			for (uint32_t b = 0; b < _current_query_branch; b++) {
				Query_branch &qb = _query_branch[b];

				// the FT translation only cares about the inner nodes
				for (uint32_t i = 1; i <= _tree_helper->height(); i++) {
					Cbe::Physical_block_address const pba = qb.trans_info[i].pba;

					if (!cache.data_available(pba)) {

						if (cache.request_acceptable(pba)) {
							cache.submit_request(pba);
							progress |= true;
						}
						data_available = false;
						break;
					}
				}
			}

			if (data_available) {

				//////////////////////////////////////
				// XXX CHANGE HOW THE WB LIST IS POPULATED:
				//     1. add type2 for each branch
				//     2. add type1 for each branch
				//    (3. add root  for each branch)
				//////////////////////////////////////

				uint32_t wb_cnt = 0;
				for (uint32_t b = 0; b < _current_query_branch; b++) {

					Query_branch &qb = _query_branch[b];
					for (uint32_t i = 1; i <= _tree_helper->height(); i++) {

						Cbe::Physical_block_address const pba = qb.trans_info[i].pba;
						Cache_Index     const idx   = cache.data_index(pba, time.timestamp());
						Cbe::Block_data &data = cache_data.item[idx.value];
						bool const type2_node = (i == 1);

						if (type2_node) {

							using T = Cbe::Type_ii_node;
							T *t = reinterpret_cast<T*>(&data);
							for (Cbe::Degree i = 0; i < _tree_helper->degree(); i++) {
								T &entry = t[i];

								/*
								 * The old and new PBA array contains data and inner node,
								 * therefor we have to check tree height + 1.
								 */
								for (uint32_t i = 0; i <= _tree_helper->height(); i++) {
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

							Cbe::Physical_block_address const pre_pba = qb.trans_info[pre_level].pba;

							Cache_Index     const idx = cache.data_index(pre_pba, time.timestamp());
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

						/* only add blocks once */
						bool already_pending = false;
						for (uint32_t i = 0; i < wb_cnt; i++) {
							if (_wb_io[i].pba == pba) {
								already_pending = true;
								break;
							}
						}

						if (already_pending) { continue; }

						_wb_io[wb_cnt].pba = pba;
						_wb_io[wb_cnt].state = Query_type_2::State::PENDING;
						_wb_io[wb_cnt].index = idx;

						wb_cnt++;
					}
				}

				_do_wb = true;
				_do_update = false;
				progress |= true;
			}
		}

		/**********************************
		 ** Write-back of changed branch **
		 **********************************/

		if (_do_wb && !_wb_done) {
			bool wb_ongoing = false;
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				wb_ongoing |= (_wb_io[i].pending() || _wb_io[i].in_progress());

				if (wb_ongoing) { break; }
			}

			if (!wb_ongoing && !_wb_done) {
				_do_wb = false;
				_wb_done = true;
				_wb_data.finished = true;
				_wb_data.prim.success = Cbe::Primitive::Success::TRUE;
				progress |= true;
			}
		}

		return progress;
	}

	Cbe::Primitive peek_generated_primitive() /* const */
	{
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
		Index idx { .value = Cbe::INVALID_INDEX };

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
				if (prim.block_number != _wb_io[i].pba) { continue; }

				if (!_wb_io[i].pending()) {
					Genode::warning(__func__, ": ignore invalid WRITE_BACK_TAG primitive");
				}

				idx.value = _wb_io[i].index.value;
				break;
			}
			break;
		}
		default: break;
		}

		if (idx.value == Cbe::INVALID_INDEX) {
			throw -1;
		}

		return idx;
	}

	void drop_generated_primitive(Cbe::Primitive const &prim)
	{
		switch (prim.tag) {
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
		if (_wb_data.valid()) {
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
		_wb_data.finished = false;
		_num_blocks = 0;
	}
};

#endif 	/* _CBE_FREE_TREE_H_ */
