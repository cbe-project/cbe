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

#define MOD_NAME "FT"

/*
 * The Free_tree meta-module handles the allocation and freeing, i.e.,
 * reservation, of nodes. It is vital to implement the CoW semantics of
 * the VBD.
 *
 * To gather all required free blocks the FT will scan the free-tree
 * branch by branch. The current implementation employs the Translation
 * module and will use the virtual-block-address to select the next
 * type 2 node, i.e., it will increament the VBA by the degree of the
 * type 2 node to get to the next type 2 node.
 *
 * (It currently _does not_ feature CoW semanitcs as it will only
 *  exchange entries in the leaf nodes. The hashes of the inner nodes
 *  are still updated, though.)
 */
struct Cbe::Free_tree
{
	using Cache          = Module::Cache;
	using Cache_Index    = Module::Cache_Index;
	using Cache_Data     = Module::Cache_Data;
	using Cache_Job_Data = Module::Cache_Job_Data;

	/*
	 * The Translation module is used to traverse the FT by looking
	 * for (re-)usable leaf nodes in each branch of the tree.
	 */
	using Translation      = Module::Translation;
	using Translation_Data = Module::Translation_Data;

	/*
	 * (Using Constructible here is not strictly necessary as we
	 *  might decide to reinstanciate the FT module as a whole
	 *  later on and just change the helpers when the free-tree
	 *  is resized might not be worth it.)
	 */
	Constructible<Cbe::Tree_helper> _tree_helper { };
	Constructible<Translation>      _trans       { };

	/*
	 * Internal states of the module
	 *
	 * (Should be replaced by a proper State type.)
	 */
	/*
	 * Do update is set to 'true' when we have found the required
	 * number of free blocks. It will be reset to 'false' either in
	 * case all data needed to populate the write-back list is available
	 * or the update step is restarted (by retrying the allocation).
	 */
	bool _do_update   { false };
	/*
	 * Do write-back is to 'true' after the update step has been
	 * finished successfully. It will be reset to 'false' when the
	 * write-back step has been finished...
	 */
	bool _do_wb       { false };
	/*
	 * ... and at the some time write-back done will be set to 'true'.
	 *
	 * (It is actually somewhat superfluous and should be removed later
	 *  on.)
	 */
	bool _wb_done     { false };

	/*
	 * Holds the number of free blocks that are needed.
	 *
	 * It will be set in 'submit_request' and cleared, i.e., set to 0
	 * in 'drop_completed_primitive'.
	 */
	uint32_t _num_blocks   { 0 };

	/*
	 * Holds the currently number of found free blocks.
	 */
	uint32_t _found_blocks { 0 };

	// XXX account for n + m blocks, currently we only replace
	//     the entries in the leaf node but do not update the
	//     FT itself in a CoW fashion
	// XXX the number of pba depends on degree which for now
	//     is dynamically set. Since 64 edges are the eventually
	//     used ones, hardcode that.
	enum { MAX_FREE_BLOCKS = 64, };
	struct Query_branch
	{
		/* virtual-block-adress of the FT branch */
		Cbe::Virtual_block_address vba;

		/* info, e.g. PBAs, of the branches type 1 inner nodes */
		Cbe::Type_1_node_info trans_info[Translation::MAX_LEVELS];

		/* physical block addresses of the free blocks in the FT branch */
		Cbe::Physical_block_address pba[MAX_FREE_BLOCKS];

		/* number of free blocks in this FT branch */
		uint32_t free_blocks;
	};

	/*
	 * The number of branches of we look at to gather free nodes is limited.
	 *
	 * (For now the limit should suffices but it could be not enough
	 *  in a fragmented FT.)
	 */
	enum { MAX_QUERY_BRANCHES = 8, };
	Query_branch _query_branch[MAX_QUERY_BRANCHES] { };
	uint32_t     _current_query_branch { 0 };

	Cbe::Physical_block_address _found_pba[Translation::MAX_LEVELS*2] { };
	Cbe::Physical_block_address _free_pba[Translation::MAX_LEVELS] { };

	/*
	 * Meta-data of the current FT
	 *
	 * (As we do not change the inner nodes but only the leaf nodes,
	 *  only the hash needs to be updated.)
	 */
	Cbe::Physical_block_address _root      { };
	Cbe::Hash                   _root_hash { };
	Cbe::Generation             _root_gen  { };

	/*
	 * This primitive holds the current translation request and
	 * thereby the current branch in the FT we look at.
	 */
	Cbe::Primitive _current_query_prim { };

	/*
	 * The Io_entry provides a stateful I/O operation abstraction
	 */
	struct Io_entry
	{
		enum class State : uint32_t { INVALID, PENDING, IN_PROGRESS, COMPLETE };

		Cbe::Physical_block_address pba;
		State state;

		Cache_Index index;

		bool pending()     const { return state == State::PENDING; }
		bool in_progress() const { return state == State::IN_PROGRESS; }
		bool complete()    const { return state == State::COMPLETE; }
	};

	/*
	 * There is always only one type 2 node query pending, branch
	 * by branch.
	 *
	 * (Eventually it might makes sense to query all disjunct branches
	 *  concurrently.)
	 */
	Io_entry _current_type_2 { 0, Io_entry::State::INVALID, 0 };

	/*
	 * As we might need to write multiple branches back to the disk
	 * make sure we have enough entries available.
	 *
	 * (Maybe it makes sense to reorganize all these array in array
	 *  structures in a better way.)
	 */
	enum { MAX_WB_IO = Translation::MAX_LEVELS*MAX_QUERY_BRANCHES, };
	Io_entry _wb_io[MAX_WB_IO] { };

	/*
	 * The Write_back_data data is used after the FT has
	 * finished to pass on all needed information to the
	 * Write_back module.
	 */
	struct Write_back_data
	{
		Cbe::Primitive             prim;
		Cbe::Generation            gen;
		Cbe::Virtual_block_address vba;
		Cbe::Height                tree_height;

		Cbe::Physical_block_address new_pba[Translation::MAX_LEVELS];
		Cbe::Type_1_node_info       old_pba[Translation::MAX_LEVELS];

		bool finished;

		bool valid() const { return finished; }
	};

	Write_back_data _wb_data { };

	/*
	 * Helper method that will reset the internal state
	 *
	 * (It currently resets all already found free blocks,
	 *  it would be better to merge them with any newly
	 *  found ones.)
	 */
	void _reset_query_prim()
	{
		_found_blocks = 0;

		_current_query_prim = Cbe::Primitive {
			.tag          = Tag::INVALID_TAG,
			.operation    = Cbe::Primitive::Operation::READ,
			.success      = Cbe::Primitive::Success::FALSE,
			.block_number = 0,
			.index        = 0,
		};

		/* reset query branches */
		_current_query_branch = 0;
		for (uint32_t b = 0; b < MAX_QUERY_BRANCHES; b++) {
			_query_branch[b].vba         = Cbe::INVALID_VBA;
			_query_branch[b].free_blocks = 0;
			for (uint32_t n = 0; n < MAX_FREE_BLOCKS; n++) {
				_query_branch[b].pba[n] = Cbe::INVALID_PBA;
			}
		}
		MOD_DBG(_current_query_prim);
	};

	/*
	 * Constructor
	 *
	 * \param  root      physical-block-address of the root node
	 * \param  root_gen  generation of the root node
	 * \param  hash      hash of the root node
	 * \param  height    height of the free-tree
	 * \param  dregree   number of edges of a node
	 * \param  leafs     total number of leafs in the tree
	 */
	Free_tree(Cbe::Physical_block_address const root,
	          Cbe::Generation             const root_gen,
	          Cbe::Hash                   const hash,
	          Cbe::Height                 const height,
	          Cbe::Degree                 const degree,
	          Cbe::Number_of_leaves       const leafs)
	: _root(root), _root_gen(root_gen)
	{
		_tree_helper.construct(degree, height, leafs);
		_trans.construct(*_tree_helper, true);

		Genode::memcpy(_root_hash.values, hash.values, sizeof (Cbe::Hash));
	}

	/**********************
	 ** Module interface **
	 **********************/

	/**
	 * Instruct the free-tree module to retry the current pending request
	 *
	 * This method relies on the requests information being available, i.e.,
	 * _do not_ call 'drop_completed_primitive' if one intends to retry the
	 * allocation after discarding a snapshot (in case the former allocation
	 * did not succeed the module will emit a failed completed primitive.
	 */
	void retry_allocation()
	{
		MOD_DBG("");
		_reset_query_prim();

		_do_update = false;
		_do_wb     = false;
		_wb_done   = false;

		_wb_data.finished = false;
	}

	/**
	 * Check if the module is able to accept a new request
	 *
	 * \return true if a request can be accepted, otherwise false
	 */
	bool request_acceptable() const
	{
		return _num_blocks == 0;
	}

	/**
	 * Submit new free block allocation request
	 *
	 * The new and old PBA lists are ordered by tree level and how many
	 * entries are used depends on the height of the current VBD tree.
	 *
	 * The items from the free PBA list replace the free PBAs in the
	 * FT's type 2 node.
	 *
	 * \param  current      the current generation
	 * \param  num_blocks   number of free blocks that must be allocated
	 * \param  new_pba      list of new physical block addresses
	 * \param  old_pba      list of old physical block addresses
	 * \param  tree_height  current height of the VBD tree
	 * \param  free_pba     list of physical block address that will
	 *                      be freed (marked as reserved) during the
	 *                      allocation
	 * \param  free_blocks  number of valid items in the free PBA list
	 * \param  req_prim     primitive of the original request that triggered
	 *                      the FT operation
	 * \param  vba          virtual-block-address of the original request
	 *                      that triggered the FT operation
	 */
	void submit_request(Cbe::Generation             const  current,
	                    uint32_t                    const  num_blocks,
	                    /* refer to tree_height for number of valid elements */
	                    Cbe::Physical_block_address const  new_pba[Translation::MAX_LEVELS],
	                    Cbe::Type_1_node_info       const  old_pba[Translation::MAX_LEVELS],
	                    Cbe::Height                 const  tree_height,
	                    Cbe::Physical_block_address const  free_pba[Translation::MAX_LEVELS],
	                    uint32_t                    const  free_blocks,
	                    Cbe::Primitive              const &req_prim,
	                    Cbe::Virtual_block_address  const  vba)
	{
		(void)free_blocks;

		if (_num_blocks) {
			return;
		}

		/*
		 * Reset module state
		 *
		 * (Rather than having multiple members a proper abstraction
		 *  would be worthwhile.)
		 */
		_num_blocks     = num_blocks;
		_found_blocks   = 0;
		_current_type_2 = { 0, Io_entry::State::INVALID, 0 };

		_do_update           = false;
		_do_wb               = false;
		_wb_done             = false;
		_wb_data.finished    = false;

		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			_wb_io[i].state = Io_entry::State::INVALID;
		}

		_reset_query_prim();

		/*
		 * Prepare the write-back data that is used later on by
		 * the Write_back module.
		 */
		_wb_data.prim        = req_prim;
		_wb_data.gen         = current;
		_wb_data.vba         = vba;
		_wb_data.tree_height = tree_height;

		/*
		 * Store given lists in the module.
		 *
		 * (The free and old PBA lists are part of the write-back data
		 *  as we have to pass them on to the Write_back module b/c it
		 *  needs the addresses for the updating the nodes.
		 *
		 *  Also putting the lists into a proper structure would allow
		 *  for statically size match checking...)
		 */

		Genode::memcpy(_wb_data.new_pba, new_pba, sizeof (_wb_data.new_pba));
		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			if (new_pba[i]) {
				MOD_DBG("new[", i, "]: ", new_pba[i]);
			}
		}

		Genode::memcpy(_wb_data.old_pba, old_pba, sizeof (_wb_data.old_pba));
		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			if (old_pba[i].pba) {
				MOD_DBG("old[", i, "]: ", old_pba[i].pba);
			}
		}

		Genode::memcpy(_free_pba, free_pba, sizeof (_free_pba));
		for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
			if (free_pba[i]) {
				MOD_DBG("free[", i, "]: ", free_pba[i]);
			}
		}
	}

	/**
	 * Check if the entry is usable for allocation
	 *
	 * A node is useable either if it is currently not reserved,
	 * or if it is reserved by the generation it was freed in is
	 * not covered by any active snapshot, i.e., it was already
	 * free before any snapshot was created and is therefor not
	 * in use anymore.
	 *
	 * \param  active        list of current active snapshots
	 * \param  last_secured  last secured generation
	 * \param  node          reference to the to be checked entry
	 *
	 * \return true if the entry is useable, false otherwise
	 */
	bool _leaf_useable(Cbe::Snapshot     const active[Cbe::NUM_SNAPSHOTS],
	                   Cbe::Generation   const last_secured,
	                   Cbe::Type_ii_node const &node) const
	{
		// XXX check could be done outside
		if (!node.reserved) { return true; }

		Cbe::Generation const f_gen = node.free_gen;
		Cbe::Generation const a_gen = node.alloc_gen;
		Cbe::Generation const s_gen = last_secured;

		bool free = false;
		/*
		 * If the node was freed before the last secured generation,
		 * check if there is a active snapshot that might be using the node,
		 * i.e., its generation is after the allocation generation and before
		 * the free generation.
		 */
		if (f_gen <= s_gen) {

			bool in_use = false;
			for (uint64_t i = 0; i < Cbe::NUM_SNAPSHOTS; i++) {
				Cbe::Snapshot const &b = active[i];
				if (!b.valid()) { continue; }

				// MOD_DBG("snap: ", b);
				Cbe::Generation const b_gen = b.gen;

				bool const is_free = (f_gen <= b_gen || a_gen >= (b_gen + 1));

				in_use |= !is_free;
				if (in_use) { break; }
			}

			free = !in_use;
		}
		// MOD_DBG(free ? "REUSE" : " RESERVE", " PBA: ", node.pba,
		//         " f: ", f_gen, " a: ", a_gen);
		return free;
	}

	/**
	 * Execute module
	 *
	 * Since the FT module incorporates a number of other modules or
	 * needs access to the data ofther modules, we pass them in from
	 * the outside to limit the copying of data.
	 *
	 * \param  active        list of currently active snapshots
	 * \param  last_secured  last secured generation
	 * \param  trans_data    reference to data used by the Translation module
	 * \param  cache         reference to the cache module
	 * \param  cache_data    reference to the data used by the Cache module
	 * \param  query_data    reference to the dat used for the processing
	 *                       of querying a branch in the FT
	 * \param  time          reference to the time object
	 *
	 * \return true if some progress was made, otherwise false
	 */
	bool execute(Cbe::Snapshot    const  active[Cbe::NUM_SNAPSHOTS],
	             Cbe::Generation  const  last_secured,
	             Translation_Data       &trans_data,
	             Cache                  &cache,
	             Cache_Data             &cache_data,
	             Query_data             &query_data,
	             Time                   &time)
	{
		/* nothing to do, return early */
		if (!_num_blocks) {
			return false;
		}

		bool progress = false;

		/**************************
		 ** Translation handling **
		 **************************/

		/*
		 * Submit new request for querying a branch in the FT
		 */
		while (true) {
			if (!_trans->acceptable()) { break; }
			if (!_current_query_prim.valid()) { break; }

			MOD_DBG("trans submit: ", _current_query_prim);
			_trans->submit_primitive(_root, _root_gen, _root_hash, _current_query_prim);

			/*
			 * Make the query primitive invalid after we successfully submitted
			 * the request to trigger the break above. It will be made valid again
			 * for next query.
			 */
			_current_query_prim.operation = Cbe::Primitive::Operation::INVALID;
			progress |= true;
		}

		/*
		 * Execute the Translation module and depending on the result
		 * invoke the Cache module.
		 */
		progress |= _trans->execute(trans_data);
		while (true) {
			Cbe::Primitive p = _trans->peek_generated_primitive();
			if (!p.valid()) { break; }

			MOD_DBG("trans peek generated: ", p);

			Cbe::Physical_block_address const pba = p.block_number;
			if (!cache.data_available(pba)) {
				MOD_DBG("cache data not available: ", pba);

				if (cache.cxx_request_acceptable(pba)) {
					cache.cxx_submit_request(pba);
					// XXX it stands to reason if we have to set
					// progress |= true;
					//     in this case to prevent the CBE from
					//     stalling
				}
				break;
			} else {
				MOD_DBG("cache data available: ", pba);

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

			/*
			 * Translation has finished, we have the PBA of the type 2
			 * node, request nodes data.
			 */
			_current_type_2 = {
				.pba   = prim.block_number,
				.state = Io_entry::State::PENDING,
				.index = Cache_Index { 0 },
			};

			MOD_DBG(prim);

			/*
			 * To later update the free-tree, store the inner type 1 nodes
			 * of the branch.
			 *
			 * (Currently not implemented.)
			 */
			if (!_trans->get_type_1_info(prim,
			                             _query_branch[_current_query_branch].trans_info)) {
				MOD_ERR("could not get type 1 info");
			}

			_trans->drop_completed_primitive(prim);
			progress |= true;
		}

		/***************************
		 ** Query free leaf nodes **
		 ***************************/

		/*
		 * The type 2 node's data was read successfully, now look
		 * up potentially free blocks.
		 */
		if (_current_type_2.complete()) {

			/*
			 * (Invalidating the primitive here should not be necessary b/c it
			 *  was already done by the Translation module, not sure why its still
			 *  there.)
			 */
			_current_query_prim.operation = Cbe::Primitive::Operation::INVALID;

			MOD_DBG("_current_type_2 complete");

			/*
			 * Check each entry in the type 2 node
			 */
			Cbe::Type_ii_node *node =
				reinterpret_cast<Cbe::Type_ii_node*>(&query_data.item[0]);
			for (size_t i = 0; i < Cbe::TYPE_2_PER_BLOCK; i++) {

				/*
				 * Ignore invalid entries.
				 *
				 * (It stands to reason if pba == 0 or pba == INVALID_PBA should
				 *  be used - unfortunately its done inconsistently throughout the
				 *  CBE. The reasons is that the 'cbe_block' uses a RAM dataspace
				 *  as backing store which is zeroed out initiall which made the
				 *  this cheap and pba 0 normally contains a superblock anyway.)
				 */
				Cbe::Physical_block_address const pba = node[i].pba;
				if (!pba) { continue; }

				bool const useable = _leaf_useable(active, last_secured, node[i]);

				if (useable) {
					/* set VBA on the first useable entry, NOP for remaining entries */
					if (_query_branch[_current_query_branch].vba == Cbe::INVALID_VBA) {
						_query_branch[_current_query_branch].vba = _current_query_prim.block_number;
					}

					uint32_t &free_blocks = _query_branch[_current_query_branch].free_blocks;
					_query_branch[_current_query_branch].pba[free_blocks] = pba;
					_query_branch[_current_query_branch].free_blocks++;

					_found_blocks++;
					MOD_DBG("found free pba: ", pba);
				}

				/* break off early */
				if (_num_blocks == _found_blocks) {
					break;
				}
			}
			/*
			 * (Rather than always increasing the current query branch,
			 *  only do that when we actually found free blocks.
			 *  Somehow or other, querying only 8 branches is not enough
			 *  for large trees and we have to change the implementation
			 *  later.)
			 */
			_current_query_branch++;

			/*
			 * Reset I/O helper to disarm complete check above.
			 *
			 * (Again, the module is in desperate need for proper state
			 *  handling.)
			 */
			_current_type_2.state = Io_entry::State::INVALID;

			bool const end_of_tree = (_current_query_prim.block_number + _tree_helper->degree() >= _tree_helper->leafs());
			if (_found_blocks < _num_blocks) {

				/*
				 * In case we did not find enough free blocks, set the write-back
				 * data. The arbiter will call 'peek_completed_primitive()' and will
				 * try to discard snapshots.
				 */
				if (end_of_tree) {
					Genode::warning("could not find enough usable leafs: ", _num_blocks - _found_blocks, " missing");
					_wb_data.finished = true;
					_wb_data.prim.success = Cbe::Primitive::Success::FALSE;
				} else {

					/* arm query primitive and check next type 2 node */
					_current_query_prim.block_number += _tree_helper->degree();
					_current_query_prim.operation = Cbe::Primitive::Operation::READ;
				}
			} else if (_num_blocks == _found_blocks) {

				/*
				 * Here we iterate over all query branches and will fill in all newly
				 * allocated blocks consecutively in the new PBA list.
				 */
				uint32_t i = 0;
				for (uint32_t b = 0; b < _current_query_branch; b++) {
					for (uint32_t n = 0; n < _query_branch[b].free_blocks; n++) {

						/* store iterator out-side so we start from the last set entry */
						for (; i < Translation::MAX_LEVELS; i++) {

							/*
							 * Same convention as during the invalid entries check,
							 * pba == 0 means we have to fill in a new block.
							 */
							if (!_wb_data.new_pba[i]) {
								Cbe::Physical_block_address const pba = _query_branch[b].pba[n];
								_wb_data.new_pba[i] = pba;
								MOD_DBG("use free branch: ", b, " n: ", n, " pba: ", pba);
								break;
							}
						}
					}
				}

				_do_update = true;
			}

			progress |= true;
		}


		/***********************************
		 ** Update meta-data in free-tree **
		 ***********************************/

		if (_do_update) {

			bool data_available = true;

			/*
			 * Make sure all needed inner nodes are in the cache.
			 *
			 * (Since there is only cache used throughout the CBE,
			 *  VBD as well as FT changes will lead to flushing in case
			 *  the generation gets sealed.)
			 */
			for (uint32_t b = 0; b < _current_query_branch; b++) {
				Query_branch &qb = _query_branch[b];

				/*
				 * Start at 1 as the FT translation only cares about the inner
				 * nodes.
				 */
				for (uint32_t i = 1; i <= _tree_helper->height(); i++) {
					Cbe::Physical_block_address const pba = qb.trans_info[i].pba;

					if (!cache.data_available(pba)) {

						if (cache.cxx_request_acceptable(pba)) {
							cache.cxx_submit_request(pba);
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
				//
				//    (Rather than brute-forcing its way through,
				//     implement post-fix tree traversel, which
				//     would omit the unnecessary hashing - it
				//     gets computational expensive quickly.)
				//////////////////////////////////////

				uint32_t wb_cnt = 0;
				for (uint32_t b = 0; b < _current_query_branch; b++) {

					Query_branch &qb = _query_branch[b];
					for (uint32_t i = 1; i <= _tree_helper->height(); i++) {

						Cbe::Physical_block_address const pba = qb.trans_info[i].pba;
						Cache_Index     const idx   = cache.data_index(pba, time.timestamp());
						Cbe::Block_data &data = cache_data.item[idx.value];
						bool const type2_node = (i == 1);

						/*
						 * Exchange the PBA in the type 2 node entries
						 */
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
						}
						/*
						 * Update the inner type 1 nodes
						 */
						else {
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

							/*  for now the root node is a special case */
							if (i == _tree_helper->height()) {

								Sha256_4k::Data const &hash_data =
									*reinterpret_cast<Sha256_4k::Data const*>(&data);
								Sha256_4k::hash(hash_data, hash);
								Genode::memcpy(_root_hash.values, hash.values, sizeof (Cbe::Hash));
							}
						}

						/*
						 * Only add blocks once, which is a crude way to merge
						 * the same inner nodes in all branches.
						 */
						bool already_pending = false;
						for (uint32_t i = 0; i < wb_cnt; i++) {
							if (_wb_io[i].pba == pba) {
								already_pending = true;
								break;
							}
						}

						if (already_pending) { continue; }

						/*
						 * Add block to write-back I/O list
						 */
						_wb_io[wb_cnt].pba = pba;
						_wb_io[wb_cnt].state = Io_entry::State::PENDING;
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

			// XXX why check here for !_wb_done? should be guarded by
			//     the previous check
			if (!wb_ongoing && !_wb_done) {
				_do_wb = false;
				_wb_done = true;
				_wb_data.finished = true;
				_wb_data.prim.success = Cbe::Primitive::Success::TRUE;
				progress |= true;
			}
		}

		MOD_DBG("progress: ", progress);
		return progress;
	}

	/**
	 * Get the next generated primitive
	 *
	 * \return valid primitive in case generated primitive
	 *         is pending, otherwise an invalid primitive is returned
	 */
	Cbe::Primitive peek_generated_primitive() const
	{
		/* current type 2 node */
		if (_current_type_2.pending()) {
			Cbe::Primitive p {
				.tag          = Tag::IO_TAG,
				.operation    = Cbe::Primitive::Operation::READ,
				.success      = Cbe::Primitive::Success::FALSE,
				.block_number = _current_type_2.pba,
				.index        = 0
			};
			MOD_DBG(p);
			return p;
		}

		/* write-back I/O */
		if (_do_wb) {
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				if (_wb_io[i].pending()) {
					Cbe::Primitive p {
						.tag          = Tag::WRITE_BACK_TAG,
						.operation    = Cbe::Primitive::Operation::WRITE,
						.success      = Cbe::Primitive::Success::FALSE,
						.block_number = _wb_io[i].pba,
						.index        = 0
					};
					MOD_DBG(p);
					return p;
				}
			}
		}

		return Cbe::Primitive { };
	}

	/**
	 * Get index of the data buffer belonging to the given primitive
	 *
	 * This method must only be called after executing
	 * 'peek_generated_primitive' returned a valid primitive.
	 *
	 * \param  p  reference to primitive the data belongs to
	 *
	 * \return index of data buffer
	 */
	Index peek_generated_data_index(Cbe::Primitive const &prim) const
	{
		Index idx { .value = Cbe::Index::INVALID };

		switch (prim.tag) {
		case Tag::IO_TAG:
			idx.value = 0;
			break;
		case Tag::WRITE_BACK_TAG:
		{
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				if (prim.block_number != _wb_io[i].pba) { continue; }

				if (!_wb_io[i].pending()) {
					Genode::warning(__func__, ": ignore invalid WRITE_BACK_TAG primitive");
					break;
				}

				idx.value = _wb_io[i].index.value;
				break;
			}
			break;
		}
		default: break;
		}

		if (idx.value == Cbe::Index::INVALID) {
			throw -1;
		}

		return idx;
	}

	/**
	 * Discard given generated primitive
	 *
	 * This method must only be called after executing
	 * 'peek_generated_primitive' returned a valid primitive.
	 *
	 * \param  p  reference to primitive
	 */
	void drop_generated_primitive(Cbe::Primitive const &prim)
	{
		switch (prim.tag) {
		case Tag::IO_TAG:
			_current_type_2.state = Io_entry::State::IN_PROGRESS;
			break;
		case Tag::WRITE_BACK_TAG:
		{
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				if (prim.block_number == _wb_io[i].pba) {
					if (_wb_io[i].pending()) {
						_wb_io[i].state = Io_entry::State::IN_PROGRESS;
					} else {
						MOD_DBG("ignore invalid WRITE_BACK_TAG primitive: ", prim);
					}
				}
			}
			break;
		}
		default:
			MOD_ERR("invalid primitive: ", prim);
			throw -1;
			break;
		}
	}

	/**
	 * Mark the primitive as completed
	 *
	 * This method must only be called after executing
	 * 'peek_generated_primitive' returned a valid primitive.
	 *
	 * \param p  reference to Primitive that is used to mark
	 *           the corresponding internal primitive as completed
	 */
	void mark_generated_primitive_complete(Cbe::Primitive const &prim)
	{
		switch (prim.tag) {
		case Tag::IO_TAG:
			if (_current_type_2.in_progress()) {
				_current_type_2.state = Io_entry::State::COMPLETE;
			} else {
				MOD_DBG("ignore invalid I/O primitive: ", prim);
			}
			break;
		case Tag::WRITE_BACK_TAG:
		{
			for (uint32_t i = 0; i < Translation::MAX_LEVELS; i++) {
				if (prim.block_number == _wb_io[i].pba) {
					if (_wb_io[i].in_progress()) {
						_wb_io[i].state = Io_entry::State::COMPLETE;

						if (prim.success == Cbe::Primitive::Success::FALSE) {
							// XXX propagate failure
							MOD_ERR("failed primitive: ", prim);
						}
					} else {
						MOD_DBG("ignore invalid WRITE_BACK_TAG primitive: ", prim,
						        " entry: ", i, " state: ", (uint32_t)_wb_io[i].state);
					}
				}
			}
			break;
		}
		default:
			MOD_ERR("invalid primitive: ", prim);
			throw -1;
		break;
		}
	}

	/**
	 * Check for any completed primitive
	 *
	 * The method will always a return a primitive and the caller
	 * always has to check if the returned primitive is in fact a
	 * valid one.
	 *
	 * \return a valid Primitive will be returned if there is an
	 *         completed primitive, otherwise an invalid one
	 */
	Cbe::Primitive peek_completed_primitive()
	{
		if (_wb_data.valid()) {
			return _wb_data.prim;
		}
		return Cbe::Primitive { };
	}

	/**
	 * Get write-back data belonging to a completed primitive
	 *
	 * This method must only be called after 'peek_completed_primitive'
	 * returned a valid primitive.
	 *
	 * \param p   reference to the completed primitive
	 *
	 * \return write-back data
	 */
	Write_back_data const &peek_completed_wb_data(Cbe::Primitive const &prim) const
	{
		if (!prim.equal(_wb_data.prim)) {
			MOD_ERR("invalid primitive: ", prim);
			throw -1;
		}

		MOD_DBG(prim);
		return _wb_data;
	}

	/**
	 * Discard given completed primitive
	 *
	 * This method must only be called after 'peek_completed_primitive'
	 * returned a valid primitive.
	 *
	 * \param  p  reference to primitive
	 */
	void drop_completed_primitive(Cbe::Primitive const &prim)
	{
		if (!prim.equal(_wb_data.prim)) {
			MOD_ERR("invalid primitive: ", prim);
			throw -1;
		}

		MOD_DBG(prim);

		/* reset state */
		_wb_data.finished = false;

		/* request finished, ready for a new one */
		_num_blocks = 0;
	}
};

#undef MOD_NAME

#endif 	/* _CBE_FREE_TREE_H_ */
