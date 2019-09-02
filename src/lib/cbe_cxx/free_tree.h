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

	Genode::uint32_t object_size(Free_tree const &);
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
struct Cbe::Free_tree : Cbe::Spark_object<8352>
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
	 * As we might need to write multiple branches back to the disk
	 * make sure we have enough entries available.
	 *
	 * (Maybe it makes sense to reorganize all these array in array
	 *  structures in a better way.)
	 */
	enum { MAX_WB_IO = Translation::MAX_LEVELS*MAX_QUERY_BRANCHES, };

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
	Free_tree(Cbe::Physical_block_address const  root,
	          Cbe::Generation             const  root_gen,
	          Cbe::Hash                   const &hash,
	          Cbe::Height                 const  height,
	          Cbe::Degree                 const  degree,
	          Cbe::Number_of_leaves       const  leafs);

	/**
	 * Instruct the free-tree module to retry the current pending request
	 *
	 * This method relies on the requests information being available, i.e.,
	 * _do not_ call 'drop_completed_primitive' if one intends to retry the
	 * allocation after discarding a snapshot (in case the former allocation
	 * did not succeed the module will emit a failed completed primitive.
	 */
	void retry_allocation();

	/**
	 * Check if the module is able to accept a new request
	 *
	 * \return true if a request can be accepted, otherwise false
	 */
	bool request_acceptable() const;

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
	                    Cbe::Physical_block_address const  new_pba[Translation::MAX_LEVELS],
	                    Cbe::Type_1_node_info       const  old_pba[Translation::MAX_LEVELS],
	                    Cbe::Height                 const  tree_height,
	                    Cbe::Physical_block_address const  free_pba[Translation::MAX_LEVELS],
	                    Cbe::Primitive              const &req_prim,
	                    Cbe::Virtual_block_address  const  vba);

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
	void execute(Cbe::Snapshot    const  active[Cbe::NUM_SNAPSHOTS],
	             Cbe::Generation  const  last_secured,
	             Translation_Data       &trans_data,
	             Cache                  &cache,
	             Cache_Data             &cache_data,
	             Query_data             &query_data,
	             Time::Timestamp         now);

	bool execute_progress() const;

	/**
	 * Get the next generated primitive
	 *
	 * \return valid primitive in case generated primitive
	 *         is pending, otherwise an invalid primitive is returned
	 */
	Cbe::Primitive peek_generated_primitive() const;

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
	Index peek_generated_data_index(Cbe::Primitive const &prim) const;

	/**
	 * Discard given generated primitive
	 *
	 * This method must only be called after executing
	 * 'peek_generated_primitive' returned a valid primitive.
	 *
	 * \param  p  reference to primitive
	 */
	void drop_generated_primitive(Cbe::Primitive const &prim);

	/**
	 * Mark the primitive as completed
	 *
	 * This method must only be called after executing
	 * 'peek_generated_primitive' returned a valid primitive.
	 *
	 * \param p  reference to Primitive that is used to mark
	 *           the corresponding internal primitive as completed
	 */
	void mark_generated_primitive_complete(Cbe::Primitive const &prim);

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
	Cbe::Primitive peek_completed_primitive();

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
	Write_back_data const &peek_completed_wb_data(Cbe::Primitive const &prim) const;

	/**
	 * Discard given completed primitive
	 *
	 * This method must only be called after 'peek_completed_primitive'
	 * returned a valid primitive.
	 *
	 * \param  p  reference to primitive
	 */
	void drop_completed_primitive(Cbe::Primitive const &prim);
};

#undef MOD_NAME

#endif 	/* _CBE_FREE_TREE_H_ */
