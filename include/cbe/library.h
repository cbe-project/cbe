/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_LIBRARY_H_
#define _CBE_LIBRARY_H_

/* Genode includes */
#include <base/stdint.h>
#include <base/output.h>

/* CBE includes */
#include <cbe/types.h>
#include <cbe/util.h>
#include <cbe/spark_object.h>


namespace Cbe {

	using namespace Genode;

	class Library;

	Genode::uint32_t object_size(Library const &);

} /* namespace Cbe */


class Cbe::Library : public Cbe::Spark_object<216648>
{
	private:

		/*
		 * Ada/SPARK compatible bindings
		 *
		 * Ada functions cannot have out parameters. Hence we call Ada
		 * procedures that return the 'progress' result as last out parameter.
		 */

		void _has_io_request(Request &, Io_buffer::Index &);

		void _client_data_ready(Request &);
		void _obtain_client_data(Request const &, Crypto_plain_buffer::Index &, bool &);
		void _client_data_required(Request &);
		void _supply_client_data(Time::Timestamp const, Request const &, Block_data const &, bool &);

		void _crypto_cipher_data_required(Request &, Crypto_plain_buffer::Index &);
		void _crypto_plain_data_required(Request &, Crypto_cipher_buffer::Index &);

	public:

	/**
	 * Constructor
	 *
	 * \param  block   reference to the Block::Connection used by the I/O
	 *                 module
	 * \param  sbs     array of all super-blocks, will be copied
	 *
	 * \param  current_sb  super-block that should be used initially
	 */
	Library(Superblocks       const &sbs,
	        Superblocks_index const &current_sb);

	/**
	 * Print current active super-block/snapshot information to LOG
	 */
//	void dump_cur_sb_info() const;

	/**
	 * Get highest virtual-block-address useable by the current active snapshot
	 *
	 * \return  highest addressable virtual-block-address
	 */
	Virtual_block_address max_vba() const;

	/**
	 * Execute one loop of the CBE
	 *
	 * \param  now               current time as timestamp
	 * \param  show_progress     if true, generate a LOG message of the current
	 *                           progress (basically shows the progress state of
	 *                           all modules)
	 * \param  show_if_progress  if true, generate LOG message only when progress was
	 *                           acutally made
	 */
	void execute(Io_buffer            &io_buf,
	             Crypto_plain_buffer  &crypto_plain_buf,
	             Crypto_cipher_buffer &crypto_cipher_buf,
	             Time::Timestamp       now);

	/**
	 * Return whether the last call to 'execute' has made progress or not
	 */
	bool execute_progress() const;

	/**
	 * Check if the CBE can accept a new requeust
	 *
	 * \return true if a request can be accepted, otherwise false
	 */
	bool client_request_acceptable() const;

	/**
	 * Submit a new request
	 *
	 * This method must only be called after executing 'request_acceptable'
	 * returned true.
	 *
	 * \param request  block request
	 */
	void submit_client_request(Request const &request);

	/**
	 * Check for any completed request
	 *
	 * \return a valid block request will be returned if there is an
	 *         completed request, otherwise an invalid one
	 */
	Request peek_completed_client_request() const;

	/**
	 * Drops the completed request
	 *
	 * This method must only be called after executing
	 * 'peek_completed_request' returned a valid request.
	 *
	 */
	void drop_completed_client_request();

	/*
	 * Backend block I/O
	 */

	/**
	 * Submit read request data from the backend block session to the CBE
	 *
	 * The given data will be transfered to the CBE.
	 *
	 * \param  request  reference to the request from the CBE
	 * \param  data     reference to the data associated with the
	 *                  request
	 *
	 * \return  true if the CBE acknowledged the request
	 */
	void io_request_completed(Io_buffer::Index const &data_index,
	                          bool             const  success);

	/**
	 * Return a write request for the backend block session
	 *
	 * \param result  valid request in case the is one pending that
	 *                needs data, otherwise an invalid one is returned
	 */
	Request has_io_request(Io_buffer::Index &data_index)
	{
		Request result { };
		_has_io_request(result, data_index);
		return result;
	}

	/**
	 * Obtain data for write request for the backend block session
	 *
	 * The CBE will transfer the payload to the given data.
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Request
	 *
	 * \return  true if the CBE could process the request
	 */
	void io_request_in_progress(Io_buffer::Index const &data_index);

	/*
	 * Frontend block I/O
	 */

	/**
	 * Return a client request that provides data to the frontend block data
	 *
	 * \param result  valid request in case the is one pending that
	 *                needs data, otherwise an invalid one is returned
	 */
	Request client_data_ready()
	{
		Request result { };
		_client_data_ready(result);
		return result;
	}

	/**
	 * Return primitive index
	 */
	uint64_t give_data_index(Request const &request) const;

	/**
	 * Return data for given client read request
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Block::Request
	 * \return          'true' on return if the CBE could process the request
	 */
	bool obtain_client_data(Request              const &request,
	                        Crypto_plain_buffer::Index &data_index)
	{
		bool result = false;
		_obtain_client_data(request, data_index, result);
		return result;
	}

	/**
	 * Return a client request that provides data to the frontend block data
	 *
	 * \param result  valid request in case the is one pending that
	 *                needs data, otherwise an invalid one is returned
	 */
	Request client_data_required()
	{
		Request result { };
		_client_data_required(result);
		return result;
	}

	/**
	 * Request access to data for writing client data
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Block::Request
	 *
	 * \return  true if the CBE could process the request
	 */
	bool supply_client_data(Time::Timestamp const now,
	                        Request         const &request,
	                        Block_data      const &data)
	{
		bool result = false;
		_supply_client_data(now, request, data, result);
		return result;
	}

	bool is_sealing_generation() const;
	bool is_securing_superblock() const;

	void start_sealing_generation();
	void start_securing_superblock();

	bool cache_dirty() const;
	bool superblock_dirty() const;


	/**
	 * CBE requests encrytion
	 *
	 * \param result  valid request in case the is one pending that
	 *                needs encrytion, otherwise an invalid one is
	 *                returned
	 */
	Request crypto_cipher_data_required(Crypto_plain_buffer::Index &data_index)
	{
		Request result { };
		_crypto_cipher_data_required(result, data_index);
		return result;
	}

	/**
	 *  Return plain data for given encryption request
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Block::Request
	 */
	void crypto_cipher_data_requested(
		Crypto_plain_buffer::Index const &data_index);

	/**
	 *  Collect cipher data for given completed encryption request
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Block::Request
	 *
	 * \return  true if the CBE could obtain the encrypted data,
	 *          otherwise false
	 */
	void supply_crypto_cipher_data(Crypto_cipher_buffer::Index const &data_index,
	                               bool                        const  data_valid);

	/**
	 * CBE requests decryption
	 *
	 * \param result  valid request in case the is one pending that
	 *                needs decrytion, otherwise an invalid one is
	 *                returned
	 */
	Request crypto_plain_data_required(Crypto_cipher_buffer::Index &data_index)
	{
		Request result { };
		_crypto_plain_data_required(result, data_index);
		return result;
	}

	/**
	 *  Return cipher data for given decryption request
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Block::Request
	 *
	 * \return  true if the CBE could supply the ciphr data,
	 *          otherwise false
	 */
	void crypto_plain_data_requested(
		Crypto_cipher_buffer::Index const &data_index);

	/**
	 *  Collect plain data for given completed decryption request
	 *
	 * \param  request  reference to the Block::Request processed
	 *                  by the CBE
	 * \param  data     reference to the data associated with the
	 *                  Block::Request
	 *
	 * \return  true if the CBE could obtain the decrypted data,
	 *          otherwise false
	 */
	void supply_crypto_plain_data(Crypto_plain_buffer::Index const &data_index,
	                              bool                       const  data_valid);
};

#endif /* _CBE_LIBRARY_H_ */
