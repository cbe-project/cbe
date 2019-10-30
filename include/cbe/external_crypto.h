/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_EXTERNAL_CRYPTO_H_
#define _CBE_EXTERNAL_CRYPTO_H_

/* Genode includes */
#include <base/stdint.h>
#include <base/output.h>

/* CBE includes */
#include <cbe/types.h>
#include <cbe/spark_object.h>


namespace External {

	class Crypto;

	Genode::uint32_t object_size(Crypto const &);

} /* namespace Cbe */


class External::Crypto : public Cbe::Spark_object<16568>
{
	public:

		struct Key_data { unsigned char value[32]; } __attribute__((packed));

	private:

		/*
		 * Ada/SPARK compatible bindings
		 *
		 * Ada functions cannot have out parameters. Hence we call Ada
		 * procedures that return the 'progress' result as last out parameter.
		 */

		void _set_key(unsigned const, unsigned const, Key_data const &, bool &);
		void _execute(bool &);

		void _submit_encryption_request(Cbe::Request const &, Cbe::Block_data const &, bool &);
		void _supply_cipher_data(Cbe::Request const &, Cbe::Block_data &, bool &);

		void _submit_decryption_request(Cbe::Request const &, Cbe::Block_data const &, bool &);
		void _supply_plain_data(Cbe::Request const &, Cbe::Block_data &, bool &);

	public:

	/**
	 * Constructor
	 */
	Crypto();

	/**
	 * Set key material
	 *
	 * \param slot  slot where the key should be stored
	 *              (XXX slot managed is not really fleshed out,
	 *                   maybe it is enough just to have 2 slots...)
	 * \param id   id of the given key
	 * \param key  key data
	 *
	 * \return  true if setting key was successful, otherwise false
	 *          (XXX method could be made void as well, the external
	 *           crypto should not care about overriding already set
	 *           keys)
	 */
	bool set_key(unsigned slot, unsigned id, Key_data const &key)
	{
		bool result = false;
		_set_key(slot, id, key, result);
		return result;
	}

	/**
	 *
	 * Execute Crypto
	 *
	 * \return  true if progress was made, otherwise false
	 */
	bool execute()
	{
		bool result = false;
		_execute(result);
		return result;
	}

	/**
	 * Encryption request acceptable
	 *
	 * \return  true if new request can be accepted, otherwise false
	 */
	bool encryption_request_acceptable() const;

	/**
	 * Submit encryption request
	 *
	 * \param  request  reference to request
	 * \param  data     data belonging to the request that should be
	 *                  encrypted
	 * \param  key_id   id of the key the data should be encrypted with
	 *
	 * \return  true if new request was submitted, otherwise false
	 *          (XXX having a return value is not strictly needed)
	 */
	bool submit_encryption_request(Cbe::Request const &request,
	                               Cbe::Block_data const &data,
	                               unsigned        const  key_id)
	{
		(void)key_id; // XXX

		bool result = false;
		_submit_encryption_request(request, data, result);
		return result;
	}

	/**
	 * Peek completed encryption request
	 *
	 * \return  valid request if any encryption request was completed,
	 *          otherwise an invalid request
	 */
	Cbe::Request peek_completed_encryption_request() const;

	/**
	 * Give access to encrypted data
	 *
	 * This method must only be called after 'peek_completed_encryption_request'
	 * returned a valid request and only this request may be used.
	 *
	 * \param  request  reference to the request the data belongs to
	 * \param  data     reference to the buffer where the cipher data
	 *                  should be stored
	 *
	 * \return  true if access could be given, otherwise false
	 */
	bool supply_cipher_data(Cbe::Request const &request,
			Cbe::Block_data    &data)
	{
		bool result = false;
		_supply_cipher_data(request, data, result);
		return result;
	}

	/**
	 * Decryption request acceptable
	 *
	 * \return  true if new request can be accepted, otherwise false
	 */
	bool decryption_request_acceptable() const;

	/**
	 * Submit decryption request
	 *
	 * \param  request  reference to request
	 * \param  data     data belonging to the request that should be
	 *                  decrypted
	 * \param  key_id   id of the key the data should be decrypted with
	 *
	 * \return  true if new request was submitted, otherwise false
	 *          (XXX having a return value is not strictly needed)
	 */
	bool submit_decryption_request(Cbe::Request    const &request,
	                               Cbe::Block_data const &data,
	                               unsigned        const  key_id)
	{
		(void)key_id; // XXX

		bool result = false;
		_submit_decryption_request(request, data, result);
		return result;
	}

	/**
	 * Peek completed decryption request
	 *
	 * \return  valid request if any decryption request was completed,
	 *          otherwise an invalid request
	 */
	Cbe::Request peek_completed_decryption_request() const;

	/**
	 * Give access to decrypted data
	 *
	 * This method must only be called after 'peek_completed_decryption_request'
	 * returned a valid request and only this request may be used.
	 *
	 * \param  request  reference to the request the data belongs to
	 * \param  data     reference to the buffer where the cipher data
	 *                  should be stored
	 *
	 * \return  true if access could be given, otherwise false
	 */
	bool supply_plain_data(Cbe::Request const &request,
	                       Cbe::Block_data    &data)
	{
		bool result = false;
		_supply_plain_data(request, data, result);
		return result;
	}
};

#endif /* _CBE_EXTERNAL_CRYPTO_H_ */
