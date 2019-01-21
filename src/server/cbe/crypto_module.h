/*
 * \brief  CBE C++ prototype crypto module
 * \author Josef Soentgen
 * \date   2019-01-21
 *
 * The crypto module is used to encrypt or decrypt a given Block_data
 * sized data chunk. The crypto stuff is provided by the Aes_cbc_4k
 * SPARK library.
 */

/*
 * Copyright (C) 2019 Genode Labs GmbH
 *
 * This file is part of the Genode OS framework, which is distributed
 * under the terms of the GNU Affero General Public License version 3.
 */

#ifndef _CBE_CRYPTO_MODULE_H_
#define _CBE_CRYPTO_MODULE_H_

/* library includes */
#include <aes_cbc_4k/aes_cbc_4k.h>

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	template <unsigned> class Crypto;

} /* namespace Module */ } /* namespace Cbe */


template <unsigned N>
class Cbe::Module::Crypto : Noncopyable
{
	public:

		struct Primitive
		{
			Cbe::Primitive const &primitive;
			Cbe::Block_data &data;

			bool valid() const { return primitive.valid(); }
		};

	private:

		Aes_cbc_4k::Key _key { };

		struct Internal_entry
		{
			Cbe::Primitive primitive { };
			Cbe::Block_data *data { nullptr };

			enum State { UNUSED, SUBMITTED, PENDING, IN_PROGRESS, COMPLETE } state { UNUSED };
		};

		Internal_entry  _entries[N]   {   };
		unsigned        _used_entries { 0 };
		Cbe::Block_data _buffer[N]    {   };

		bool _equal_primitives(Cbe::Primitive const &p1, Cbe::Primitive const &p2)
		{
			return p1.block_number == p2.block_number
				&& p1.index        == p2.index
				&& p1.operation    == p2.operation;
		}

	public:

		Crypto(char const *key /* must be 32b total */)
		{
			Genode::memcpy(&_key, key, 32);
		}

		/**
		 * Check if the module can accept a new primitive
		 *
		 * \return true if a primitive can be accepted, otherwise false
		 */
		bool acceptable() const
		{
			return _used_entries < N;
		}

		/**
		 * Submit a new primitive
		 *
		 * The primitive will be copied to the internal buffer and the Block_data
		 * reference will be stored as a reference. The method may only be called
		 * after 'acceptable' was executed and returned true. The new primitive is
		 * marked as submitted and waits for execution.
		 *
		 * \param p  reference to the Primitive
		 * \param d  reference to a Block_data object
		 */
		void submit_primitive(Cbe::Primitive const &p, Block_data &d)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state == Internal_entry::UNUSED) {
					_entries[i].primitive = p;
					_entries[i].data      = &d;
					_entries[i].state     = Internal_entry::SUBMITTED;

					_used_entries++;
					return;
				}
			}

			error("failed to accept request");
		}

		/**
		 * Process all submitted primitives
		 *
		 * This method tries to process any submitted request. In case it is a
		 * write request the Block_data will be encrypted and the primitive is
		 * marked as complete. On the other hand, if it is a read request, the
		 * primitive will be marked as pending and will later on be passed on
		 * to a I/O module.
		 *
		 * \return true if any submitted primitive was processed
		 */
		bool execute()
		{
			bool progress = false;

			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::SUBMITTED) { continue; }

				if (_entries[i].primitive.write()) {

					Aes_cbc_4k::Block_number const block_number {
						_entries[i].primitive.block_number };

					Aes_cbc_4k::Plaintext const *src =
						reinterpret_cast<Aes_cbc_4k::Plaintext*>(_entries[i].data);

					Aes_cbc_4k::Ciphertext *dst =
						reinterpret_cast<Aes_cbc_4k::Ciphertext*>(&_buffer[i]);

					Aes_cbc_4k::encrypt(_key, block_number, *src, *dst);

					_entries[i].state = Internal_entry::COMPLETE;
					_entries[i].primitive.success = Cbe::Primitive::Success::TRUE;
				} else {
					_entries[i].state = Internal_entry::PENDING;
				}
				progress = true;
			}

			return progress;
		}

		/**
		 * Check for any completed primitive
		 *
		 * The method will always a return a primitive. The caller can
		 * check which subsequent module must be used to process the
		 * Primitive further as the process-chain might differ for read
		 * and write primtives. The caller always has to check if the
		 * returned primitive is in fact a valid one.
		 *
		 * \return a valid Primitive will be returned if there is an
		 *         completed primitive, otherwise an invalid one
		 */
		Cbe::Primitive peek_completed_primitive() const
		{
			for (unsigned i = 0; i < N; i++)
				if (_entries[i].state == Internal_entry::COMPLETE)
					return _entries[i].primitive;

			return Cbe::Primitive { };
		}

		/**
		 * Take the next completed primitive
		 *
		 * This method must only be called after executing
		 * 'peek_completed_primitive' returned true.
		 *
		 * \return takes next valid completed primitive and removes it
		 *         from the module
		 */
		Cbe::Primitive take_completed_primitive()
		{
			Cbe::Primitive p { };

			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state == Internal_entry::COMPLETE) {
					_entries[i].state = Internal_entry::UNUSED;
					_used_entries--;

					p = _entries[i].primitive;
					break;
				}
			}
			return p;
		}

		/**
		 * Check for any generated primitive
		 *
		 * The method will always a return a primitive. The caller can
		 * check which subsequent module must be used to process the
		 * Primitive further as the process-chain might differ for read
		 * and write primtives. The caller always has to check if the
		 * returned primitive is in fact a valid one.
		 *
		 * \return a valid Primitive will be returned if there is an
		 *         completed primitive, otherwise an invalid one
		 */
		Cbe::Primitive peek_generated_primitive() const
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state == Internal_entry::PENDING) {
					return _entries[i].primitive;
				}
			}

			return Cbe::Primitive { };
		}

		/**
		 * Take the next generated crypto primitive
		 *
		 * This method must only be called after executing
		 * 'peek_generated_primitive' returned true.
		 *
		 * \return next valid generated crypto primitive
		 */
		Cbe::Primitive take_generated_primitive()
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state == Internal_entry::PENDING) {
					return _entries[i].primitive;
				}
			}

			struct No_generated_primitive_available { };
			throw No_generated_primitive_available();
		}

		/**
		 * Take the generated data buffer beloging to given primitive
		 *
		 * This method must only be called after executing
		 * 'peek_generated_primitive' returned true.
		 *
		 * \param  p  reference to primitive the data belongs to
		 *
		 * \return reference to data
		 */
		Cbe::Block_data &take_generated_data(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (   _entries[i].state == Internal_entry::PENDING
				    && _equal_primitives(_entries[i].primitive, p)) {
					return _buffer[i];
				}
			}

			struct No_data_available { };
			throw No_data_available();
		}

		/**
		 * Discard given primitive
		 *
		 * \param  p  reference to primitive
		 */
		void discard_generated_primitive(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (   _entries[i].state == Internal_entry::PENDING
				    && _equal_primitives(_entries[i].primitive, p)) {
					_entries[i].state = Internal_entry::IN_PROGRESS;
					return;
				}
			}

			struct Cannot_discard_generated_primitive { };
			throw Cannot_discard_generated_primitive();
		}

		/**
		 * Mark the primitive as completed
		 *
		 * \param p  reference to Primitive that is used to lookup
		 *           the corresponding internal primitive as completed
		 */
		void mark_completed_primitive(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::IN_PROGRESS) { continue; }
				if (!_equal_primitives(_entries[i].primitive, p)) { continue; }

				if (_entries[i].primitive.read() && p.success == Cbe::Primitive::Success::TRUE) {

					Aes_cbc_4k::Block_number const block_number {
						_entries[i].primitive.block_number };

					Aes_cbc_4k::Ciphertext const *src =
						reinterpret_cast<Aes_cbc_4k::Ciphertext*>(&_buffer[i]);

					Aes_cbc_4k::Plaintext *dst =
						reinterpret_cast<Aes_cbc_4k::Plaintext*>(_entries[i].data);

					Aes_cbc_4k::decrypt(_key, block_number, *src, *dst);
				}

				_entries[i].state = Internal_entry::COMPLETE;
				_entries[i].primitive.success = p.success;
				break;
			}
		}
};

#endif /* _CBE_CRYPTO_MODULE_H_ */
