/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_IO_MODULE_H_
#define _CBE_IO_MODULE_H_

/* Genode includes */
#include <block_session/connection.h>

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	struct Io_data
	{
		enum { NUM_ITEMS = 1, };
		Cbe::Block_data item[NUM_ITEMS];
	} __attribute__((packed));

	class Block_io;

} /* namespace Module */ } /* namespace Cbe */

#define MOD_NAME "IO"

class Cbe::Module::Block_io : Noncopyable
{
	public:

		static constexpr Genode::uint32_t N = Io_data::NUM_ITEMS;

		struct Index
		{
			static constexpr Genode::uint32_t INVALID = Io_data::NUM_ITEMS;
			Genode::uint32_t value;
			bool valid() const { return value != INVALID; }
		};

	private:

		struct Internal_entry
		{
			Cbe::Tag       orig_tag { };
			Cbe::Primitive primitive { };
			// Cbe::Block_data *data { nullptr };

			Block::Packet_descriptor packet { };

			enum State { UNUSED, PENDING, IN_PROGRESS, COMPLETE } state { UNUSED };

			void print(Genode::Output &out) const
			{
				auto state_string = [](State state) {
					switch (state) {
					case State::UNUSED:      return "unused";
					case State::PENDING:     return "pending";
					case State::IN_PROGRESS: return "in_progress";
					case State::COMPLETE:    return "complete";
					}
					return "unknown";
				};
				Genode::print(out, "primitive.block_number: ", primitive.block_number,
				              " state: ", state_string(state));
			}
		};

		Internal_entry _entries[N]   {   };
		unsigned       _used_entries { 0 };

		Block::Connection<> &_block;
		Block::Session::Info const _info { _block.info() };

		struct Fake_sync_primitive     { };
		struct Invalid_block_operation { };

		Block::Packet_descriptor _convert_from(Cbe::Primitive const &primitive)
		{
			auto operation = [] (Cbe::Primitive::Operation op) {
				switch (op) {
				case Cbe::Primitive::Operation::READ:
					return Block::Packet_descriptor::READ;
				case Cbe::Primitive::Operation::WRITE:
					return Block::Packet_descriptor::WRITE;
				case Cbe::Primitive::Operation::SYNC:
					throw Fake_sync_primitive();
				default:
					throw Invalid_block_operation();
				}
			};

			return Block::Packet_descriptor(_block.alloc_packet(Cbe::BLOCK_SIZE),
					operation(primitive.operation),
					primitive.block_number, Cbe::BLOCK_SIZE / _info.block_size);
		}

		bool _equal_packets(Block::Packet_descriptor const &p1,
		                    Block::Packet_descriptor const &p2) const
		{
			return p1.block_number() == p2.block_number() && p1.operation() == p2.operation();
		}

		bool _equal_primitives(Cbe::Primitive const &p1, Cbe::Primitive const &p2)
		{
			return p1.block_number == p2.block_number
			    && p1.index        == p2.index
			    && p1.operation    == p2.operation;
		}

	public:

		struct Block_size_mismatch { };

		Block_io(Block::Connection<> &block) : _block(block)
		{
			if (_info.block_size > Cbe::BLOCK_SIZE || _info.block_size % Cbe::BLOCK_SIZE != 0) {
				MOD_ERR("back end block size must either be equal to "
				        "or be a multiple of ", Cbe::BLOCK_SIZE);
				throw Block_size_mismatch();
			}
		}

		/**
		 * Check if the module can accept new primitives
		 *
		 * \return true if a primitive can be accepted, otherwise false
		 */
		bool primitive_acceptable() const { return _used_entries < N; }

		/**
		 * Submit a new primitive
		 *
		 * The primitive will be copied to the internal buffer and the Block_data
		 * reference will be stored as a reference. The method may only be called
		 * after 'acceptable' was executed and returned true. The new primitive is
		 * marked as pending and waits for execution.
		 *
		 * \param p  reference to the Primitive
		 * \param d  reference to a Block_data object
		 */
		void submit_primitive(Tag const tag, Primitive const &p,
		                      Io_data &io_data, Block_data const &data)
		{
			for (unsigned i = 0; i < N; i++) {
				Internal_entry &e = _entries[i];

				if (e.state != Internal_entry::UNUSED) { continue; }

				e.primitive = p;
				e.primitive.tag = tag;
				e.orig_tag  = p.tag;

				if (p.write()) {
					Genode::memcpy(&io_data.item[i], &data, sizeof (Cbe::Block_data));
				}
				// e.data      = &data;
				e.state     = Internal_entry::PENDING;

				_used_entries++;
				MOD_DBG("primitive: ", p, p.read() ? " READ ====================================== " : "");
				return;
			}
		}

		/**
		 * Process all pending primitives
		 *
		 * This method tries to process any pending request by submitting it
		 * to the Block session back end and at the same time tries to
		 * acknowledge all already finished Block requests.
		 *
		 * \return true if any pending primitive was used to trigger a request
		 * at the back end Block session.
		 */
		bool execute(Io_data &io_data)
		{
			bool progress = false;

			(void)io_data;

			/* first mark all finished I/O ops */
			while (_block.tx()->ack_avail()) {
				Block::Packet_descriptor packet = _block.tx()->get_acked_packet();

				for (unsigned i = 0; i < N; i++) {
					if (_entries[i].state != Internal_entry::IN_PROGRESS) { continue; }
					if (!_equal_packets(_entries[i].packet, packet)) { continue; }

					if (_entries[i].primitive.read()) {
						void const * const src = _block.tx()->packet_content(packet);
						Genode::size_t    size = Cbe::BLOCK_SIZE;
						// void      * const dest = reinterpret_cast<void*>(_entries[i].data);
						void      * const dest = reinterpret_cast<void*>(&io_data.item[i]);
						Genode::memcpy(dest, src, size);
					}

					_entries[i].state = Internal_entry::COMPLETE;
					_entries[i].primitive.success = packet.succeeded() ? Primitive::Success::TRUE
						: Primitive::Success::FALSE;

					_block.tx()->release_packet(_entries[i].packet);
					MOD_DBG("ACK prim: ", _entries[i].primitive);
					progress = true;
				}
			}

			/* second submit new I/O ops */
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::PENDING) { continue; }


				if (!_block.tx()->ready_to_submit()) { break; }

				try {
					Block::Packet_descriptor packet = _convert_from(_entries[i].primitive);

					if (_entries[i].primitive.write()) {
						// void const * const src = reinterpret_cast<void*>(_entries[i].data);
						void const * const src = reinterpret_cast<void*>(&io_data.item[i]);
						Genode::size_t    size = Cbe::BLOCK_SIZE;
						void      * const dest = _block.tx()->packet_content(packet);
						Genode::memcpy(dest, src, size);
					}

					_entries[i].state  = Internal_entry::IN_PROGRESS;
					_entries[i].packet = packet;

					_block.tx()->submit_packet(_entries[i].packet);
					MOD_DBG("SUBMIT prim: ", _entries[i].primitive);
					progress = true;
				}
				catch (Fake_sync_primitive) {
					_entries[i].state = Internal_entry::COMPLETE;
					_entries[i].primitive.success = Primitive::Success::TRUE;
					MOD_DBG("ACK SYNC prim: ", _entries[i].primitive);
					break;
				}
				catch (Invalid_block_operation) {
					_entries[i].state = Internal_entry::COMPLETE;
					_entries[i].primitive.success = Primitive::Success::FALSE;
					MOD_DBG("ACK INVALID prim: ", _entries[i].primitive);
					break;
				}
				catch (Block::Session::Tx::Source::Packet_alloc_failed) { break; }
			}

			return progress;
		}

		/**
		 * Check for any completed primtive
		 *
		 * \return true if there is an completed primitive, otherwise
		 *         false
		 */
		Primitive peek_completed_primitive()
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::COMPLETE) { continue; }

				return _entries[i].primitive;
			}

			return Cbe::Primitive { };
		}

		Index peek_completed_data_index(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::COMPLETE
				    || !_equal_primitives(p, _entries[i].primitive)) { continue; }

				// return *_entries[i].data;
				return Index { .value = i };
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}

		Cbe::Tag peek_completed_tag(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::COMPLETE
				    || !_equal_primitives(p, _entries[i].primitive)) { continue; }

				return _entries[i].orig_tag;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}

		/**
		 * Take the next completed primtive
		 *
		 * This method must only be called after executing
		 * 'peek_completed_primitive' returned true.
		 *
		 * \return takes next valid completed primtive and removes it
		 *         from the module
		 */
		void drop_completed_primitive(Cbe::Primitive const &p)
		{
			for (unsigned i = 0; i < N; i++) {
				if (_entries[i].state != Internal_entry::COMPLETE
				    || !_equal_primitives(p, _entries[i].primitive)) { continue; }

				_entries[i].state = Internal_entry::UNUSED;
				_used_entries--;
				return;
			}

			MOD_ERR("invalid primitive: ", p);
			throw -1;
		}
};

#undef MOD_NAME

#endif /* _CBE_IO_MODULE_H_ */
