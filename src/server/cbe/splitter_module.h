/*
 * \brief  CBE C++ prototype splitter module
 * \author Josef Soentgen
 * \date   2019-01-21
 *
 * The splitter module is used to split a given Block request
 * into a number of primitives where each one covers Cbe::BLOCK_SIZE
 * bytes of data.
 */

/*
 * Copyright (C) 2019 Genode Labs GmbH
 *
 * This file is part of the Genode OS framework, which is distributed
 * under the terms of the GNU Affero General Public License version 3.
 */

#ifndef _CBE_SPLITTER_MODULE_H_
#define _CBE_SPLITTER_MODULE_H_

/* local includes */
#include <cbe/types.h>


namespace Cbe { namespace Module {

	class Splitter;

} /* namespace Module */ } /* namespace Cbe */

class Cbe::Module::Splitter
{
	private:

		Block::Request       _current_request    {   };
		Cbe::Primitive       _current_primitive  {   };
		Number_of_primitives _current_primitives { 0 };

	public:

		/**
		 * Get the number of primitives the request will generate
		 *
		 * \return number of primitives
		 */
		Number_of_primitives number_of_primitives(Block::Request const &request) const
		{
			return request.count;
		}

		/**
		 * Check if the module can accept a new request
		 *
		 * \return true if a request can be accepted, otherwise false
		 */
		bool request_acceptable() const
		{
			return !_current_request.operation_defined();
		}

		/**
		 * Submit a new request
		 *
		 * The request will be copied to the internal buffer.
		 * The method may only be called after 'request_acceptable' was
		 * executed and returned true.
		 *
		 * \param r  reference to the Block request
		 */
		void submit_request(Block::Request const &request, Tag const tag)
		{
			auto operation = [] (Block::Request::Operation op)
			{
				switch (op) {
					case Block::Request::Operation::READ:
						return Cbe::Primitive::Operation::READ;
					case Block::Request::Operation::WRITE:
						return Cbe::Primitive::Operation::WRITE;
					case Block::Request::Operation::SYNC:
						return Cbe::Primitive::Operation::SYNC;
					default:
						return Cbe::Primitive::Operation::INVALID;
				}
			};

			_current_primitive = Cbe::Primitive {
				.tag          = tag,
				.operation    = operation(request.operation),
				.success      = Cbe::Primitive::Success::FALSE,
				.block_number = request.block_number,
				.index        = 0,
			};

			_current_request    = request;
			_current_primitives = number_of_primitives(request);
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
			return _current_primitive;
		}

		/**
		 * Take the next generated primitive
		 *
		 * This method must only be called after executing
		 * 'peek_generated_primitive' returned true.
		 *
		 * \return next valid generated primitive
		 */
		Cbe::Primitive take_generated_primitive()
		{
			Cbe::Primitive p { };

			if (_current_primitive.index < _current_primitives) {
				p = _current_primitive;
				_current_primitive.block_number++;
				_current_primitive.index++;

				if (_current_primitive.index == _current_primitives) {
					_current_primitive = Cbe::Primitive { };
					_current_request   = Block::Request { };
				}
			}

			return p;
		}
};


#endif /* _CBE_SPLITTER_MODULE_H_ */
