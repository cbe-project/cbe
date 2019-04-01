/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _SPARK_OBJECT_H_
#define _SPARK_OBJECT_H_

/* Genode includes */
#include <base/stdint.h>

namespace Spark {

	template <Genode::size_t BYTES>
	struct Object
	{
		Genode::uint8_t _space[BYTES] { };
	} __attribute__((packed));
}

#endif /* _SPARK_OBJECT_H_ */
