#ifndef COMPILER_CHUNK_H
#define COMPILER_CHUNK_H

#include <stdint.h>
#include <vector>

namespace soul
{
	using byte = uint8_t;

	/**
	 * Represents bytecode chunk of instructions that can be exectued by a vm.
	 */
	struct chunk
	{
		std::vector<byte> code;
		/* std::vector<>; */ // @TODO Constants
	};
} // namespace soul

#endif // COMPILER_CHUNK_H
