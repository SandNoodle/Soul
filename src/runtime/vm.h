#ifndef RUNTIME_VM_H
#define RUNTIME_VM_H

#include <stdint.h>

namespace soul
{
	struct chunk;
	enum class error_code : uint32_t;

	class vm
	{
		public:
			vm() = delete;
			vm(const vm&) = delete;
			vm(vm&&) = delete;
			~vm() = delete;

			/**
			 * Exectues target bytecode chunk.
			 *
			 * @params chunk Compiled bytecode chunk.
			 * @return @TODO !!!Return type!!!
			 */
			void execute(const chunk& chunk);

		private:
	};
} // namespace soul

#endif // RUNTIME_VM_H
