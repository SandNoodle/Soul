#ifndef SOUL_SOUL_H
#define SOUL_SOUL_H

#include <string>

namespace soul
{
	struct chunk;

	chunk compile(const char* str, size_t size);
	chunk compile(const std::string& script);

} // namespace soul

#endif // SOUL_SOUL_H
