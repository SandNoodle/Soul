# Embedding Soul's runtime

## Requirements

* (Optional) Git
* C99 compatible compiler.

## Quick start

1. Download or clone [soul.h](src/soul.h) from this repository.
2. (In exactly one file) define `SOUL_IMPLEMENTATION` beforehand.
3. Include `soul.h`.

Your file should look something like this:
```c
// ...
#define SOUL_IMPLEMENTATION
#include "soul.h"

int main(void)
{
  // ...
  return EXIT_SUCCESS;
}
```



