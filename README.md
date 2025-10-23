# The Soul Scripting Langauge

Soul is a statically typed scripting language designed to work in a high performance scenarios (such as games).

```python
import std;

fn main :: void {
    # This is a comment.
    let name: str = "Soul";
    std::print("Hello, {name}!");
}
```

:warning: Soul is in early stages of development - expect breaking changes!

## Features

Soul is:

* **Fast**. Soul's VM aims to be as performant as possible (See [BENCHMARKS.md](BENCHMARKS.md)).
* **(Easily) embeddable**. Just grab [soul.h](src/soul.h), `#define SOUL_IMPLEMENTATION` in (exactly) one file before
  including it.
* **Cross-platform**. Works out of the box on Windows and Linux without any additional setup.
* **Extensible**. Soul's compiler is created in modularity in mind, nothing stops you from writing your own optimization
  passes.
* **Statically typed**.

## Quick start

This repository contains both embedded runtime and the compiler project.

- For embedding the runtime see [here](docs/QUICK_START_RUNTIME.md).
- For building the compiler see [here](docs/QUICK_START_COMPILER.md).

## Documentation

See [docs](docs/) directory for an up-to-date project documentation.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md)

## License

See [LICENSE.md](LICENSE.md)