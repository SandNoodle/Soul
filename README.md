# Soul

Soul is a statically typed scripting language designed for use in games.

```c
import std;

fn main :: void
{
	# This is a handy comment!
	let greetings : str = "Hello, Soul!";
	std::print("{}", greetings);
}
```

## Features

**Soul** is:
* **Fast**. Compiled bytecode is a lot faster and quicker to execute than other scripting languages at the cost of initial compilation time. (TODO:
  See [benchmarks]().)
* **Small**. Whole language and it's standard library is implemented in less than **TODO** LoC and compiles to just under **TODO** kB!
* **Extensible**. Created with modularity in mind, nothing stops you from writing custom optimization passes, providing custom allocators, etc.
* **Cross-Platform**. Soul works out-of-the-box on all major platforms without any additional setup.
* **Statically typed**.

## Getting started

See [Getting Started](docs/GETTING_STARTED.md) for more information.

## License

See [License](LICENSE.md) for more information.

# Special thanks
