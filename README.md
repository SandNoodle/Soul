# Soul

Soul is a statically typed scripting language designed for use in games.

```python
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
* **Fast**. Soul's virtual machine aims to be as performant as possible. See [benchmarks](TODO))
* **Small**. Whole language and it's standard library is implemented in less than **TODO** LoC and compiles to just under **TODO** kB!
* **Customizable**. You can modify specific elements to tune the performance. See `soul_config.h`.
* **Extensible**. Created with modularity in mind, nothing stops you from writing custom optimization passes, providing custom allocators, etc.
* **Cross-Platform**. Soul works out-of-the-box on Windows and Linux without any additional setup.
* **Statically typed**. ;)

## Getting started

See [Getting Started](docs/GETTING_STARTED.md) for more information.

## License

See [License](LICENSE.md) for more information.

# Special thanks
* [Crafting Interpreters](https://craftinginterpreters.com/)
* [JStar](https://github.com/bamless/jstar)

