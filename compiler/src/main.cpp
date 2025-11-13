#include "core/types.h"

#include <charconv>
#include <cstdio>
#include <cstdlib>
#include <string_view>
#include <thread>
#include <vector>

enum class OptimizationLevel : soul::u8
{
	/// @brief No optimizations (other than essential ones). Corresponds to Clang's O0.
	None = 0,

	/// @brief Perform only essential optimizations. Corresponds to Clang's O1.
	Low = 1,

	/// @brief Optimize program for the execution speed, but prevent some of the longer running passes from executing.
	/// Corresponds to Clang's O2.
	Medium = 2,

	/// @brief Optimize program for the execution speed. Corresponds to Clang's O3.
	High = 3,

	Default = None,
};

struct CompilerArguments
{
	std::string_view output_filename;
	std::vector<std::string_view> input_files;
	OptimizationLevel optimization_level;
	soul::u16 thread_count;
	bool output_soul_ir;
};

void print_version()
{
	// TODO: Pass Git's commit hash here (debugging).
	printf("soul %d.%d.%d (%s)\n", SOUL_VERSION_MAJOR, SOUL_VERSION_MINOR, SOUL_VERSION_PATCH, __DATE__);
	fflush(stdout);
}

void print_usage()
{
	printf("Usage: soul [OPTIONS] INPUT\n");
	printf("\n");
	printf("Options:\n");
	printf("    -h, --help        Display this message (and exit)\n");
	printf("    -v, --version     Display compiler version (and exit)\n");

	printf("    --emit-sir        [Optional] Compile the program into Soul's IR.\n");
	printf("    -O<ARG>           [Optional] Optimization level to use (default=0)\n");
	printf("    -j <ARG>          [Optional] Amount of threads to use when compiling.\n");
	printf("    -o <FILENAME>     [Required] Write output to FILENAME\n");
	fflush(stdout);
}

int main(int argc, char* argv[])
{
	if (argc < 2) {
		print_usage();
		return EXIT_SUCCESS;
	}

	std::vector<std::string_view> arguments{};
	arguments.reserve(argc - 1);
	for (std::size_t index = 1; index < argc; ++index) {
		arguments.emplace_back(std::string_view{ argv[index] });
	}

	CompilerArguments compiler_arguments{};
	using namespace std::string_view_literals;
	for (std::size_t index = 0; index < arguments.size(); ++index) {
		std::string_view argument = arguments[index];

		// Help flag takes priority and does not execute the program further.
		if (argument == "-h"sv || argument == "--help") {
			print_usage();
			return EXIT_SUCCESS;
		}

		// Version flag takes priority and does not execute the program further.
		if (argument == "-v"sv || argument == "--version") {
			print_version();
			return EXIT_SUCCESS;
		}

		if (argument == "--emit-sir") {
			compiler_arguments.output_soul_ir = true;
			continue;
		}

		if (argument.starts_with("-O"sv)) {
			compiler_arguments.optimization_level = argument == "-O0"sv ? OptimizationLevel::None
			                                      : argument == "-O1"sv ? OptimizationLevel::Low
			                                      : argument == "-O2"sv ? OptimizationLevel::Medium
			                                      : argument == "-O3"sv ? OptimizationLevel::High
			                                                            : OptimizationLevel::Default;
			continue;
		}

		if (argument == "-j"sv) {
			// NOTE: Next argument has to be a count;
			if (index + 1 >= arguments.size()) {
				printf("ERROR: Argument to option 'j' is missing.\n");
				printf("       Usage:\n");
				printf("           -j <ARG>          [Optional] Amount of threads to use when compiling.\n");
				fflush(stdout);
				return EXIT_FAILURE;
			}
			std::string_view next_argument = arguments[index + 1];
			if (!std::from_chars(next_argument.begin(), next_argument.end(), compiler_arguments.thread_count)) {
				printf("WARNING: Argument to option 'j' could not be parsed into a number - using default.\n");
				compiler_arguments.thread_count = 0;
			}
			++index;  // Skip over the count.
			continue;
		}

		if (argument == "-o"sv) {
			// NOTE: Next argument has to be a filename.
			if (index + 1 >= arguments.size()) {
				printf("ERROR: Argument to option 'o' is missing.\n");
				printf("       Usage:\n");
				printf("           -o <FILENAME>     Write output to FILENAME\n");
				fflush(stdout);
				return EXIT_FAILURE;
			}
			compiler_arguments.output_filename = arguments[index + 1];
			++index;  // Skip over the filename.
			continue;
		}

		// NOTE: If a flag was not specified, then assume we are looking at input files.
		compiler_arguments.input_files.push_back(std::move(argument));
	}

	if (compiler_arguments.input_files.empty()) {
		printf("ERROR: No input file name(s) given\n");
		fflush(stdout);
		return EXIT_FAILURE;
	}

	// NOTE: Output filename was not provided - use default.
	if (compiler_arguments.output_filename.empty()) {
		compiler_arguments.output_filename = "main";
	}

	// NOTE: Thread count was not provided - use default.
	if (compiler_arguments.thread_count == 0) {
		compiler_arguments.thread_count = std::thread::hardware_concurrency();
	}

	return EXIT_SUCCESS;
}
