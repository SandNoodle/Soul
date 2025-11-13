#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

int main(int argc, char* argv[])
{
	if (argc < 1) {
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
