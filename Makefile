.PHONY: build test clean

# Build the library and tests
build:
	stack build

# Run the benchmark golden tests
test:
	stack test

# build documentation
docs:
	stack haddock

# Clean build artifacts
clean:
	stack clean

# Build with file watching
watch:
	stack build --file-watch

# Run GHCi with the library loaded
repl:
	stack ghci golds-gym:lib
