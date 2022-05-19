# Scheme48

Scheme48 is a toy Scheme written in Haskell, based on the book _Write Yourself A Scheme in 48 Hours_.

## Building

### Cabal

Scheme48 can be built and run with [Cabal](https://www.haskell.org/cabal/).

```bash
# build library and executable, output to `dist-newstyle`
cabal build
# run interpreter without arguments (runs REPL)
cabal run
# run interpreter with argument(s) (runs a program file)
cabal run scheme48 -- program.scm
```

### Nix

For totally deterministic builds, Scheme48 can be built with [Nix](https://nixos.org/nix/).

_NOTE: [Nix Flake](https://nixos.wiki/wiki/Flakes) support is necessary, but not yet enabled by default._

```bash
# enter a shell with `ghc`, `cabal`, etc.
nix develop
# build executable, output to `result`
nix build
# run executable without arguments
nix run
# run executable with arguments
nix run . -- program.scm
```

## Architecture

- `app`: contains the executable source code and implements the user interface (e.g. REPL).
- `src/Lisp`: implements the language itself as a library.
  - `src/Lisp/Core.hs`: contains core language datatypes shared between other modules.
  - `src/Lisp/Parse.hs`: implements parsing and is responsible for language syntax.
  - `src/Lisp/Value.hs`: provides utility functions for working with language values.
  - `src/Lisp/Interpret.hs`: implements program execution and builtins.
  - `src/Lisp/Debug.hs`: implements debugging facilities, namely pretty-printing for language values.

## Examples

There are a few examples in the `examples` directory.

- `examples/hello-world.scm`: a simple program that prints "Hello, World!"
- `examples/fizz-buzz.scm`: a program that plays [Fizz buzz](https://en.wikipedia.org/wiki/Fizz_buzz)
- `examples/bubble-sort.scm`: a program which sorts a list of numbers using the [bubble sort algorithm](https://en.wikipedia.org/wiki/Bubble_sort)
- `examples/fibonacci.scm`: two algorithms for computing elements of the Fibonacci sequence

Most examples have I/O and are meant to be run like so:

```bash
scheme48 examples/hello-world.scm

# with cabal
cabal run scheme48 -- examples/hello-world.scm

# with nix
nix run . -- examples/hello-world.scm
```

And most can also be imported into the REPL:

```scheme
(load "examples/hello-world.scm")
```