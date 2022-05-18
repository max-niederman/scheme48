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
