# Haddock [![CI][CI badge]][CI page] [![Hackage][Hackage badge]][Hackage page]

Haddock is the standard tool for generating documentation from Haskell code.
Full documentation about Haddock itself can be found in the `doc/` subdirectory,
in [reStructedText format][ReST] format.

## Project overview

This project consists of three packages:

 * `haddock`: provides the `haddock` executable. It is implemented as a tiny
    wrapper around `haddock-api`'s `Documentation.Haddock.haddock` function.

 * `haddock-api`: contains the program logic of the `haddock` tool.
   [The haddocks for the `Documentation.Haddock` module][Documentation.Haddock]
   offer a good overview of the functionality.

 * `haddock-library`: is concerned with the parsing and processing of the
   Haddock markup language. Unlike the other packages, it is expected to build
   on a fairly wide range of GHC versions.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) to see how to make contributions to the
project.


[CI page]: https://travis-ci.org/haskell/haddock
[CI badge]: https://travis-ci.org/haskell/haddock.svg?branch=ghc-8.10
[Hackage page]: https://hackage.haskell.org/package/haddock
[Hackage badge]: https://img.shields.io/hackage/v/haddock.svg
[ReST]: http://www.sphinx-doc.org/en/stable/rest.html
[Documentation.Haddock]: http://hackage.haskell.org/package/haddock-api/docs/Documentation-Haddock.html
[cabal v2]: http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html
