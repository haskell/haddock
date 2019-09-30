# Haddock, a Haskell Documentation Tool [![Build Status][TravisCI badge]][Travis CI]

See [Description on Hackage][Hackage haddock].

## Source code documentation

Full documentation can be found in the `doc/` subdirectory, in
[reStructedText format](http://www.sphinx-doc.org/en/stable/rest.html)
format.


## Project overview

This project consists of three packages:

* [__haddock__][Hackage haddock] - This provides the `haddock` executable and
  is implemented as a tiny wrapper around _haddock-api_'s
  `Documentation.Haddock.haddock` function.

* [__haddock-api__][Hackage haddock] - This contains the program logic of the
  `haddock` tool. The `Documentation.Haddock` module offers a good overview of
  provided functionality.

* [__haddock-library__][Hackage haddock-library] - Concerned with parsing and
  processing the Haddock markup language.


## Contributing

Please create issues when you have any problems and pull requests if you have some code.

## Hacking

To get started you'll need a latest GHC release installed.
Clone the repository:

```bash
git clone https://github.com/haskell/haddock.git
cd haddock
```

and then proceed using your favourite build tool.

#### Using [`cabal v2-build`][cabal-install]

```bash
cabal v2-build     # build
cabal v2-test      # build & run the test suites for `haddock` package only
cabal v2-test all  # build & run the test suites for all the packages
```

#### Using Stack

```bash
stack init
stack build
# run the test suite
export HADDOCK_PATH="$(stack exec which haddock)"
stack test
```

### Git Branches

If you're a GHC developer and want to update Haddock to work with your
changes, you should be working on the `ghc-head` branch.
See [instructions on the GHC wiki][GHC submodules] for an example workflow.

More generally, due to depending on [`ghc`-the-library][Hackage ghc], _haddock_
and _haddock-api_ only build with one version of GHC at a time. Consequently,
Haddock has a branch for each major GHC version, and that branch should should
build with that GHC.

```
git checkout ghc-8.8 && cabal v2-build -w ghc-8.8.1
git checkout ghc-8.6 && cabal v2-build -w ghc-8.6.5
git checkout ghc-8.4 && cabal v2-build -w ghc-8.4.4
...
```

Every time a new major GHC release happens, the default development branch for
Haddock changes.

### Updating test outputs

If you've made changes to Haddock which alter the output of existing test cases
(and the change is intended), it is important to update the testsuite. With a
new enough version of Cabal, this can be done simultaneously for all testsuites.

```
cabal new-test --test-option='--accept'
```

[TravisCI badge]:          https://travis-ci.org/haskell/haddock.svg?branch=ghc-8.8
[TravisCI]:                https://travis-ci.org/haskell/haddock
[Hackage haddock]:         https://hackage.haskell.org/package/haddock
[Hackage haddock-api]:     https://hackage.haskell.org/package/haddock-api
[Hackage haddock-library]: https://hackage.haskell.org/package/haddock-api
[Hackage ghc]:             https://hackage.haskell.org/package/ghc
[cabal-install]:           http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html
[GHC submodules]:          https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions/git/submodules
