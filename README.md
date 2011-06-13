Git For Haskell
===============

A more Haskell-like layer on top of [Bindings.Libgit2](https://github.com/sakari/hlibgit2) which is itself a shallow layer on top of [libgit2](https://github.com/libgit2/libgit2)

Building
--------

You will need to build and install from source hlibgit2 [hlibgit2](https://github.com/sakari/hlibgit2). If you want to use ghci or run doctests with the library you will need to build hlibgit2 without the bundled libgit2. 

Build, test and install with [cabal](http://www.haskell.org/cabal/):

    cabal configure --enable-test
    cabal build
    cabal test
    cabal install --enable-documentation

To run doctests you will need a bit hacked version of [doctest-haskell](https://github.com/sol/doctest-haskell.git) from [my repo](https://github.com/sakari/doctest-haskell/tree/sakari%2Fcombine-examples-for-doc-node). After building than run:

    ./ruby doctest.rb

Usage
-----

See the produced haddock docs for usage examples
