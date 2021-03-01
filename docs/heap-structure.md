# Heap Structure

A basic understanding of the structure of the heap is a prerequisite for using
ghc-debug seriously.

All objects allocated on the heap are referred to as closures.

The layout of a closure has a standard form of an info table followed by the specific
payload for that closure type.

## Common Heap Objects

The layout of common heap objects can be found on the [GHC wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects).

