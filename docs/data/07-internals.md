# Internal Implementation Details

This chapter of the documentation describes some of the internal implementation
details.

## Closure Representation

Perhaps the most important part of the library is the Haskell representation of
the heap.

Closures are represented by the `DebugClosure pap string s b` type. Each different
closure type has its own constructor. The type parameters correspond to the
four different types of pointers which can be found in closures.

* `b` - Normal closure pointers
* `s` - Stack pointer
* `string` - Pointer to a string (used for ConstrDesc)
* `pap` - Pointer to a PAP payload

For example, constructor closures have the standard layout, an info table followed
by pointers, followed by data with the addition of a pointer to a description
for that closure. Therefore it is represented as follows:

```
  ConstrClosure
    { info       :: !StgInfoTableWithPtr
    , ptrArgs    :: ![b]            -- ^ Pointer arguments
    , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
    , constrDesc :: !string
    }
```

Type parameters mark the recursive positions.

Unlike `ghc-heap`, closures are just things which have info tables. The
representation of the info table in `ghc-debug` also contains the pointer to
the info table as well as the decoded table.

The easiest way to generically interact with a `DebugClosure pap string s b` is
using the `Quadtraversable` class, which is a generalisation of `Traversable` to
work with 4 type parameters.

```
class Quadtraversable m where
  quadtraverse ::
    Applicative f => (a -> f b)
                  -> (c -> f d)
                  -> (e -> f g)
                  -> (h -> f i)
                  -> m a c e h
                  -> f (m b d g i)
```

## Decoding Closures

After the raw closure information is requested from the debuggee, the
raw bytestring is decoded into a `DebugClosure` using the `decodeClosure` function.

There are two different ways closures are decoded.

For most closures the raw closure is mapped into memory and the `getClosureRaw`
function from `ghc-heap` is reused to decode the closure before it is converted
into the `ghc-heap` representation using `convertClosure`.

For common and usual closures, the decoding logic is implemented directly using
the `Data.Binary` interface. This turns out to be significantly faster (25%ish)
so is worthwhile for simple and common types as well as ones which ghc-heap does
not properly support such as APThunk and APStack closures.

Only one layer of a closure is decoded at a time. The information placed in
the parameter positions is sufficient to decode that parameter on its own. For
example, the `StackCont` contains the address of the stack pointer and the raw
stack frames, because this is what we need in order to query to debuggee in order
to decode it.

### Stack Frame Decoding

Stack frame decoding is implemented in `GHC.Debug.Decode.Stack`. The implementation
is complicated significantly by `RET_FUN` frames which require looking at information
other than the info table in order to know how to decode the frame. This
is why the `StackCont` has a `StackPtr` and the `RequestStackBitmap` request
is in terms of a `StackPtr` and offset rather than just an address.

## Block Cache

A significant performance optimisation was to precache all the blocks a program knows about
and then handle any requests to dereference a closure pointer firstly by looking
in the block cache, to see if we have the block, and if not getting the block
from the debuggee. All this logic is in `GHC.Debug.Client.BlockCache`.

## Request Model

The debugger connects to a running process over a socket and then issues
requests over the socket using the API defined by `GHC.Debug.Types.Request`.

## Caching

All requests are cached in the `RequestCache`.
Snapshotting is implemented by serialising the `RequestCache`. Resuming a process
clears are requests which are invalidated by a pause, such as `RequestClosure` but
doesn't remove entries such as `RequestInfoTable` and `RequestSourceLoc` which
are unchanged across the execution of a program. This is also a significant performance
improvement, when querying a live process the first traversal is far slower than subsequent pauses.
