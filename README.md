[Documentation Site](http://ghc.gitlab.haskell.org/ghc-debug)


`ghc-debug` is a set of libraries which allow you to inspect the heap of
a running Haskell program from an external debugger.

For example, you could use this library to
* Implement a memory profiler, written in Haskell - [GHC.Debug.Profile](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/Profile.hs)
* Precisely analyse other heap properties such as retainers - [GHC.Debug.Retainers](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/Retainers.hs)
* Work out any other question you want about the heap by writing your own
custom analysis. The possibilities are endless!

# Getting Started

There are two parts to using `ghc-debug`. Firstly the application you want to
inspect has to be instrumented using the `withGhcDebug` function from
`GHC.Debug.Stub`. This just wraps the normal main function of your executable,
when it is executed it will create a socket by which a debugger can connect
and issue requests to. The location of the socket can be controlled by setting
the `GHC_DEBUG_SOCKET` variable when the executable is run.

```
import GHC.Debug.Stub

main = withGhcDebug normalMain
```

Note: To enable source information you should also compile your application and
dependencies with `-finfo-table-map` and optionally `-fdistinct-constructor-tables`.

## A simple debugger

The most productive way to use `ghc-debug` is to write your own heap analysis
scripts. Fortunately, this is also quite simple. Here is a simple, complete, debugger
which connects to the `/tmp/ghc-debug` socket, requests the GC roots and then
decodes the first one up to depth 10 before printing the result to the user.

```
import GHC.Debug.Client

main = withDebuggeeConnect "/tmp/ghc-debug" p1

p1 :: Debuggee -> IO ()
p1 e = do
  pause e
  g <- run e $ do
        precacheBlocks
        (r:_) <- gcRoots
        buildHeapGraph (Just 10) r
  putStrLn (ppHeapGraph (const "") h)
```

The API for writing debuggers is described in the `GHC.Debug.Client` module.

There are many more examples in the `test/Test.hs` file.

## Snapshotting

A convenient way to use `ghc-debug` is to take a *snapshot* of the heap and then
perform further analysis on the snapshot rather than connecting to a running
process. Snapshotting utilities are in the `GHC.Debug.Snapshot` module. A
snapshot can be created using the `makeSnapshot` program, it will pause
the process and then save a snapshot to the `/tmp/ghc-debug-snapshot` file.

```
import GHC.Debug.Client
import GHC.Debug.Snapshot

main = withDebuggeeConnect "/tmp/ghc-debug" (\d -> makeSnapshot d "/tmp/ghc-debug-snapshot")
```

A snapshot can be then used for further analysis. For example, we can run `p1` on
the snapshot by using `snapshotRun` instead of `withDebuggeeConnect`. The same
programs can be used with snapshots but requests such as pausing and resuming are
just ignored.

```
import GHC.Debug.Client

main = snapshotRun "/tmp/ghc-debug-snapshot" p1
```


## High-Level Analysis

There are also some more high-level analysis tools already packaged with the
library. Mostly as an idea about what sort of thing you could program yourself.

* [Profiling](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/Profile.hs) - Profiling modes in the spirit of `-hT`.
* [Object Equivalence](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/ObjectEquiv.hs) - Detect equivalent heap objects which could be shared.
* [Count](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/Count.hs) - Simple heap statistics, total number of objects, total size and maximum object size.
* [Fragmentation](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/Fragmentation.hs) - Functions for analysis memory fragmentation including block and mblock utilisation histograms.
* [Retainers](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/Retainers.hs) - Finding paths through the heap to work out why objects are being retained.
* [Type Points From](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/TypePointsFrom.hs) - Collapse a heap graph so that nodes are info tables and edges are references between info tables. This allows you to implement the [Cork](https://www.cs.utexas.edu/users/speedway/DaCapo/papers/cork-popl-2007.pdf) leak analysis.

These analysis modes are implemented in terms of the more low-level traversal
functions.

* [Sequential Traversal](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/Trace.hs) - Traversal with low memory overhead, accounting for cycles.
* [Parallel Traversal](https://gitlab.haskell.org/ghc/ghc-debug/-/blob/master/client/src/GHC/Debug/ParTrace.hs) - Experimental Parallel Traversal with low memory overhead.

# Other Resources

* [An introduction to ghc-debug: precise memory analysis for Haskell programs](https://www.youtube.com/watch?v=9zuAsGk9xoM)


# How does it work?

We call the process we want to debug the debuggee and the process which does
the debugging the debugger.
Whilst the debuggee is
running it calls the C function `start` which creates a unix domain socket (which is set from `GHC_DEBUG_SOCKET`). The debugger starts and connects to the socket.

Once the debugger is connected it can send requests to the debuggee to control
and inspect the RTS.

# How do I use it?

You can build the libraries directly from hackage.

### Automated Testing

There are `hspec` tests, that can be run with `cabal`:

```
cabal new-test all
```

### Unexpected Build Failures

If you encounter dependencies failing to build but there's a patch for
the library in head.hackage then you may need to delete `~/.cabal/packages/head.hackage.org`
so that the fresh patch is visible. This is probably a bug in cabal!
