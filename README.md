This set of libraries is progress towards implementing a way to interact
with Haskell's RTS from another Haskell program.

For example, you could use this library to
* Implement a memory profiler, written in Haskell
* Precisely analyse other heap properties such as retainers

# How does it work?

We call the process we want to debug the debuggee and the process which does
the debugging the debugger.
Whilst the debuggee is
running it calls the C function `start` which creates a unix domain socket (which is set from `GHC_DEBUG_SOCKET`). The debugger starts and connects to the socket.

Once the debugger is connected it can send requests to the debuggee to control
and inspect the RTS.

# How do I use it?

The project needs to built with a development version of GHC from
[this](https://gitlab.haskell.org/ghc/ghc/tree/wip/ghc-debug-ghc) branch. Then you
can use normal cabal commands to build this library. The easiest way to do this
is with nix. The nix shell uses the development version of GHC so you don't have to build it yourself.

```
$ nix-shell
> cabal new-build all
```

In order to make a process debuggable it needs to wrap it's main function in the `withGhcDebug` function, see the `debug-test` executable for exactly how to do this.
See `test/Test.hs` for an example of how to run and debug the `debug-test`
executable. There are quite a lot of examples which query and traverse the heap
in different ways.

## Manual Testing / Examples

There are two test executables `debug-test` and `debugger` which are used to
test the library (manually). Run the test with:

```
$ cabal new-run -- debugger
```

`debugger` starts and immediately attaches to `debug-test` and makes some
requests.  There are lots of helpful traces to see what's going on with each
process. `debug-test` is the program we're debugging. It prints out an
incrementing counter each second.

### Automated Testing

There are `hspec` tests, that can be run with `cabal`:

```
cabal new-build all && cabal new-test all
```

### Unexpected Build Failures

If you encounter dependencies failing to build but there's a patch for
the library in head.hackage then you may need to delete `~/.cabal/packages/head.hackage.org`
so that the fresh patch is visible. This is probably a bug in cabal!
