This set of libraries is progress towards implementing an way to interact
with Haskell's RTS from another Haskell program.

For example, you could use this library to
* Implement a memory profiler, written in Haskell
* Precisely analyse other heap properties such as retainers

# How does it work?

We call the process we want to debug the debuggee and the process which does
the debugging the debugger.
Whilst the debuggee is
running it calls the C function `start` which creates a unix domain socket (`/tmp/ghc-debug` for now). The debugger starts and connects to the socket.

Once the debugger is connected it can send requests to the debuggee to control
and inspect the RTS. The requests API is specified as follows:

```
-- | A request sent from the debugger to the debuggee parametrized on the result type.
data Request a where
    -- | Request protocol version
    RequestVersion :: Request Word64
    -- | Pause the debuggee.
    RequestPause :: Request ()
    -- | Resume the debuggee.
    RequestResume :: Request ()
    -- | Request the debuggee's root pointers.
    RequestRoots :: Request [ClosurePtr]
    -- | Request a set of closures.
    RequestClosures :: [ClosurePtr] -> Request [GenClosure InfoTablePtr ClosurePtr]
    -- | Request a set of info tables.
    RequestInfoTables :: [InfoTablePtr] -> Request [StgInfoTable]
```

So far only the version, pause and resume functions are tested (and work).

We will add some more functions to this interface, for example, a request to
call `findPtr` and also a way to mark objects of interest so we can retrieve their
addresses in the same manner as `RequestRoots`.

# How do I use it?

In short, you don't, yet.

The project needs to built with a development version of GHC from [this](https://gitlab.haskell.org/ghc/ghc/tree/wip/ghc-debug) branch.

Then you can use normal cabal commands to build this library.

In order to make a process debuggable it needs to call the socket initialisation
function `start`, see the `test-debug` executable for exactly how to do this.

## Testing

There are two test executables `test-debug` and `debugger` which are used to
test the library.

`test-debug` is an interruptable program which prints out an incrementing
counter each second. `debugger` starts and immediately attaches to `test-debug`
and makes some requests. So the way to test the library is to first start `test-debug`
and then start `debugger`. There are lots of helpeful traces to see what's going
on with each process.
