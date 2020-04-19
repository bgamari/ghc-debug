This set of libraries is progress towards implementing a way to interact
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
    RequestVersion :: Request Word32
    -- | Pause the debuggee.
    RequestPause :: Request ()
    -- | Resume the debuggee.
    RequestResume :: Request ()
    -- | Request the debuggee's root pointers.
    RequestRoots :: Request [ClosurePtr]
    -- | Request a set of closures.
    RequestClosures :: [ClosurePtr] -> Request [RawClosure]
    -- | Request a set of info tables.
    RequestInfoTables :: [InfoTablePtr] -> Request [RawInfoTable]
    -- | Wait for the debuggee to pause itself and then
    -- execute an action. It currently impossible to resume after
    -- a pause caused by a poll.
    RequestPoll :: Request ()
    -- | A client can save objects by calling a special RTS method
    -- This function returns the closures it saved.
    RequestSavedObjects :: Request [ClosurePtr]
    -- | Calls the debugging `findPtr` function and returns the retainers
    RequestFindPtr :: ClosurePtr -> Request [ClosurePtr]
    -- | Request the pointer bitmap for an info table.
    RequestBitmap :: InfoTablePtr -> Request PtrBitmap
    -- | Request the description for an info table.
    RequestConstrDesc :: ClosurePtr -> Request ConstrDesc
```

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
and then start `debugger`. There are lots of helpful traces to see what's going
on with each process.

### Automated Testing

There are `hspec` tests, that can be run with `cabal`:

```
cabal new-build all && cabal new-test all
```
