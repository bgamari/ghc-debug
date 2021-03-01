# What is ghc-debug?

ghc-debug is a tool for analysing the heap of Haskell programs. The library works
in two parts. Firstly, `ghc-debug-stub` provides a wrapper function `withGhcDebug`
which you can wrap around the `main` function in your executable. Once you have
done this, when the executable starts, a socket is created which can be used to
control the process.

```haskell
import GHC.Debug.Stub

main = withGhcDebug $ do ...
```

When a debugger starts it looks for and connects to this socket in order to take control
and analyse the running process.
A debugger is a Haskell program implemented using the `ghc-debug-client` API. This
might be a user-defined analysis script or some kind of frontend UI for interactive
use. In this post we will write our debuggers as specific analysis scripts. Debuggers
request information from the instrumented application by issuing requests over
the socket.

The first set of requests are to do with pausing and resuming the running
process. The debugger usually starts by sending the pause request. It's important
the process is paused whilst it is being debugged as otherwise the garbage collector
is free to move around objects and invalidate pointers.

```haskell
-- | A request sent from the debugger to the debuggee parametrized on the result type.
data Request a where
    -- | Request protocol version
    RequestVersion :: Request Word32
    -- | Pause the debuggee.
    RequestPause :: Request ()
    -- | Resume the debuggee.
    RequestResume :: Request ()
```

The next set of requests are to do with getting basic information from the
process. `RequestRoots` will get the set of GC roots from the running process
and `RequestSavedObjects` will return a list of objects the user has chosen
to specially mark.


```haskell
    -- | Request the debuggee's root pointers.
    RequestRoots :: Request [ClosurePtr]
    -- | A client can save objects by calling a special RTS method
    -- This function returns the closures it saved.
    RequestSavedObjects :: Request [ClosurePtr]
```

The final set of requests are more internal requests which are necessary to
dereference pointers of different types and decode the heap representation into
a Haskell data type.


```haskell
    -- | Request a closure
    RequestClosure :: ClosurePtr -> Request RawClosure
    -- | Request an info table
    RequestInfoTable :: InfoTablePtr -> Request (StgInfoTableWithPtr, RawInfoTable)
```

The `GHC.Debug.Client` module provides an API to trigger these requests.
They run in the `DebugM` monad, which contains the state necessary for performing
and caching requests. A debugger will typical start by pausing the running process,
before requesting the `gcRoots` and then recursively calling `dereferenceClosure`
starting from the GC roots in order to analyse the heap.

```haskell
pause :: Debuggee -> IO ()
resume :: Debuggee -> IO ()

gcRoots :: DebugM [ClosurePtr]
savedObjects :: DebugM [ClosurePtr]
dereferenceClosure :: ClosurePtr -> DebugM SizedClosure
```


You can use the `run` method in order to run a `DebugM` action.

```haskell
run :: Debuggee -> DebugM a -> IO a
```

There are two ways which you can create a `Debuggee` to pass to the `run`
function: either by connecting to a running process or by loading and running a
snapshot. `withDebuggeeConnect` connects to the given socket name and runs the
provided script.

```haskell
-- | Connects to a debuggee, runs the action, then closes the debuggee.
withDebuggeeConnect :: FilePath  -- ^ executable name of the debuggee
                    -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                    -> (Debuggee -> IO a)
                    -> IO a
```

Snapshots can be created in order to save the state of a process and further
analyse it offline. Taking snapshots is useful as then you can easily run different
analysis scripts on the same snapshot without having to restart the process.

```haskell
-- | Make a snapshot of the current heap and save it to the given file.
snapshot :: FilePath -> DebugM ()
```

`snapshotRun` provides a way to load a snapshot and run an analysis script only
using the snapshot. This method doesn't connect to the process and so can be used
offline.

```haskell
-- | Start an analysis session using a snapshot. This will not connect to a
-- debuggee.
snapshotRun :: FilePath -> (Debuggee -> IO a) -> IO a
```

There are quite a few more useful library functions in the API but this gives
a taste about the fundamental parts of the library.
