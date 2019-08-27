# ghc-debug: Call for Participation

`ghc-debug` is a tool for debugging the heap of a Haskell program. Think of it like `gdb`
but specialised for debugging Haskell programs. It's the tool for you if you've ever wondering things like:

* Why is a specific object not being garbage collected!?
* What is the precise breakdown of the heap at a specific point in the program!?
* What's inside all the lists in my program!?

How does it work? There are two parts to the library. The first part adds a startup hook to the RTS which
creates a socket for another process to connect to control and request information from the process.
The debugger then connects to this socket, executes a user debugging script and reports some information from the user.

It's important that there are two different processes because in order to report accurate information to the user
you have to completely pause the executation of the program you want to debug. If you don't do this then GC will
quickly invalidate any information that you obtain.

There are still many parts of the library which could be improved. At Munihac 2019 we (@bgamari and @mpickering) will
work on implementing the crucial missing features to the library.

# How to use the library

The library lives at: http://www.github.com/bgamari/ghc-debug

In order to build the library you need to use a custom version of GHC from this branch:

The easiest way to set up the correct build environment is by using this `shell.nix` script.

If you end up building `ghc` yourself then make sure you use at least the `quick` flavour so that
the RTS is built is all the necessary ways. Then you can use `cabal new-configure` to set the
path to ghc.


Once the environment is set up you can use `cabal new-build` as normal in order to build the project.
There are a few components which are useful to know about.

```
debugger: The main debugger
debug-test: An executable which is used for testing with the debugger
```

# Fundamental Goals

There are two tasks which Ben and I will focus on in order to fix some fundamental problems with the library.

## Decoding Stack Closures

A full heap traversal is not currently possible because we don't decode STACK closures properly. They have
a sightly different structure to other closures but Ben has indicated that he knows what to do here.

## Fixing the pause behaviour

Currently there are two pause modes, one where the debugger pauses and one where the debuggee pauses.

* If the debugger initiates the pause then unpausing works correctly.
* If the debuggee initiates the pause then unpauses causes an assertion failure.

This second case needs to be fixed as initiating the pause from the debuggee is far more useful as you
have precise control over when exactly the pause happens.


# Future Goals

We welcome help with any of these future goals.

## Starter: Add a warning to stub events

The `ghc-debug` API implemented in GHC is only available with the threaded runtime.
Therefore, for the single-threaded runtime, stub implementations of all the methods
are provided. At the moment, they just don't do anything but it would be much better
to warn a user that they need to use the threaded runtime.

## Starter: Finish implementation of TSO closures in `ghc-heap`.

`TSO` closures are only half implemented. There is quite a bit more information in a `TSO`
which you might plausibly care about.

You can use this recent MR which added support for WEAK closures as a starting point.

## Starter: Check that file modtime matches DWARF information

When a file snippet is printed out it's important that the file is still the same one
as when the executable was compiled. The DWARF information inside the executable records
the modtime of the file so in the debugger we can check if the file has been modified
in the meantime. There is a similar check already implemented in gdb.

## Intermediate: Clean up the API

The current API to write debugging programs could be refined. In particular, the
`Debugger` is explicitly passed around everywhere rather than using a `Reader` like monad.

No particular thought has been given to what API endpoints are exposed so organising them and
writing documentation would be worthwhile.

## Intermediate: Implement closure timestamps

`ClosurePtr`s are only valid in the current pause window. Information about the current
pause window should be tracked

## Intermediate: Integrate `haxl`

It will be much more efficient if we can batch together non-dependent requests
to decode closures. However, it is much more convenient to provide an API which
performs an individual request. The `haxl` library is designed to solve this problem
so it would be good to implement support for it.

The `RequestClosures` request already supports requesting multiple closures at once so
all the work to implement this will be in the Haskell code.

## Intermediate: Improve `ghc-vis` visualisation

A very basic visualisation mode is currently implemented which starts a web server and draws a specific closure on
a HTML canvas. It should be possible to do much better than this. The details are up to you!
The heap is just a graph, but a large one so some standard visualisation techniques would be useful.

## Intermediate: Implement a testing framework

Testing the library is a little bit difficult because you always need two programs. One to debug and a debugger.
I'm not sure if you can nicely do this in a normal `Cabal` testing framework. It could be possible to compile simple
test programs using the GHC API.

## Advanced: Trigger pauses based on eventlog events

It's also possible to add a hook to redirect the eventlog. This could be used
to read eventlog events and dispatch on them. It's not clear to me how easy this would
be to implement or even what would need to be implemented where.

You might want to trigger a pause if the stack size reaches a certain amount or if
residency crosses a certain threshold.

## Advanced: Allow thunk closures to be evaluated

It's unclear exactly how to implement this but the basic idea is that
it should be possible to request that the RTS evaluates a THUNK closure.

Once the request is made, the RTS should be unpaused, the closure evaluated, repaused
and the resulting closure sent back to the debugger.

## Unspecified: TUI

It would be nice to implement a TUI which allowed for a more interactive exploration of
the heap. We have not deeply considered the design of this.

## Unspecified: Web GUI

It would be nice to implement a Web GUI which allowed for a more interactive exploration of
the heap. We have not deeply considered the design of this.
