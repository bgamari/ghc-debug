# Snapshots vs Connecting

There are two modes which ghc-debug can be used. The first mode connects to a
running process over a socket and then queries information from the heap of the
process over the socket.
The second mode uses a saved snapshot, created after connecting to the process
using the first mode, which allows analysis to be completed without connecting
to the process.

The same analysis programs can be used in both modes, if you are using snapshot
mode then write requests such as pausing and resuming are just ignored.

There are two advantages of taking a snapshot:

1. Your analysis is reproducible across separate runs.
2. Performance can be much faster.

The recommended way to use ghc-debug is to take a snapshot by connecting to
the process and then performing further analysis on the snapshot.

## Taking Snapshots

Functions to do with snapshotting can be found in `GHC.Debug.Snapshot`.
The easiest way to take a snapshot is to use the precanned `makeSnapshot` function.

```haskell
main = withDebuggeeConnect "/tmp/ghc-debug" (\e -> makeSnapshot e "/tmp/ghc-debug-cache)
```

When this program runs, it will connect to the `/tmp/ghc-debug` socket and save
the snapshot to `/tmp/ghc-debug-cache`. Simple.

## Using Snapshots

A `Debuggee` can be created from a snapshot by using the `snapshotRun` function.

```haskell
main = snapshotRun "/tmp/ghc-debug-cache" p41c
```

The `/tmp/ghc-debug-cache` snapshot which we just saved will be loaded and
the `p41c` program will be executed on the snapshot.

## Size of Snapshots

Snapshots are quite large but only a small order of magnitude larger than the
approximate memory footprint of the program. The size is bloated a bit at the moment
as even unreachable blocks are included in the snapshot.
In future the size of snapshots might be optimised to only include reachable blocks.

