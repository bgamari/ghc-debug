# Development Notes

## Debugger <-> Debuggee Communication

The debugger and debuggee communicate via a unix domain socket created by the
debuggee via `start` in the `ghc-debug-stub` package. Such sockets appear as
files in the file system. By convention, these sockets are placed with arbitrary
name form "<PID> - <Title>" into the `$XDG_DATA_HOME/ghc-debug/debuggee`
directory. This directory is observed by the frontend to automatically discover
debuggees.The location of the socket can be overridden by setting the
`GHC_DEBUG_SOCKET` environment variable when running the debuggee. In summary
the socket location is:

* `$GHC_DEBUG_SOCKET` if `GHC_DEBUG_SOCKET` is defined. Else
* `$XDG_DATA_HOME/ghc-debug/debuggee/<PID>-<Title>` if `XDG_DATA_HOME` is defined. Else
* `/tmp/ghc-debug/debuggee/<PID> - <Title>`