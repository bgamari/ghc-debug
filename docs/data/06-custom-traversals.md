# Custom Traversals

You can get a lot of mileage out of the library without having to resort to writing
your own traversals. This section is a bit more advanced for anyone who wants to
get their hands dirty.

There are two built-in traversal functions, one which traverses sequentially
and one in parallel.

Using one of these two traversal modes is a good idea because it's easy to
write an traversal which is not tail-recursive and will use all your memory.

## Sequential Traversal

The interface to the sequential traversal allows you to specify continuations
to apply to each of the four different pointer types which are present in the AST.

* Closure Pointers
* Stack Pointers
* PAP Pointers
* Constr Description Pointers

Most of the time you just care about closure pointers but the interface is general
enough to deal with these other cases as well. The `GHC.Debug.Trace` module provides
the generic traversal function called `traceFromM`:

```
traceFromM :: C m => TraceFunctions m -> [ClosurePtr] -> m DebugM ()

data TraceFunctions m =
      TraceFunctions { papTrace :: !(GenPapPayload ClosurePtr -> m DebugM ())
      , stackTrace :: !(GenStackFrames ClosurePtr -> m DebugM ())
      , closTrace :: !(ClosurePtr -> SizedClosure -> m DebugM () -> m DebugM ())
      , visitedVal :: !(ClosurePtr -> m DebugM ())
      , conDescTrace :: !(ConstrDesc -> m DebugM ())
      }
```

The 4 continuations `papTrace`, `conDescTrace`, `stackTrace` and `closTrace` are
applied the first time the relevant object is visited. The `closTrace` has an
additional continuation which must be called to carry on the traversal.
The `visitedVal` function is applied when we have already visited a closure.

For example, `census2LevelClosureType` is implemented using `traceFromM`.
Constant functions are supplied for all the functions apart from the `closTrace` function
which instead performs the dereferencing work for the analysis. The whole analysis
runs in the `StateT` monad.

```
census2LevelClosureType :: [ClosurePtr] -> DebugM CensusByClosureType
census2LevelClosureType cps = snd <$> runStateT (traceFromM funcs cps) Map.empty
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }
    -- Add cos
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (StateT CensusByClosureType DebugM) ()
               -> (StateT CensusByClosureType DebugM) ()
    closAccum _ s k = do
      s' <- lift $ quadtraverse dereferencePapPayload dereferenceConDesc dereferenceStack pure s
      pts <- lift $ mapM dereferenceClosure (allClosures (noSize s'))
      pts' <- lift $ mapM (quadtraverse pure dereferenceConDesc pure pure) pts


      modify' (go s' pts')
      k

    go d args =
      let k = closureToKey (noSize d)
          kargs = map (closureToKey . noSize) args
          final_k :: Text
          final_k = k <> "[" <> T.intercalate "," kargs <> "]"
      in Map.insertWith (<>) final_k (mkCS (dcSize d))

```

## Parallel Traversal

The parallel traversal has a slightly different interface to the sequential traversal
to account for the necessary communication and combination of the results of
running actions on different threads. The `traceParFromM` function is from `GHC.Debug.ParTrace`.

```
traceParFromM :: Monoid s => TraceFunctionsIO a s -> [ClosurePtrWithInfo a] -> DebugM s

data TraceFunctionsIO a s =
      TraceFunctionsIO { papTrace :: !(GenPapPayload ClosurePtr -> DebugM ())
      , stackTrace :: !(GenStackFrames ClosurePtr -> DebugM ())
      , closTrace :: !(ClosurePtr -> SizedClosure -> a -> DebugM (a, s, DebugM () -> DebugM ()))
      , visitedVal :: !(ClosurePtr -> a -> DebugM s)
      , conDescTrace :: !(ConstrDesc -> DebugM ())
      }
```

`TraceFunctionsIO a s` communicates values of type `a` between threads and produces
a result of type `s`.
Like the sequential traversal there is one continuation for each pointer type
and one for already visited values. This time the type of `closTrace` is quite
different. This time in addition to the current pointer and decoded closure, a
value of type `a` is passed to the interpretation function. The function computes
a new value of `a` to communicate to its children, a state value `s` to update the
thread-local result state with and a continuation about whether to carry on the traversal.

The `a` value communicates information about a closures parents in the `findRetainers`
function.


The performance of the parallel traversal depends on the contention placed on the
common socket. For analysis modes which require many expensive `DebugM` actions, it
may be faster to use the sequential traversal. A workload which primarily consults the
block cache

As an example, the `parCount` function is implemented using the parallel traversal:

```
parCount :: [ClosurePtr] -> DebugM CensusStats
parCount = traceParFromM funcs . map (ClosurePtrWithInfo ())
  where
    nop = const (return ())
    funcs = TraceFunctionsIO nop nop clos (const (const (return mempty))) nop

    clos :: ClosurePtr -> SizedClosure -> ()
              -> DebugM ((), CensusStats, DebugM () -> DebugM ())
    clos _cp sc _ = do
      return ((), mkCS (dcSize sc), id)
```

I think there is still performance improvements to be made. In future I want to look
at the performance using threadscope. It seems that in the case in snapshot mode where all requests can
be answered using the cache, for example, `parCount`, there should be almost full core utilisation.
