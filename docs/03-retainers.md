# Finding Retainer Paths

One of the most useful queries which can be answered by ghc-debug is asking
"Why is this specific object being retained". For example, if you look at a heap
profile and see a larger than expected entry for a specific band, then the next
step might be to sample some of those closures and look why they are being retained.
Functions for dealing with retainers live in `GHC.Debug.Retainers`.

## Finding Retainer Paths

The first part of the analysis is to find retainer paths to a specific object.
The `findRetainers` function provides the most general interface.

```
findRetainers :: Maybe Int
              -> [ClosurePtr]
              -> (ClosurePtr -> SizedClosure -> DebugM Bool)
              -> DebugM [[ClosurePtr]]
```

The first argument to `findRetainers` is an optional limit, which will stop the
search after finding this many closures. When you are looking for an object with
a large number of occurences then providing a limit is sensible because otherwise
you will run out of memory.

The second argument is the roots to start the search from. Normally this is `gcRoots`.

The third argument is a classification function which decides whether the currently
inspected closure is one we are interested in knowing the path to.

The function returns a list of retainer paths, a retainer path is a path through the heap from one
of the given roots to an object which was classified by the classification function.

For example, we can use `findRetainers` to find retainer paths for 100 `TyConApp`
constructors.

```
tyConApp rroots = findRetainers (Just 100) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ ps _ cd -> do
          ConstrDesc _ _  cname <- dereferenceConDesc cd
          return $ cname == "TyConApp"
        _ -> return $ False
```


In this particular snapshot there are over 100 000 TyConApp objects, but by sampling
a small number you can still manually inspect the output and work out what's
going on.

In the output, only one path to each distinct object will be returned, there could
be many paths to reach an object so you have to be a bit careful when interpreting
the results.

NOTE: The notion of a retainer is *not* the same as on GHC's heap, for example,
a weak pointer will count as a retainer for an object. Sometimes you want to know this.

## Visualising a Retainer Path

The returned result of `findRetainersOf` is not very useful on its own.
`addLocationToStack` is a useful function which decorates a retainer stack with
additional location information.

```
addLocationToStack :: [ClosurePtr] -> DebugM [(SizedClosureC, Maybe SourceInformation)]
```

Then once the locations have been added to the stack, the normal way I render
the stacks is using `displayRetainerStack` which then allows you to use a `less`
based interface for inspecting the output. The argument to `displayRetainerStack`
is the output of `addLocationStack` but with an additional label for each path.

```
displayRetainerStack :: [(String, [(SizedClosureC, Maybe SourceInformation)])] -> IO ()
```

A complete analysis program combines together these three functions:

```
p41a e = do
  pause e
  stacks <- runTrace e $ do
    precacheBlocks
    roots <- gcRoots
    rs <- tyConApp roots
    traverse (\c -> (show (head c),) <$> (addLocationToStack c)) rs
  displayRetainerStack stacks
```

### An example retainer path

One path from running the above program might look like the following stack.
The top of the stack is the object we care about. The bottom of the stack is
one of the roots. On each line first appears the pretty printed representation of
the closure at that part of the stack, then a source position if there is one
for that object.

```
"0x4225a44120"
TyConApp 0x42884b6260 0x4225a44a68 <:GHC.Core.TyCo.Rep:compiler/GHC/Core/TyCo/Rep.hs:1029:20-22>
Id 0x4241352d58 0x4225a44120 0x7fa762bf06c0 0x7fa764861480 0x7fa76478eeb0 0x420ea9f9e8 6341068275337658638 <:GHC.Core.Opt.Simplify:compiler/GHC/Core/Opt/Simplify.hs:(781,9)-(793,67)>
Tip 0x420ea9fa38 6341068275337658638 <:Data.IntMap.Internal:libraries/containers/containers/src/Data/IntMap/Internal.hs:838:27>
Bin 0x422606d488 0x420eaa0328 6341068275337658368 256 <:Data.IntMap.Internal:libraries/containers/containers/src/Data/IntMap/Internal.hs:835:21-44>
Bin 0x422606d4a0 0x422607ea10 4611686018427387904 2305843009213693952 <:Data.IntMap.Internal:libraries/containers/containers/src/Data/IntMap/Internal.hs:836:21-44>
Bin 0x4283d20530 0x422607ea38 0 4611686018427387904 <:Data.IntMap.Internal:libraries/containers/containers/src/Data/IntMap/Internal.hs:836:21-44>
_bh 0x422607ea60 <nl>
SimplEnv 0x42130cabd0 0x7fa7505d5ae8 0x7fa7505d5ae8 0x422607d550 0x422607d530 <:GHC.Core.Opt.Simplify.Env:compiler/GHC/Core/Opt/Simplify/Env.hs:(806,1)-(829,41)>
_thunk(  ) 0x422607d578 0x422607e4b0 <Unfolding:GHC.Core.Opt.Simplify:compiler/GHC/Core/Opt/Simplify.hs:3004:14-48>
IdInfo 0x7fa764790090 0x422607e6b0 0x7fa764713410 0x7fa76470e360 0x7fa764744068 0x7fa764741618 0x7fa764744930 0x7fa74f112478 0 <:GHC.Core.Opt.Simplify:compiler/GHC/Core/Opt/Simplify.hs:3005:54-64>
Id 0x4234a4e600 0x4234a4e5a8 0x7fa762bf06c0 0x7fa764861480 0x7fa76478eeb0 0x422607e850 6341068275337658369 <:GHC.Core.Opt.Simplify:compiler/GHC/Core/Opt/Simplify.hs:3005:44-52>
Tip 0x422607e8a0 6341068275337658369 <:Data.IntMap.Internal:libraries/containers/containers/src/Data/IntMap/Internal.hs:838:27>
Bin 0x422607ea88 0x42647b2700 6341068275337658368 256 <:Data.IntMap.Internal:libraries/containers/containers/src/Data/IntMap/Internal.hs:836:21-44>
Bin 0x42647b2728 0x4283091b58 4611686018427387904 2305843009213693952 <:Data.IntMap.Internal:libraries/containers/containers/src/Data/IntMap/Internal.hs:836:21-44>
Bin 0x4283d20530 0x4283091b80 0 4611686018427387904 <:Data.IntMap.Internal:libraries/containers/containers/src/Data/IntMap/Internal.hs:836:21-44>
_bh 0x4283091ba8 <nl>
_thunk(  ) 0x42830912e8 0x4283091368 <InScopeSet:GHC.Types.Var.Env:compiler/GHC/Types/Var/Env.hs:(318,6)-(320,55)>
Stack( 4093 ) <nl>
TSO <nl>
TSO <nl>
```

What you can learn from a stack, depends on the stack and also your own domain knowledge of a program.
This above stack explains that a THUNK which has type `InScopeSet` retains an `IntMap` which
contains `Id`s and in one of those `Id`s, there is an `IdInfo` field which has thunk of
type `Unfolding` wehich retains a ... and so on. This information can be verbose but very useful.
You need to look at the source positions and program in order to understand what is going on and whether it is good or bad. Randomly forcing thunks is likely to get you nowhere. We didn't write ghc-debug
to get people to randomly insert ! patterns - you can now be precise.
