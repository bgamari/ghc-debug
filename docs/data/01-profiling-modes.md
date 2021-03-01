# Profiling Modes

In the `GHC.Debug.Profiling` module there are several profiling modes which
can be used to get an overview of the heap structure of your application.
You can use these modes when you're first getting a handle on the memory
characteristics of your program.

## Closure Type Census

`censusClosureType` produces a heap census where each bucket corresponds to the
*closure type* of the object. The resulting profile gives you a simplified view
of the heap which can be inspected to get a feeling for what is taking up space
and where best to direct your efforts.

A simple program which performs a heap census is as follows:

```
p1 e = do
  pause e
  c <- run e $ do
          precacheBlocks
          roots <- gcRoots
          censusClosureType roots
  writeCensusByClosureType "profile.txt" c
```

The output of a heap census is a text file where each line corresponds to one
entry in the census. The lines are sorted by the total size of each bucket.

```
key, total, count, max, avg
FUN:204785592:4573257:120:44.77893807411217
THUNK_2_0:158541184:4954412:32:32.0
ghc-prim:GHC.Types:::144895080:6037295:24:24.0
THUNK_1_0:117966120:4915255:24:24.0
THUNK:60638608:1369396:72:44.281280214050575
text-1.2.4.0:Data.Text.Internal:Text:54494080:1702940:32:32.0
FUN_1_0:49647184:3102949:16:16.0
ARR_WORDS:31934760:586457:272:54.45371101376571
FUN_2_0:31811472:1325478:24:24.0
ghc-prim:GHC.Tuple:(,):30369504:1265396:24:24.0
```

For example, this census tells us that the largest source of allocations in this
program is `FUN` closures, which are functions. The second column is the total
size of the closures, the third column the total number of closures, the fourth
column the maximum size of any such closure and the final column the average size.

For most closure types all the closures of that type are the same size, for example
`THUNK_1_0` closures are a special version of a `THUNK` closure which always
have size 24. As can be seen in the profile.

This detailed output is usually not too big but can also be used to inspect much
smaller bands, for example you might want to check some invariant that certain
data types are only resident a small number of times.

It can also be useful to perform a census but not starting from the GC roots,
this will give you information about how much certain objects retain or allow you
to focus your attention on a certain portion of the heap.

## 2-Level Closure Type Census

Another pre-canned census type is the 2-level closure type census (`census2LevelClosureType`). This
census type is more detailed than a normal closure type census because each
bucket is a combination of the closure type of a closure and the closure type
of it's pointer arguments. For example, this profiling mode allows you to distinguish
lists of different types.

```
p2 e = do
  pause e
  c <- run e $ do
          precacheBlocks
          roots <- gcRoots
          census2LevelClosureType roots
  writeCensusByClosureType "profile.txt" c
```

The example output is similar to the output for the normal census. For example
this is a 2 level census from GHC compiling Cabal.

```
key, total, count, max, avg
ghc-prim:GHC.Types::[ghc:GHC.Core.TyCo.Rep:TyConApp,ghc-prim:GHC.Types::]:67063104:2794296:24:24.0
ghc-prim:GHC.Types::[ghc:GHC.Core.TyCo.Rep:TyConApp,ghc-prim:GHC.Types:[]]:46401624:1933401:24:24.0
ghc:GHC.Core.TyCo.Rep:TyConApp[ghc:GHC.Core.TyCon:AlgTyCon,ghc-prim:GHC.Types::]:35328504:1472021:24:24.0
containers-0.6.4.1:Data.IntMap.Internal:Bin[containers-0.6.4.1:Data.IntMap.Internal:Bin,containers-0.6.4.1:Data.IntMap.Internal:Bin]:34713640:867841:40:40.0
ARR_WORDS[]:27880928:136664:2154128:204.01077094187204
ghc:GHC.Iface.Type:IfaceTyCon[ghc:GHC.Types.Name:Name,ghc:GHC.Iface.Type:IfaceTyConInfo]:25211784:1050491:24:24.0
ghc:GHC.Core.TyCo.Rep:TyConApp[ghc:GHC.Core.TyCon:PromotedDataCon,ghc-prim:GHC.Types:[]]:22608408:942017:24:24.0
ghc:GHC.Iface.Type:IfaceTyConApp[ghc:GHC.Iface.Type:IfaceTyCon,ghc:GHC.Iface.Type:IA_Nil]:17181432:715893:24:24.0
ghc:GHC.Iface.Type:IfaceTyConInfo[ghc:GHC.Types.Basic:NotPromoted,ghc:GHC.Iface.Type:IfaceNormalTyCon]:16766592:698608:24:24.0
```

Compared the the previous profile you can now see that the majority of list allocations
are lists which contain types, and specifically `TyConApp` constructors. The top
band correponds to `:` constructors where the first argument is a `TyConApp` constructor
and the second argument another `:`. These account for 67mb of all heap allocation.

Looking further down you can also see that `TyConApp` appears twice again, this time
firstly with the first argument being an `AlgTyCon` but more interestingly secondly with
`PromotedDataCon` and and argument of `[]`. This second band probably has a high potential
for sharing as it's unlikely there are 942 000 different nullary promoted type constructors.
In a later chapter of this brilliant documentation, we'll look a bit more deeply into
the issue of `TyConApp`s.

You have to be careful when using a more detailed profiling mode because
the size of each band becomes quite small. Therefore concentrating on eliminating
specific bands will not often yield very good results. It is necessary to think about
the larger structure of the program in order to get the best results.

## Writing your own census

Writing your own census is also remarkably easy using the `closureCensusBy`
helper function. The supplied classification function is applied once to each
closure on the heap and the result returned.

```
closureCensusBy :: forall k v . (Semigroup v, Ord k)
                => (ClosurePtr -> SizedClosure -> DebugM (Maybe (k, v)))
                -> [ClosurePtr] -> DebugM (Map.Map k v)
```

For example, if you want to perform a census of the allocation locations of
all the `TyConApp` constructors in your application, you can implement this
using `closureCensusBy`.

```
tyConAppLoc :: ClosurePtr -> SizedClosure -> DebugM (Maybe (SourceInformation, Count))
tyConAppLoc cp sc = do
  case noSize sc of
    ConstrClosure info ps ds cd -> do
      cd' <- dereferenceConDesc cd
      case cd' of
        ConstrDesc _ _ "TyConApp" -> do
          info <- getSourceInfo (tableId info)
          return ((, Count 1) <$> info)
        _ -> return Nothing
    _ -> return Nothing
```

The `tyConAppLoc` classification function checks to see if a specific closure
is a `TyConApp` constructor, if it is then it queries the source location the
constructor originated from and adds it to the census. The result is a map from
source locations to the number of allocations which arose from that location.
This kind of analysis can be useful to debug sharing problems.


## Comparing Heap Profiles

It can be very useful to take the difference between two heap profiles if you
are investigating how the heap has changed between two parts of your program.
`censusClosureType` just returns a normal map so can be manipulated easily.
Taking the difference amounts to first performing a census on the before and after
snapshots and then taking the difference between the two. The census
can then be printed using `printCensusByClosureType`.

```
p44e before after = do
  before_census <- runTrace before $ do
                      precacheBlocks
                      gcRoots >>= censusClosureType
  after_census  <- runTrace after $ do
                      precacheBlocks
                      gcRoots >>= censusClosureType
  let diff_census = Map.differenceWith (\cs1 cs2 -> Just (CS (cscount cs1 - cscount cs2) (cssize cs1 - cssize cs2) (Max 0))) after_census before_census
  writeCensusByClosureType "profile.txt" diff_census
```

In the output it's interesting to see the big differences between bands but also
sometimes the very small differences (for example, if a cache is leaking then between
iterations it may only increase by 1).


## Rendering a profile

There is also some support for rendering a profile using `eventlog2html`,
see the `profile` function for an example of how to do this. Bare in mind that
profiling using ghc-debug is much slower than the built-in heap profiling modes.


