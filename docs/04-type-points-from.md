# Type Points From

The type points from analysis creates a graph where the nodes are info table addresses
and there is an edge between nodes if there's a pointer from one node to the other.
The TPF graph can be seen as a quotiented version of the original heap graph.
This makes the size of graph tractable to deal with unlike the original heap
graph which is usually too large to represent fully in memory.

The type points from graph is used in the [Cork](https://www.cs.utexas.edu/users/speedway/DaCapo/papers/cork-popl-2007.pdf) leak detection analysis. A more detailed description of
the graph structure and the `detectLeaks` analysis can be read in the paper.

## Making the Graph

The graph is created using the `typePointsFrom` function, which takes a set
of roots as the location to start from. The output is a `TypePointsFrom` graph,
the nodes of the graph can be retrieved using `getNodes` and the edges by `getNodes`.

```
p45 e = do
  pause e
  t <- runTrace e $ do
    _ <- precacheBlocks
    rs <- gcRoots
    typePointsFrom rs
  let es = (reverse $ sortBy (comparing (cssize . snd)) (Map.assocs (getEdges t)))
  es' <- runTrace e $ mapM (\(Edge e1 e2, c) -> do
                                e' <- (,) <$> getKey e1 <*> getKey e2
                                return (e', c)) es
  mapM_ print es'
```

This program just gets the edges and prints them out so you can use a `grep`
based interface to look at specific edges you care about.

## Understanding the Output

The display modes for the TPF graph are not very refined yet. I just print out
all the edges and then use a `less` based TUI in order to understand the output.

Using this interface we can have a closer look at what is retaining `TyConApp` nodes than
any census method allowed us so far. By filtering the output for edges which point
from a TyConApp, you can find the edge which points from `TyConApp` to :.

```
(("CONSTR_2_0:TyConApp_GHC.Core.TyCo.Rep_12_con_info::compiler/GHC/Core/TyCo/Rep.hs:1029:20-22","CONSTR_2_0::_GHC.Base_2_con_info::libraries/base/GHC/Base.hs:1246:16-29"),CS {cscount = Count 3335397, cssize = Size {getSize = 80049528}, csmax = Max {getMax = Size {getSize = 24}}})
```

From the source information you can see these `:` applications come from the
`map` function, which is not so useful. Let's see why these `:` are being retained.

```
(("CONSTR_2_0::_GHC.Base_2_con_info::libraries/base/GHC/Base.hs:1246:16-29","CONSTR_2_0::_GHC.Base_2_con_info::libraries/base/GHC/Base.hs:1246:16-29"),CS {cscount = Count 3406303, cssize = Size {getSize = 81751272}, csmax = Max {getMax = Size {getSize = 24}}})
(("CONSTR_2_0::_GHC.Base_2_con_info::libraries/base/GHC/Base.hs:1246:16-29","CONSTR_2_0:TyConApp_GHC.Core.TyCo.Rep_12_con_info::compiler/GHC/Core/TyCo/Rep.hs:1029:20-22"),CS {cscount = Count 1966843, cssize = Size {getSize = 47204232}, csmax = Max {getMax = Size {getSize = 24}}})
(("CONSTR_2_0::_GHC.Base_2_con_info::libraries/base/GHC/Base.hs:1246:16-29","CONSTR:IfaceCase_GHC.CoreToIface_0_con_info::compiler/GHC/CoreToIface.hs:548:31-88"),CS {cscount = Count 18957, cssize = Size {getSize = 454968}, csmax = Max {getMax = Size {getSize = 24}}})
```

The top two entries dominate, which correpond to pointing to its own tail but interestingly
pointing back to a `TyConApp` constructor. So we end up with a lot of nested `TyConApp`
calls and so on.. The analysis could continue further but you sometimes need to go a long
way up the tree to find out why something is being retained. A better way to
process the graph rather than looking at the text dump would be useful.


In future it would be good to generalise this analysis so it could create different
kinds of graphs in a similar way to the normal census mode.

## (Experimental) Automatically Detecting Leaks

There is also an experimental implementation of the leak detection
algorithm described in the Cork paper. This needs more testing to be generally
usable.

The analysis works by pausing the program in a given interval and comparing the difference
between TPF graphs, in order to try to find the parts of the graph which are
getting bigger over time. The output is rendered as a dot graph.

```
detectLeaks :: Int  -- Sample interval in seconds.
            -> Debuggee
            -> IO ()
```

If you want to use this then it's best to read the source code, as you will
probably need to modify it to make it useful for you.


