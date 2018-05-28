Generic Graphviz
====

documentation can be found on [pursuit](https://pursuit.purescript.org/packages/purescript-generic-graphviz/)

consists of 3 parts (which probably *now are* seperate modules..):

1. Dot-Lang typed representation and Dot-Lang code-generator. [here](https://github.com/csicar/purescript-dotlang)
2. Generic Graph generator (takes an arbituary ADT and generates a graph for it) (this repository)
3. Bindings for Graphviz [here](https://github.com/csicar/purescript-graphviz)

Example
---

```purescript
-- your data type
data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)

-- derive generic
derive instance treeGeneric :: Generic (Tree' a) _

-- create instances for the needed type classes
instance treeEdges :: Edges a => Edges (Tree' a) where
  edges x = genericEdges x
instance treeDotRepr ::  Edges a => GraphRepr (Tree' a) where
  toGraph = genericToGraph

example :: String
example = renderToSvg Dot $ toGraph $
  Node' Leaf' 3 (Node' (Node' Leaf' 5 Leaf') 4 Leaf')
-- example = "...<svg><g>...</g>...</svg>..."
```
`example` will be:


![screenshot](screenshot.png)

see [full example](./test/Example.purs) for imports

see [todo mvc example](https://github.com/csicar/generic-graphviz-todomvc) for a larger example

Features
--------

- generic graph generation
- after `npm install`: usable from nodejs backend
- graphviz-images can be rendered to Chrome's WebDev Console (using console.log)

TODOs
-----

- Refactor into multiple libraries
- support entire DOT language in data model
- allow custom edges in GenericGraph
- add effects to console.image
- move viz.js call into a WebWorker

Testing
-------

run tests:
```bash
$ pulp test
```

