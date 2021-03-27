module Test.Example where

import Data.DotLang (class GraphRepr, toGraph)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.GenericGraph (class Edges, genericEdges, genericToGraph)
import Graphics.Graphviz (Engine(..), renderToSvg)


data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)

derive instance treeGeneric :: Generic (Tree' a) _

instance treeEdges :: Edges a => Edges (Tree' a) where edges x = genericEdges x
instance treeDotRepr ::  Edges a => GraphRepr (Tree' a) where toGraph = genericToGraph

example :: String
example = renderToSvg Dot $ toGraph $
  Node' Leaf' 3 (Node' (Node' Leaf' 5 Leaf') 4 Leaf')
