module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.DotLang (class GraphRepr, toGraph, toText)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.GenericGraph (class Edges, genericEdges, genericToGraph)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Example (example)

--Simple
data Simple = A | B

derive instance genericSimple :: Generic Simple _
instance simpleToDot :: GraphRepr Simple where
  toGraph = genericToGraph
instance simpleEdges :: Edges Simple where
  edges =  genericEdges

--Rec
data Rec = Leaf | Node Rec

derive instance recGeneric :: Generic Rec _

instance recGraphRepr :: GraphRepr Rec where toGraph = genericToGraph
instance recEdges :: Edges Rec where edges x = genericEdges x


--List
data List' a = Nil | Cons' a (List' a)

derive instance listGeneric :: Generic (List' a) _

instance listEdges :: Edges a => Edges (List' a) where edges x = genericEdges x
instance listGraphRepr ∷ Edges a => GraphRepr (List' a) where toGraph  = genericToGraph

fromArray :: ∀a. Array a -> List' a
fromArray = foldr Cons' Nil

--Tree
data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)

derive instance treeGeneric :: Generic (Tree' a) _

instance treeEdges :: Edges a => Edges (Tree' a) where edges x = genericEdges x
instance treeDotRepr ::  Edges a => GraphRepr (Tree' a) where toGraph = genericToGraph

-- Todo
newtype Todo = Todo
  { id :: Int
  , text :: String
  , newText :: String
  , completed :: Boolean
  , editing :: Boolean
  }

derive instance genericTodo :: Generic Todo _
instance showTodo :: Show Todo where show = genericShow
instance graphReprTodo :: GraphRepr Todo where toGraph = genericToGraph
instance egdesTodo :: Edges Todo where edges x = genericEdges x

main :: Eff
  ( console :: CONSOLE
  , avar :: AVAR
  , testOutput :: TESTOUTPUT
  )
  Unit
main = do
  -- log $ toText $ genericToGraph $ fromArray [1, 2, 3, 4, 7]
  -- log $ toText $ toGraph $ (Node' Leaf' 3 (Node' (Node' Leaf' 5 Leaf') 4 Leaf'))
  log $ example
  main'


main' ::
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    )
    Unit
main' = runTest do
  suite "GenericGraph" do
    test "simple" do
      equal "digraph {root [style=invis]; 0 [label=\"A\"]; root -> 0; }" (toText $ toGraph A)
    test "recursive" do
      equal
        "digraph {root [style=invis]; 0 [label=\"Node\"]; 1 [label=\"Node\"]; 2 [label=\"Leaf\"]; root -> 0; 0 -> 1; 1 -> 2; }"
        (toText $ toGraph $ Node (Node Leaf))
    test "list" do
      equal
        "digraph {root [style=invis]; 0 [label=\"Cons'\"]; 4 [label=\"1\"]; 1 [label=\"Cons'\"]; 3 [label=\"2\"]; 2 [label=\"Nil\"]; root -> 0; 0 -> 4; 0 -> 1; 1 -> 3; 1 -> 2; }"
        (toText $ toGraph $ Cons' 1 (Cons' 2 Nil))
    test "tree" do
      equal
        "digraph {root [style=invis]; 0 [label=\"Node'\"]; 9 [label=\"Leaf'\"]; 8 [label=\"3\"]; 1 [label=\"Node'\"]; 4 [label=\"Node'\"]; 7 [label=\"Leaf'\"]; 6 [label=\"5\"]; 5 [label=\"Leaf'\"]; 3 [label=\"4\"]; 2 [label=\"Leaf'\"]; root -> 0; 0 -> 9; 0 -> 8; 0 -> 1; 1 -> 4; 4 -> 7; 4 -> 6; 4 -> 5; 1 -> 3; 1 -> 2; }"
        (toText $ toGraph $ Node' Leaf' 3 (Node' (Node' Leaf' 5 Leaf') 4 Leaf'))
