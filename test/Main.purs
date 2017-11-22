module Test.Main where

import Prelude


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.DotLang (class GraphRepr, toGraph, toText)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.GenericGraph (class Edges, genericEdges, genericToDot)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

--Simple
data Simple = A | B

derive instance genericSimple :: Generic Simple _
instance simpleToDot :: GraphRepr Simple where
  toGraph = genericToDot
instance simpleEdges :: Edges Simple where
  edges =  genericEdges

--Rec
data Rec = Leaf | Node Rec

derive instance recGeneric :: Generic Rec _
instance recToDot :: GraphRepr Rec where
  toGraph = genericToDot

instance recEdges :: Edges Rec where
  edges x = genericEdges x


--List
data List' a = Nil | Cons' a (List' a)

derive instance listGeneric :: Generic (List' a) _

instance listEdges :: Edges a => Edges (List' a) where
  edges x = genericEdges x

fromArray :: ∀a. Array a -> List' a
fromArray = foldr Cons' Nil

--Tree
data Tree' a = Leaf' | Node' a (Array (Tree' a))

derive instance treeGeneric :: Generic (Tree' a) _

instance treeEdges :: Edges a => Edges (Tree' a) where
  edges x = genericEdges x

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
instance graphReprTodo :: GraphRepr Todo where toGraph = genericToDot
instance egdesTodo :: Edges Todo where edges x = genericEdges x

main :: Eff
  ( console :: CONSOLE
  , avar :: AVAR
  , testOutput :: TESTOUTPUT
  )
  Unit
main = do
  log $ toText $ genericToDot $ fromArray [1, 2, 3, 4, 7]
  -- log $ renderToJson Dot $ genericToDot $ fromArray [1, 2]
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
      equal "digraph {\nroot [style=invis]; 0 [label=\"A\"]; root -> 0;\n }" (toText $ toGraph A)
    test "recursive" do
      equal
        "digraph {\nroot [style=invis]; 0 [label=\"Node\"]; 1 [label=\"Node\"]; 2 [label=\"Leaf\"]; root -> 0;\n 0 -> 1;\n 1 -> 2;\n }"
        (toText $ toGraph $ Node (Node Leaf))
    test "list" do
      equal
        "digraph {\nroot [style=invis]; 0 [label=\"Cons'\"]; 4 [label=\"1\"]; 1 [label=\"Cons'\"]; 3 [label=\"2\"]; 2 [label=\"Nil\"]; root -> 0;\n 0 -> 4;\n 0 -> 1;\n 1 -> 3;\n 1 -> 2;\n }"
        (toText $ genericToDot $ Cons' 1 (Cons' 2 Nil))
