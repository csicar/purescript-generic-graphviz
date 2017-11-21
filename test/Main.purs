module Test.Main where

import Prelude

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.DotLang (class DotR, Graph(..), toDot, toText)
import Data.Generic.Rep (class Generic, from)
import Data.GenericGraph (class Edges, genericEdges, genericToDot)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


--Simple
data Simple = A | B

derive instance genericSimple :: Generic Simple _
instance simpleToDot :: DotR Simple where
  toDot = genericToDot
instance simpleEdges :: Edges Simple where
  edges =  genericEdges

--Rec
data Rec = Leaf | Node Rec

derive instance recGeneric :: Generic Rec _
instance recToDot :: DotR Rec where
  toDot = genericToDot

instance recEdges :: Edges Rec where
  edges x = genericEdges x


--List
data List' a = Nil | Cons' Int (List' a)

derive instance listGeneric :: Generic (List' a) _
instance listToDot :: DotR (List' a) where
  toDot = genericToDot

instance listEdges :: Edges (List' a) where
  edges x = genericEdges x

main = do
  log $ toText $ toDot $ Cons' 1 (Cons' 2 Nil)
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
      equal "digraph {\nroot [style=invis]; 0 [label=\"A\"]; root -> 0;\n }" (toText $ toDot A)
    test "recursive" do
      equal
        "digraph {\nroot [style=invis]; 0 [label=\"Node\"]; 1 [label=\"Node\"]; 2 [label=\"Leaf\"]; root -> 0;\n 0 -> 1;\n 1 -> 2;\n }"
        (toText $ toDot $ Node (Node Leaf))
    test "list" do
      equal
        "digraph {\nroot [style=invis]; 0 [label=\"Cons'\"]; 4 [label=\"1\"]; 1 [label=\"Cons'\"]; 3 [label=\"2\"]; 2 [label=\"Nil\"]; root -> 0;\n 0 -> 4;\n 0 -> 1;\n 1 -> 3;\n 1 -> 2;\n }"
        (toText $ toDot $ Cons' 1 (Cons' 2 Nil))
