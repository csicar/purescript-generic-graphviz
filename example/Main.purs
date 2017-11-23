module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (foldr)
import Data.DotLang (class GraphRepr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.GenericGraph (class Edges, genericEdges, genericToGraph)
import Graphics.Graphviz (Engine(..), renderReprSvg, renderToSvg)
import Prelude (class Show, Unit, ($))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

data Test = A | B | R Test Test Test

derive instance genericTest :: Generic Test _

instance showTest :: Show Test where
  show x = genericShow x

instance edgesTest :: Edges Test where
  edges x = genericEdges x

instance graphReprTest :: GraphRepr Test where
  toGraph = genericToGraph

data Simple = S | E String | F String Simple


derive instance genericSimple :: Generic Simple _

instance egdeSimple :: Edges Simple where
  edges x = genericEdges x

instance graphReprSimple :: GraphRepr Simple where
  toGraph = genericToGraph


data List' = Nil | Cons Int (List')

fromArray :: Array Int -> List'
fromArray = foldr Cons Nil

derive instance genericList' :: Generic (List') _

instance edgeList :: Edges Int => Edges (List') where
  edges = genericEdges

newtype User = User {name :: String, age :: Int, friends :: Test}

derive instance genericUser :: Generic User _

instance egdeUser :: Edges User where
  edges = genericEdges

instance graphReprUser :: GraphRepr User where
  toGraph = genericToGraph

generateSvg :: ∀a. GraphRepr a => a -> String
generateSvg e = renderReprSvg Dot e

ex1 :: String
ex1 = generateSvg (R (R B A A) B B)

-- Infinite Recursion
-- ex2 :: String
-- ex2 = generateSvg Nil

ex3 :: String
ex3 = generateSvg $ User {name: "Test", age: 2, friends: R A A B}

ex4 :: String
ex4 = renderToSvg Dot $ genericToGraph  [A, R A B A]
