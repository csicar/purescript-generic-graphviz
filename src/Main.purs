module Main where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (fst)
import Dot (Graph, Node(..), graphFromEdges, toText)
import GenericGraph (class Edges, Tree(..), edges, eliminateNothings, extractEdges, extractNodes, genericEdges, uniqueNodes)
import Prelude (class Show, Unit, id, ($))
import Vizjs (viz_internal)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array ((!!), foldr)
import Data.Maybe (fromMaybe)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

data Test = A | B | R Test Test Test

derive instance genericTest :: Generic Test _

instance showTest :: Show Test where
  show x = genericShow x

instance edgesTest :: Edges Test where
  edges x = genericEdges x

data Simple = S | E String | F String Simple


derive instance genericSimple :: Generic Simple _

instance genericEdge :: Edges Simple where
  edges x = genericEdges x

defaultNode :: Node
defaultNode = Node "" []

exEdge :: ∀a. Edges a => a -> Graph
exEdge e
  = id
  $ (\f -> graphFromEdges (extractNodes f) (extractEdges (Node "root" []) f))
  $ (\g -> fst $ uniqueNodes 1 g)
  $ fromMaybe (Root defaultNode [])
  $ (\a -> a !! 0)
  $ eliminateNothings
  $ edges e

data List' a = Nil | Cons a (List' a)

fromArray :: ∀a. Array a -> List' a
fromArray = foldr Cons Nil

derive instance genericList' :: Generic (List' a) _

instance genericEgde :: Edges a => Edges (List' a) where
  edges = genericEdges

-- instance edgesSimple :: Edges Simple where
--   edges = genericEdges

generateSvg :: ∀a. Edges a => a -> String
generateSvg e = (\text -> viz_internal text "svg" "dot" 1) $ toText $ exEdge e

exampleResult :: String
exampleResult = generateSvg (R (R B A A) B B)
