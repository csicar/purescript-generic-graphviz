module Data.GenericGraph where

import Control.Semigroupoid ((>>>))
import Data.Array (concat, foldr, (!!), (:))
import Data.DotLang (Attr(..), Edge(..), FillStyle(..), Graph, Node(..), graphFromEdges, mapNodeId, nodeId)
import Data.Functor ((<$>))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Field(..), NoArguments, NoConstructors, Product(..), Rec(..), Sum(..), from)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..), fst)
import Prelude (class Show, id, show, ($), (+), (<$>), (<>))


-- Tree type
data Tree a = Root a (Array (Tree a))

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show (Root a l) = (show a) <> "( " <> (joinWith "\n" $ show <$> l) <> ")"

class Edges a where
  edges :: a -> Tree (Maybe Node)


class GenericEdges a where
  genericEdges' :: a -> Tree (Maybe Node)

instance genericEdgesNoConstructors :: GenericEdges NoConstructors where
  genericEdges' _ = Root Nothing []

instance genericEdgesNoArguments :: GenericEdges NoArguments where
  genericEdges' _ =  Root Nothing []

instance genericEdgesSum :: (GenericEdges a, GenericEdges b) => GenericEdges (Sum a b) where
  genericEdges' (Inl a) = genericEdges' a
  genericEdges' (Inr b) = genericEdges' b

instance genericEdgesProduct :: (GenericEdges a, GenericEdges b) => GenericEdges (Product a b) where
  genericEdges' (Product a b) = Root Nothing [genericEdges' a, genericEdges' b]

instance genericEdgesConstructor :: (GenericEdges a, IsSymbol name) => GenericEdges (Constructor name a) where
  genericEdges' (Constructor a) = Root (Just $ Node constructorName []) [ genericEdges' a ]
    where
      constructorName = reflectSymbol (SProxy :: SProxy name)


instance stringEdge :: Edges String where
  edges a = Root (Just $ Node (show a) []) []

instance intEdge :: Edges Int where
  edges i = Root (Just $ Node (show i) []) []

instance numberEdge :: Edges Number where
  edges n = Root (Just $ Node (show n) []) []

instance charEdge :: Edges Char where
  edges c = Root (Just $ Node (show c) []) []

instance boolEdge :: Edges Boolean where
  edges b = Root (Just $ Node (show b) []) []

instance arrayEdges :: Edges a => Edges (Array a) where
  edges a = Root (Just $ Node ("array") []) (edges <$> a)

instance genericReprArgument :: Edges a => GenericEdges (Argument a) where
  genericEdges' (Argument a) = edges a


instance genericEdgesRec :: GenericEdges a => GenericEdges (Rec a) where
  genericEdges' (Rec a) = genericEdges' a

instance genericEdgesField :: (Edges a, IsSymbol name) => GenericEdges (Field name a) where
  genericEdges' (Field a) = Root (Just $ Node fieldName []) [ edges a ]
    where
      fieldName = reflectSymbol (SProxy :: SProxy name)

-- | A `Generic` implementation of the `eq` member from the `Eq` type class.
genericEdges :: forall a rep. Generic a rep => GenericEdges rep => a -> Tree (Maybe Node)
genericEdges a = genericEdges' (from a)

eliminateNothings :: ∀a. Tree (Maybe a) -> Array (Tree a)
eliminateNothings (Root Nothing list) = concat $ eliminateNothings <$> list
eliminateNothings (Root (Just a) list) = [Root a $ concat $ eliminateNothings <$> list]

uniqueNode :: Tree Node -> Tuple Int (Array (Tree Node)) -> Tuple Int (Array (Tree Node))
uniqueNode child (Tuple accId accChildren) = let
    Tuple newChild newId = uniqueNodes' accId child
  in
    Tuple newId (newChild : accChildren)

uniqueNodes' :: Int -> Tree Node -> Tuple (Tree Node) Int
uniqueNodes' id' (Root node children) = let
    newNode = mapNodeId (\name -> show id') node
    id = id' + 1
    Tuple finalId newChildren = foldr uniqueNode (Tuple id []) children
  in  Tuple (Root newNode newChildren) finalId

uniqueNodes :: Tree Node -> Tree Node
uniqueNodes = (uniqueNodes' 0) >>> fst

extractEdges :: Node -> Tree Node -> Array Edge
extractEdges parent (Root node children) = [Edge (nodeId parent) (nodeId node)] <>
  (concat $
    (extractEdges node) <$> children)

extractNodes :: Tree Node -> Array Node
extractNodes (Root node children) = node : (concat $ extractNodes <$> children)

genericToDot :: ∀a. Edges a => a -> Graph
genericToDot e
  = id
  $ (\f -> graphFromEdges ((Node "root" [Style Invis]) : extractNodes f) (extractEdges (Node "root" []) f))
  $ uniqueNodes
  $ fromMaybe (Root (Node "" []) [])
  $ (\a -> a !! 0)
  $ eliminateNothings
  $ edges e
