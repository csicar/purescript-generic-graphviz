module GenericGraph where

import Dot
import Data.Generic.Rep
import Data.Show
import Data.Tuple
import Prelude
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

mapFst f (Tuple a b) = Tuple (f a) b

mapSnd f (Tuple a b) = Tuple a (f b)

addNode :: Node -> Tuple (Array Node) (Array Edge) -> Tuple (Array Node) (Array Edge)
addNoe n = mapFst (\list -> n : list)

connectEdges :: Node -> Tuple (Array Node) (Array Edge) -> Tuple (Array Node) (Array Edge)
connectEdges (Node n _) (Tuple a b) = Tuple [] $ b <> ((\(Node node _) -> Edge n node) <$> a)

class GenericEdges a where
  genericEdges' :: a -> Tuple (Array Node) (Array Edge)

instance genericEdgesNoConstructors :: GenericEdges NoConstructors where
  genericEdges' _ = Tuple [] []

instance genericEdgesNoArguments :: GenericEdges NoArguments where
  genericEdges' _ = Tuple [] []

instance genericEdgesSum :: (GenericEdges a, GenericEdges b) => GenericEdges (Sum a b) where
  genericEdges' (Inl a) = genericEdges' a
  genericEdges' (Inr b) = genericEdges' b

instance genericEdgesProduct :: (GenericEdges a, GenericEdges b) => GenericEdges (Product a b) where
  genericEdges' (Product a b) = let
      Tuple nodesA edgesA = genericEdges' a
      Tuple nodesB edgesB = genericEdges' b
    in
      Tuple (nodesA <> nodesB) (edgesA <> edgesB)

instance genericEdgesConstructor :: (GenericEdges a, IsSymbol name) => GenericEdges (Constructor name a) where
  genericEdges' (Constructor a) = connectEdges (Node constructorName []) $ genericEdges' a
    where
      constructorName = reflectSymbol (SProxy :: SProxy name)

instance genericEdgesArgument :: Show a => GenericEdges (Argument a) where
  genericEdges' (Argument a) = Tuple [Node (show a) []] []

instance genericEdgesRec :: GenericEdges a => GenericEdges (Rec a) where
  genericEdges' (Rec a) = genericEdges' a

instance genericEdgesField :: Show a => GenericEdges (Field name a) where
  genericEdges' (Field a) = Tuple [Node (show a) []] []

-- | A `Generic` implementation of the `eq` member from the `Eq` type class.
genericEdges :: forall a rep. Generic a rep => GenericEdges rep => a -> Tuple (Array Node) (Array Edge)
genericEdges a= genericEdges' (from a)
