module Data.DotLang where

import Prelude (class Show, show, ($), (<$>), (<>))
import Data.String (joinWith)
import Data.Generic.Rep
import Data.Generic.Rep.Show

type Id = String

data ShapeType
  = Box | Polygon | Ellipse | Oval | Circle | Point | Egg
  | Triangle | Plaintext | Plain | Diamond -- ...

instance showShape :: Show ShapeType where
  show Box = "box"
  show Polygon = "polygon"
  show Ellipse = "ellipse"
  show Oval = "oval"
  show Circle = "circle"
  show Point = "point"
  show Egg = "egg"
  show _ = "fuck"

instance shapeDotLang :: DotLang ShapeType where
  toText a = show a

data FillStyle = Filled

derive instance genericFillStyle :: Generic FillStyle _

instance showFillStyle :: Show FillStyle where
  show = genericShow

instance fillStyleDotLang :: DotLang FillStyle where
  toText Filled = "filled"

data Attr
  = Margin Int
  | FontColor String
  | FontSize Int
  | Width Int
  | Label String
  | Shape ShapeType
  | Style FillStyle

derive instance genericAttr :: Generic Attr _

instance showAttr :: Show Attr where
  show = genericShow

instance attrDotLang :: DotLang Attr where
  toText (Margin i) = "margin="<> show i
  toText (FontColor s) = "fontcolor="<> s
  toText (FontSize i) = "fontsize="<> show i
  toText (Width i) = "width="<> show i
  toText (Shape t) = "shape="<> (toText t)
  toText (Style f) = "fillstyle="<>(toText f)
  toText (Label t) = "label="<> show t

data Node = Node Id (Array Attr)

nodeId :: Node -> Id
nodeId (Node id _) = id

mapNodeId :: (Id -> Id) -> Node -> Node
mapNodeId f (Node id attr) = Node (f id) $ attr <> [Label id]

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show = genericShow

instance nodeDotLang :: DotLang Node where
  toText (Node id attrs) = id <> " [" <> (joinWith " ," (toText <$> attrs)) <> "]"


data Edge = Edge Id Id

derive instance genericEdge :: Generic Edge _

instance showEdge :: Show Edge where
  show = genericShow

data Definition
  = NodeDef Node
  | EdgeDef Edge
  | Subgraph (Array Definition)

instance definitionDotlang :: DotLang Definition where
  toText (NodeDef node) = toText node <> "; "
  toText (EdgeDef (Edge a b)) = a <> " -> " <> b <> "; "
  toText (Subgraph defs) = "subgraph {\n " <> (joinWith "" $ toText <$> defs) <> "}"

data Graph
  = Graph (Array Definition)
  | DiGraph (Array Definition)


instance graphDotLang :: DotLang Graph where
  toText (Graph defs) = "graph {\n" <> (joinWith "" $ toText <$> defs) <> "}"
  toText (DiGraph defs) = "digraph {\n" <> (joinWith "" $ toText <$> defs) <> "}"

graphFromEdges :: Array (Node) -> Array (Edge) -> Graph
graphFromEdges nodes edges = DiGraph $ (NodeDef <$> nodes) <> (EdgeDef <$> edges)

class DotR a where
  toDot :: a -> Graph

class DotLang a where
  toText :: a -> String
