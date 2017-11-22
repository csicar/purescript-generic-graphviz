module Data.DotLang where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (joinWith)
import Prelude (class Show, show, ($), (<$>), (<>))

-- | type alias for a Nodes Name
type Id = String

-- | possible node shapes
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
  show Triangle = "triangle"
  show Plaintext = "plaintext"
  show Plain = "plain"
  show Diamond = "diamond"

instance shapeDotLang :: DotLang ShapeType where
  toText a = show a

data FillStyle
  = Filled
  | Dotted
  | Invis

derive instance genericFillStyle :: Generic FillStyle _

instance showFillStyle :: Show FillStyle where
  show = genericShow

instance fillStyleDotLang :: DotLang FillStyle where
  toText Filled = "filled"
  toText Dotted = "dotted"
  toText Invis = "invis"

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
  toText (FontColor s) = "fontcolor="<>s
  toText (FontSize i) = "fontsize="<> show i
  toText (Width i) = "width="<> show i
  toText (Shape t) = "shape="<> (toText t)
  toText (Style f) = "style="<>(toText f)
  toText (Label t) = "label="<> show t

-- | Dot-Node
-- | example : Node "e" [Margin 3, Label "some label"]
-- | is turned into: e [margin=3, label="some label"];
data Node = Node Id (Array Attr)


-- | get a nodes id
-- | example: nodeId (Node "e" [Label "foo"]) == "e"
nodeId :: Node -> Id
nodeId (Node id _) = id

-- | change Nodes id to a new one; keeing the old id as the label
-- | example: mapNodeId (\a -> a+"!") (Node "e" []) == Node "e!" [Label "e"]
changeNodeId :: (Id -> Id) -> Node -> Node
changeNodeId f (Node id attr) = Node (f id) $ attr <> [Label id]

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show = genericShow

instance nodeDotLang :: DotLang Node where
  toText (Node id attrs) = id <> " [" <> (joinWith " ," (toText <$> attrs)) <> "]"

-- | egde from id to id
-- | toText $ Edge "a" "b" == a -> b
-- | option for different arrows is missing
data Edge = Edge Id Id

derive instance genericEdge :: Generic Edge _

instance showEdge :: Show Edge where
  show = genericShow

-- | definition in a graph
data Definition
  = NodeDef Node
  | EdgeDef Edge
  | Subgraph (Array Definition)

instance definitionDotlang :: DotLang Definition where
  toText (NodeDef node) = toText node <> "; "
  toText (EdgeDef (Edge a b)) = a <> " -> " <> b <> ";\n "
  toText (Subgraph defs) = "subgraph {\n " <> (joinWith "" $ toText <$> defs) <> "}"

-- | graph can either be a graph or digraph
data Graph
  = Graph (Array Definition)
  | DiGraph (Array Definition)


instance graphDotLang :: DotLang Graph where
  toText (Graph defs) = "graph {\n" <> (joinWith "" $ toText <$> defs) <> "}"
  toText (DiGraph defs) = "digraph {\n" <> (joinWith "" $ toText <$> defs) <> "}"

-- | create graph from Nodes and Edges
-- | example: graphFromElements [Node "e" [], Node "d" []] [Edge "e" "f"]
graphFromElements :: Array (Node) -> Array (Edge) -> Graph
graphFromElements nodes edges = DiGraph $ (NodeDef <$> nodes) <> (EdgeDef <$> edges)

-- | a is a type that can be represented by a Dot-Graph
class GraphRepr a where
  toGraph :: a -> Graph

-- | a is a type that has a representation in the dot language
class DotLang a where
  toText :: a -> String
