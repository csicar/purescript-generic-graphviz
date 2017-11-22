module Graphics.Graphviz (renderToJson, renderToSvg, renderReprSvg, Engine(..)) where

import Data.DotLang (class DotLang, class GraphRepr, toGraph, toText)
import Data.Function (($))
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Show (class Show, show)

data Engine
  = Circo
  | Dot
  | Neato
  | Osage
  | Twopi

instance showEngine :: Show Engine where
  show Circo = "circo"
  show Dot = "dot"
  show Neato = "neato"
  show Osage = "osage"
  show Twopi = "twopi"

--
-- newtype GraphvizJson = GraphvizJson
--   { name :: String
--   , directed :: Boolean
--   , strict :: Boolean
--   , _draw_ :: Array ()
--
-- }

renderReprSvg :: ∀a. GraphRepr a => Engine -> a -> String
renderReprSvg e a = renderToSvg e $ toGraph a

renderToJson :: ∀a. DotLang a => Engine -> a -> String
renderToJson e a = runFn4 viz_internal (toText a) "json" (show e) 1

renderToSvg :: ∀a. DotLang a => Engine -> a -> String
renderToSvg e a = runFn4 viz_internal (toText a) "svg" (show e) 1

foreign import viz_internal :: Fn4 String String String Int String
