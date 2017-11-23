module Graphics.Graphviz (renderToJson, renderToSvg, renderReprSvg, Engine(..), svgXmlToPngBase64) where

import Color.Scale (addStop)
import Control.Bind ((>=>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.DotLang (class DotLang, class GraphRepr, toGraph, toText)
import Data.Function (($))
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Show (class Show, show)
import FFI.Util.Function (callAff2r1)
import FFI.Util (require)

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

-- foreign import data VizJs :: Type
--
-- vizjs :: VizJs
-- vizjs = require "viz.js"
--
-- --Viz.svgXmlToPngBase64(data, 1, function(err, data){window.a = data})
-- svgXmlToPngBase64 ∷ ∀a. String → Int → Aff (| a) String
-- svgXmlToPngBase64 text scale = callAff2r1 vizjs "svgXmlToPngBase64" text scale
