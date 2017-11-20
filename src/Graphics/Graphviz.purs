module Graphics.Graphviz (renderToJson, renderToSvg, Engine(..)) where

import Data.DotLang (class DotR, toDot, toText)
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

renderToSvg :: ∀a. DotR a => Engine -> a -> String
renderToSvg e a = runFn4 viz_internal (toText $ toDot a) "svg" (show e) 1

renderToJson :: ∀a. DotR a => Engine -> a -> String
renderToJson e a = runFn4 viz_internal (toText $ toDot a) "json" (show e) 1


foreign import viz_internal :: Fn4 String String String Int String
