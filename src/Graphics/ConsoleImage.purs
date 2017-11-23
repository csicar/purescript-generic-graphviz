module Graphics.ConsoleImage where

import Data.Unit (Unit)

foreign import consoleImage :: Number -> String -> Unit

foreign import consoleSvgToPngImage :: Number -> String -> Unit
