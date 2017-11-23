module Graphics.ConsoleImage where

import Data.Unit (Unit)

foreign import consoleImage :: Int -> String -> Unit

foreign import consoleSvgToPngImage :: Int -> String -> Unit
