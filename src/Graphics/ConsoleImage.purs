module Graphics.ConsoleImage where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Unit (Unit)

foreign import consoleImage :: String -> Int -> Unit
