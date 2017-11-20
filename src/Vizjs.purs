module Vizjs where

-- viz :: String -> Format -> Engine -> Int -> Either String String
-- viz text format engine 

foreign import viz_internal :: String -> String -> String -> Int -> String
