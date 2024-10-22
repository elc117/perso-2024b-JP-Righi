module Main where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Hello Gloss!" (400, 400) (10, 10)) white (Circle 80)
