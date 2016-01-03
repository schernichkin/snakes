module Main where

import Data.Snakes
import Data.Snakes.DiffLike

main :: IO ()
main = do
  a <- snake Nothing "" "1234S"
  print a
  return ()
