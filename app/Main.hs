module Main where

import Data.Snakes
import Data.Snakes.DiffLike

main :: IO ()
main = do
  print $ getDiff "axxbxx" "bxxaxxbxx" 
  return ()
