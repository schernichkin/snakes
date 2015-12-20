module Main where

import Data.Snakes

main :: IO ()
main = do
  a <- snake Nothing "abc" "abc"
  return ()
