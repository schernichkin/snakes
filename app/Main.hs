module Main where

import Data.Snakes

main :: IO ()
main = do
  a <- snakes "abc" "abc" (Just 10)
  return ()
