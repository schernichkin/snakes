-- | Data.Algorithm.Diff-like API
module Data.Snakes.DiffLike
  ( Diff (..)
  , getDiff
  ) where

import Data.Functor.Identity
import Data.Snakes

getDiff ::  Eq t => [t] -> [t] -> [Diff t]
getDiff l r = case runIdentity $ diffStream Straight Nothing l r of
  Just s -> runIdentity $ streamToList s
  Nothing -> error "Data.Snakes.DiffLike.getDiff: unbounded search does not yield any result."
{-# INLINE getDiff #-}
