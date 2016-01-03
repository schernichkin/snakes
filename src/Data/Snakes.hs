{-# LANGUAGE CPP                    #-}

module Data.Snakes
    ( Stream (..)
    , Snake (..)
    , SnakeHead (..)
    , SnakeShape (..)
    , Diff (..)
    , snake
    , diffStream
    -- * Utils
    , streamToList
    ) where

import Data.Maybe (fromMaybe)
import Data.Snakes.Internal

#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
#endif

snake :: (Stream s m t, Eq t, Num a, Ord a)
      => Maybe a -> s -> s -> m (Maybe (Snake a))
snake d l r = do
  firstSnake <- slideDownSnake l r HeadNill 0
  case firstSnake of
    Left s  -> goLeftToRight d [s]
    Right s -> return $ Just s
  where
    go ex k d' ss = if fromMaybe 1 d' == 0
      then return Nothing
      else do
        r <- ex ss
        case r of
          Left ss' -> k (fmap (subtract 1) d') ss'
          Right s  -> return $ Just s

    goLeftToRight = go expandLeftToRight goRightToLeft
    goRightToLeft = go expandRightToLeft goLeftToRight
{-# INLINE snake #-}

diffStream :: (Stream s m t, Eq t, Num a, Ord a)
           => SnakeShape -> Maybe a -> s -> s -> m (Maybe (DiffStream s t a))
diffStream sh d l r = fmap (snakeToDiffStream sh l r) <$> snake d l r
{-# INLINE diffStream #-}
