module Data.Snakes
    ( Stream (..)
    , Snake (..)
    , SnakeHead (..)
    , snake
    ) where

import Data.Maybe (fromMaybe)
import Data.Snakes.Internal

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
