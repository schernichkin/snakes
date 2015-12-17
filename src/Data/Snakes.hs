{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.Snakes
    ( Stream (..)
    , Snake (..)
    , SnakeHead (..)
    , snakes
    ) where

import Data.Maybe (fromMaybe)

class Monad m => Stream s m t | s -> t where
  uncons :: s -> m (Maybe (t, s))

instance Monad m => Stream [a] m a where
  uncons []     = return $ Nothing
  uncons (x:xs) = return $ Just (x, xs)

data Snake a = Snake (SnakeHead a) a deriving ( Show, Eq )

data SnakeHead a = HeadNill
                 | HeadLeft (Snake a)
                 | HeadRight (Snake a)
                 | HeadBoth (Snake a) (Snake a)
                 deriving ( Show, Eq )

data SnakeState a s = SnakeState (Snake a) a s s

slideDown :: (Stream s m t, Eq t, Num a) => s -> s -> m (a, Maybe (s, s))
slideDown = go 0 where
  go a ls rs = do
    lr <- uncons ls
    rr <- uncons rs
    case (lr, rr) of
      (Just (l, ls'), Just (r, rs')) | l == r -> go (a + 1) ls' rs'
      (Nothing, Nothing) -> return (a, Nothing)
      _ -> return (a, Just (ls, rs))

start :: (Stream s m t, Eq t, Num a) => s -> s -> m (Snake a, Maybe (s, s))
start ls rs = do
  (a, ms) <- slideDown ls rs
  return (Snake HeadNill a, ms)

expand :: (Num a, Eq a, Monad m) => Maybe a -> [SnakeState a s] -> m (Maybe (Snake a))
expand d ss = if fromMaybe 1 d == 0
  then return Nothing
  else error "d > 0"
    
snakes :: (Stream s m t, Eq t, Num a, Eq a) => s -> s -> Maybe a -> m (Maybe (Snake a))
snakes ls rs d = do
  (h@(Snake _ l), ms) <- start ls rs
  case ms of
    Just (ls, rs) -> expand d [SnakeState h l ls rs]
    Nothing -> return $ Just h
