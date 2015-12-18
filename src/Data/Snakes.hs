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
import Data.Snakes.Internal

-- можно обойтись без билдеров, если мы будем переключать left-right в зависимости
-- от прохода.
expandLeft :: (Stream s m t, Eq t, Num a, Eq a) => [SnakeState a s] -> m (Either [SnakeState a s] (Maybe (Snake a)))
-- if have state to expand, attemt to move left
expandLeft (SnakeState prevSnake progress ls rs:_) = do
  return undefined
-- this should never happen (the only reason for empty acc is when both streams
-- ended but in this case we have found the solution)
expandLeft [] = fail "Data.Snakes: no state to expand."

expand :: (Stream s m t, Eq t, Num a, Eq a) => Maybe a -> [SnakeState a s] -> m (Maybe (Snake a))
expand d ss = if fromMaybe 1 d == 0
    -- if search distance limit hit return nothing
    then return Nothing
    -- otherwise attemt to expand
    else expandLeft
  where
    -- это не верно. Нам нужно либо собрать аккумулятор (причем в таком виде,
    -- чтобы не пришлось его разворачивать и перейти с ним на следующую итерацию,
    -- либо выяснить, что мы нашли решение).
    expandLeft = do
      case ss of
        -- if having state to expand, attemt to move left
        (SnakeState prevSnake progress ls rs:_) -> do
          lr <- uncons ls
          case lr of
            -- if can move left build left snake
            Just (l, ls') -> do
              (a, ms) <- slideDown ls' rs
              let leftSnake = Snake (HeadLeft prevSnake) a
              case ms of
                -- if left snake not final add it to accumulator and continue expanding
                Just (ls'', rs'') -> expandCenter [SnakeState leftSnake (progress + 1 + a * 2) ls'' rs'']
            -- otherwise continue expanding without left snake
            Nothing -> expandCenter []
        -- otherwise return nothing
        [] -> return Nothing

    expandCenter = go ss
      where
        go ss' acc = undefined

snakes :: (Stream s m t, Eq t, Num a, Eq a) => s -> s -> Maybe a -> m (Maybe (Snake a))
snakes ls rs d = do
  -- build head snake
  (a, ms) <- slideDown ls rs
  let headSnake = Snake HeadNill a
  case ms of
    -- if head snake not final attemt to expand search area
    Just (ls', rs') -> expand d [SnakeState headSnake (a * 2) ls' rs']
    -- otherwise just return it
    Nothing -> return $ Just headSnake
