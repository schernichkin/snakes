{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | This module is created to expose internal functions for unit testing
-- only and should not be used directly.
module Data.Snakes.Internal where

-- | Generic stream to read tokens from.
class Monad m => Stream s m t | s -> t where
  uncons :: s -> m (Maybe (t, s))

-- | Stream instance for haskell list.
instance Monad m => Stream [a] m a where
  uncons []     = return Nothing
  uncons (x:xs) = return $ Just (x, xs)

-- | Snake indicates where non-matching token was read from and matching token count.
data Snake a = Snake (SnakeHead a) a deriving ( Show, Eq )

-- | Snake head points where token was read from (left or right or none in case
-- of first snake or either if both actions will lead to the same progress value).
data SnakeHead a = HeadNill
                 | HeadLeft (Snake a)
                 | HeadRight (Snake a)
                 | HeadEither (Snake a) (Snake a)
                 deriving ( Show, Eq )

-- | Snake state holds snake, progress value (sum of tokens read from the both
-- streams) and unconsumed streams.
data SnakeState a s = SnakeState (Snake a) a s s deriving ( Show, Eq )

-- | Build snake's head dual (i.e. swap left and right).
reflectHead :: SnakeHead a -> SnakeHead a
reflectHead HeadNill = HeadNill
reflectHead (HeadLeft l) = HeadRight l
reflectHead (HeadRight r) = HeadLeft r
reflectHead (HeadEither l r) = HeadEither r l

-- | Build snake's dual (i.e. swap left and right).
reflectSnake :: Snake a -> Snake a
reflectSnake (Snake h a) = Snake (reflectHead h) a

-- | Build snake's state dual (i.e. swap left and right).
reflectState :: SnakeState a s -> SnakeState a s
reflectState (SnakeState s p ls rs) = SnakeState (reflectSnake s) p rs ls

-- | Swap left and right streams of state, but does not affect snake
reflectStateOnly :: SnakeState a s -> SnakeState a s
reflectStateOnly (SnakeState s p ls rs) = SnakeState s p rs ls

-- | Slide down the diagonal till tokens are equals and returns diagonal length
-- and possibly new streams or nothing if both streams consumed (solution found).
slideDown :: (Stream s m t, Eq t, Num a) => s -> s -> m (a, Maybe (s, s))
slideDown = go 0 where
  go a ls rs = do
    lr <- uncons ls
    rr <- uncons rs
    case (lr, rr) of
      (Just (l, ls'), Just (r, rs')) | l == r -> go (a + 1) ls' rs'
      (Nothing, Nothing) -> return (a, Nothing)
      _ -> return (a, Just (ls, rs))

-- | Attemt to build a new snake by consuming token from the left stream.
-- Returns nothing if the left stream is empty, or new state if non-final snake
-- created or just snake if it's final.
snakeLeft :: (Stream s m t, Eq t, Num a, Eq a)
          => SnakeState a s
          -> m (Maybe (Either (SnakeState a s) (Snake a)))
snakeLeft (SnakeState prevSnake progress ls rs) = do
  lr <- uncons ls
  case lr of
    Just (_, ls') -> do
      (a, ms) <- slideDown ls' rs
      let leftSnake = Snake (HeadLeft prevSnake) a
      return $ Just $ case ms of
        Just (ls'', rs'') -> Left $ SnakeState leftSnake (progress + 1 + a * 2) ls'' rs''
        Nothing -> Right leftSnake
    Nothing -> return Nothing

-- | Attemt to build a new snake by consuming token from the right stream.
-- Returns nothing if the right stream is empty, or new state if non-final snake
-- created or just snake if it's final.
snakeRight :: (Stream s m t, Eq t, Num a, Eq a)
          => SnakeState a s
          -> m (Maybe (Either (SnakeState a s) (Snake a)))
-- здесь вложенную змею менять не надо, т.к. не мы её создавали
snakeRight = fmap (fmap reflectResut) . snakeLeft . reflectStateOnly
  where
    reflectResut (Left s) = Left $ reflectState s
    reflectResut (Right s) = Right $ reflectSnake s
