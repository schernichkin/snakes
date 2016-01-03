{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | This module is created to expose internal functions for unit testing
-- only and should not be used directly.
module Data.Snakes.Internal where

import GHC.Generics

#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
#endif

-- | Generic stream to read tokens from.
class Monad m => Stream s m t | s -> t where
  uncons :: s -> m (Maybe (t, s))

streamToList :: (Stream s m t) => s -> m [t]
streamToList = go []
  where
    go acc s = do
      res <- uncons s
      case res of
        Just (a, s') -> go (a:acc) s'
        Nothing -> return $ reverse acc
{-# INLINE streamToList #-}

-- | Stream instance for haskell list.
instance Monad m => Stream [a] m a where
  uncons (x:xs) = return $ Just (x, xs)
  uncons []     = return Nothing
  {-# INLINE uncons #-}

-- | Snake indicates where non-matching token was read from and matching token count.
data Snake a = Snake (SnakeHead a) a deriving ( Show, Eq, Generic )

-- | Snake head points where token was read from (left or right or none in case
-- of first snake or either if both actions will lead to the same progress value).
data SnakeHead a = HeadNill
                 | HeadLeft (Snake a)
                 | HeadRight (Snake a)
                 | HeadEither (Snake a) (Snake a)
                 deriving ( Show, Eq, Generic )

-- | Snake state holds snake, progress value (sum of tokens read from the both
-- streams) and unconsumed streams.
data SnakeState a s = SnakeState (Snake a) a s s deriving ( Show, Eq )

-- | Slide down the diagonal till tokens are equals and returns diagonal length
-- and possibly new streams or nothing if both streams exhausted (solution found).
slideDown :: (Stream s m t, Eq t, Num a) => s -> s -> m (a, Maybe (s, s))
slideDown = go 0 where
  go a ls rs = do
    lr <- uncons ls
    rr <- uncons rs
    case (lr, rr) of
      (Just (l, ls'), Just (r, rs')) | l == r -> go (a + 1) ls' rs'
      (Nothing, Nothing) -> return (a, Nothing)
      _ -> return (a, Just (ls, rs))
{-# INLINE slideDown #-}

-- | Slide down the diagonal and return either Snake if both streams exhausted
-- (solution found) or SnakeState otherwise.
slideDownSnake :: (Stream s m t, Eq t, Num a)
               => s -> s -> SnakeHead a -> a
               -> m (Either (SnakeState a s) (Snake a))
slideDownSnake ls rs snakeHead headProgress = do
  (a, ms) <- slideDown ls rs
  let snake = Snake snakeHead a
  return $ case ms of
    Just (ls', rs') -> Left $ SnakeState snake (a * 2 + headProgress) ls' rs'
    Nothing -> Right snake
{-# INLINE slideDownSnake #-}

-- | Attemt to build a new snake by consuming token from the left stream.
-- Returns nothing if the left stream is empty, or new state if non-final snake
-- created or just snake if it's final.
moveLeft :: (Stream s m t, Eq t, Num a, Eq a)
         => SnakeState a s
         -> m (Maybe (Either (SnakeState a s) (Snake a)))
moveLeft (SnakeState prevSnake prevProgress ls rs) =
  uncons ls >>= mapM (\(_, ls') -> slideDownSnake ls' rs (HeadLeft prevSnake) (prevProgress + 1))

-- | Attemt to build a new snake by consuming token from the right stream.
-- Returns nothing if the right stream is empty, or new state if non-final snake
-- created or just snake if it's final.
moveRight :: (Stream s m t, Eq t, Num a, Eq a)
           => SnakeState a s
           -> m (Maybe (Either (SnakeState a s) (Snake a)))
moveRight (SnakeState prevSnake prevProgress ls rs) =
  uncons rs >>= mapM (\(_, rs') -> slideDownSnake ls rs' (HeadRight prevSnake) (prevProgress + 1))

-- | Move from the left snake to the right or from the right snake to the left
-- or record either moves if they lead to the same progress.
-- It should be always possible to move right from the left snake or move left
-- from the right snake unless we have read all data from the both streams.
-- If we have equal progress on both streams, left stream of left snake will be
-- one token ahead left stream of right snake and vise versa, hence we do not
-- need to uncons streams to build a new snake.
moveCenter :: (Stream s m t, Eq t, Num a, Ord a)
           => SnakeState a s
           -> SnakeState a s
           -> m (Maybe (Either (SnakeState a s) (Snake a)))
moveCenter l@(SnakeState leftSnake  leftProgress  leftStream  _)
           r@(SnakeState rightSnake rightProgress _ rightStream)
           =
  case leftProgress `compare` rightProgress of
    LT -> moveLeft r
    EQ -> Just <$> slideDownSnake leftStream rightStream (HeadEither rightSnake leftSnake) (leftProgress + 1)
    GT -> moveRight l

-- | Handle move result and continue expansion process using specified
-- continuation function.
continueExpand :: (Stream s m t, Eq t, Num a, Ord a)
         => ([SnakeState a s] -> m (Either [SnakeState a s] (Snake a)))
         -> [SnakeState a s]
         -> Maybe (Either (SnakeState a s) (Snake a))
         -> m (Either [SnakeState a s] (Snake a))
continueExpand k acc r = case r of
  Just (Left s)  -> k (s:acc)
  Just (Right s) -> return $ Right s
  Nothing        -> k acc

-- | Expad current state by 1 assuming snakes are given in left-to-right
-- order (i.e. leftmost snake is the fist in state list)
-- Return either expanded state in right-to-left order (i.e. leftmost snake
-- will be the last in the list) or Snake if solution has been found.
-- State list to expand should not be empty.
expandLeftToRight :: (Stream s m t, Eq t, Num a, Ord a)
                  => [SnakeState a s]
                  -> m (Either [SnakeState a s] (Snake a))
expandLeftToRight ss = moveLeft (head ss) >>= continueExpand (go ss) []
  where
    go state acc = case state of
      l:remains@(r:_) -> moveCenter l r >>= continueExpand (go remains) acc
      [r] -> moveRight r >>= continueExpand (return . Left) acc

-- | Expad current state by 1 assuming snakes are given in right-to-left
-- order (i.e. leftmost snake is the last in state list)
-- Return either expanded state in left-to-right order (i.e. leftmost snake
-- will be the first in the list) or Snake if solution has been found.
-- State list to expand should not be empty.
expandRightToLeft :: (Stream s m t, Eq t, Num a, Ord a)
                  => [SnakeState a s]
                  -> m (Either [SnakeState a s] (Snake a))
expandRightToLeft ss = moveRight (head ss) >>= continueExpand (go ss) []
  where
    go state acc = case state of
      r:remains@(l:_) -> moveCenter l r >>= continueExpand (go remains) acc
      [l] -> moveLeft l >>= continueExpand (return . Left) acc

-- | A value is either from the 'First' stream, the 'Second' or from 'Both'.
-- 'Both' contains both the left and right values, in case you are using a form
-- of equality that doesn't check all data (for example, if you are using a
-- newtype to only perform equality on side of a tuple).
data Diff t = First t
            | Second t
            | Both t t
            deriving ( Show, Eq, Generic )

-- в публичных модулях не следует экспортировать конструктор, следует
-- использовать этот тип в качестве абстрактного, для которого важно только
-- то, что он является экземпляром  Stream
data DiffStream s t a = DiffStream (DiffStreamHead s t a) s s a

data DiffStreamHead s t a = StreamNill
                          | StreamLeft  ((s, s) -> DiffStream s t a)
                          | StreamRight ((s, s) -> DiffStream s t a)

instance (Stream s m t, Num a, Eq a) => Stream (DiffStream s t a) m (Diff t) where
  uncons (DiffStream h ls rs n) = if n == 0
    then case h of
      StreamNill   -> return Nothing
      StreamLeft f -> do
        lr <- uncons ls
        return $ do
          (l, ls') <- lr
          return (First l, f (ls', rs))
      StreamRight f -> do
        rr <- uncons rs
        return $ do
          (r, rs') <- rr
          return (Second r, f (ls, rs'))
    else do
      lr <- uncons ls
      rr <- uncons rs
      return $ do
        (l, ls') <- lr
        (r, rs') <- rr
        return (Both l r, DiffStream  h ls' rs' (n - 1))
  {-# INLINE uncons #-}

data SnakeShape = Straight | Wriggled

data LastBend = BendLeft | BendRight

snakeToDiffStream :: SnakeShape -> s -> s -> Snake a -> DiffStream s t a
snakeToDiffStream shape ls rs = go BendRight StreamNill
  where
    go _ acc (Snake HeadNill a) = DiffStream acc ls rs a
    go _ acc (Snake (HeadLeft s) a) = bendLeft acc a s
    go _ acc (Snake (HeadRight s) a) = bendRight acc a s
    go bend acc (Snake (HeadEither sl sr) a) = case (shape, bend) of
      (Straight, BendLeft)  -> bendLeft acc a sl
      (Wriggled, BendRight) -> bendLeft acc a sl
      (Straight, BendRight) -> bendRight acc a sr
      (Wriggled, BendLeft)  -> bendRight acc a sr

    bendLeft acc a = go BendLeft (StreamLeft $ streamCont acc a)
    bendRight acc a = go BendRight (StreamRight $ streamCont acc a)

    streamCont acc a (ls', rs') = DiffStream acc ls' rs' a
{-# INLINE snakeToDiffStream #-}
