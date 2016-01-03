{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.Algorithm.Diff  as DA
import           Data.Functor.Identity
import           Data.Snakes
import qualified Data.Snakes.DiffLike as DL
import           Data.Vector ( Vector )
import qualified Data.Vector as V
import           GHC.Generics
import           System.Random

#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
#endif

deriving instance Generic (DA.Diff a)

instance (NFData a) => NFData (DL.Diff a)
instance (NFData a) => NFData (DA.Diff a)

instance (NFData a) => NFData (Snake a)
instance (NFData a) => NFData (SnakeHead a)

instance (Monad m) => Stream (Vector a) m a where
  uncons v | null v    = return Nothing
           | otherwise = return $ Just (V.unsafeHead v, V.unsafeTail v)
  {-# INLINE uncons #-}

randomList :: (Random a) => Int -> IO [a]
randomList n = take n . randoms <$> newStdGen

similarLists :: (Random a) => Int -> Double -> IO ([a], [a])
similarLists n k = do
  ls <- randomList n
  rs <- randomList n
  ks <- randomRs (0, 1) <$> newStdGen
  return $ unzip $ map (\(l, r, k') -> (l, if k < k' then l else r)) $ zip3 ls rs ks

randomVector :: (Random a) => Int -> IO (Vector a)
randomVector n = V.fromList <$> randomList n

snakeInt :: (Stream s m t, Eq t) => Maybe Int -> s -> s -> m (Maybe (Snake Int))
snakeInt = snake
{-# INLINE snakeInt #-}

vectorBench :: Benchmark
vectorBench = bgroup "vectorBench"
  [ env (randomVector $ 2^10) $ \ (v :: Vector Char) ->
    bench "2^10 Core" $ (nf $ runIdentity . uncurry (snakeInt Nothing)) (v, v)
  , env (randomVector $ 2^20) $ \ (v :: Vector Char) ->
    bench "2^20 Core" $ (nf $ runIdentity . uncurry (snakeInt Nothing)) (v, v)
  ]

diffLikeBench :: Benchmark
diffLikeBench =  bgroup "Snakes vs Diff"
  [ env (randomList $ 2^10) $ \ (s :: String) ->
    bgroup "Same streams 2^10"
    [ bench "Snakes-Core" $ (nf $ runIdentity . uncurry (snakeInt Nothing)) (s, s)
    , bench "Snakes-Diff" $ (nf $ uncurry DL.getDiff) (s, s)
    , bench "Diff"   $ (nf $ uncurry DA.getDiff) (s, s)
    ]
  , env (randomList $ 2^20) $ \ (s :: String) ->
    bgroup "Same streams 2^20"
    [ bench "Snakes-Core" $ (nf $ runIdentity . uncurry (snakeInt Nothing)) (s, s)
    , bench "Snakes" $ (nf $ uncurry DL.getDiff) (s, s)
    , bench "Diff"   $ (nf $ uncurry DA.getDiff) (s, s)
    ]
  , env (randomList $ 2^10) $ \ (s1 :: String) ->
    env (randomList $ 2^10) $ \ (s2 :: String) ->
    bgroup "Random streams 2^10"
    [ bench "Snakes-Core (weak head)" $ (whnf $ runIdentity . uncurry (snakeInt Nothing)) (s1, s2)
    , bench "Snakes" $ (nf $ uncurry DL.getDiff) (s1, s2)
    , bench "Diff" $ (nf $ uncurry DA.getDiff) (s1, s2)
    ]
  , env (similarLists (2^10) 0.1) $ \ ~(s1 :: String, s2) ->
    bgroup "Random streams 2^10 (10% diff)"
    [ bench "Snakes-Core (weak head)" $ (whnf $ runIdentity . uncurry (snakeInt Nothing)) (s1, s2)
    , bench "Snakes" $ (nf $ uncurry DL.getDiff) (s1, s2)
    , bench "Diff" $ (nf $ uncurry DA.getDiff) (s1, s2)
    ]
  , env (similarLists (2^16) 0.01) $ \ ~(s1 :: String, s2) ->
    bgroup "Random streams 2^16 (1% diff)"
    [ bench "Snakes-Core (weak head)" $ (whnf $ runIdentity . uncurry (snakeInt Nothing)) (s1, s2)
    , bench "Snakes" $ (nf $ uncurry DL.getDiff) (s1, s2)
    , bench "Diff" $ (nf $ uncurry DA.getDiff) (s1, s2)
    ]
  ]

-- Our benchmark harness.
main :: IO ()
main = defaultMain
  [ diffLikeBench
  , vectorBench
  ]
