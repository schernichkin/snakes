{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Functor.Identity
import           Data.Maybe
import           Data.Snakes
import           Data.Snakes.Internal
import           Test.HUnit hiding ( Test )
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import           Debug.Trace

instance (Arbitrary a) => Arbitrary (Snake a) where
  arbitrary = Snake <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (SnakeHead a) where
  arbitrary = frequency
    [ (5, return HeadNill)
    , (10, HeadLeft <$> arbitrary)
    , (10, HeadRight <$> arbitrary)
    , (2, HeadEither <$> arbitrary <*> arbitrary)
    ]

instance (Arbitrary a, Arbitrary s) => Arbitrary (SnakeState a s) where
  arbitrary = SnakeState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data SmallList a = SmallList [a] deriving ( Show, Eq )

instance (Arbitrary a) => Arbitrary (SmallList a) where
  arbitrary = do
    l <- choose (0, 10)
    SmallList <$> vector l

  shrink (SmallList a) = map SmallList $ shrink a

snakesTest :: Test
snakesTest = testGroup "Snakes"
  [ slideDownTest
  , moveLeftTest
  , moveRightTest
  , moveCenterTest
  , expandLeftToRightTest
  , expandRightToLeftTest
  , snakeTest
  ]
  where
    slideDownTest = testGroup "slideDown"
      [ testProperty "matching streams to end" $ \(ss :: [Int]) ->
          let res = runIdentity $ slideDown ss ss
          in  res == (length ss, Nothing)
      , testProperty "all matching tokens" $ \(s1 :: [Int]) (s2 :: [Int]) ->
          let (l, _) = runIdentity $ slideDown s1 s2
              r1 = drop l s1
              r2 = drop l s2
          in  (take l s1 == take l s2)
           && (null r1 || null r2 || (head r1 /= head r2))
      , testProperty "return correct remains" $ \(s1 :: [Int]) (s2 :: [Int]) ->
          let (l, rs) = runIdentity $ slideDown s1 s2
              r1 = drop l s1
              r2 = drop l s2
          in case rs of
            Just (rs1, rs2) -> r1 == rs1 && r2 == rs2
            Nothing -> null r1 && null r2
      ]
    moveLeftTest = testGroup "moveLeft"
      [ testProperty "always left" $ \(s :: SnakeState Int [Int]) ->
          let res = runIdentity $ moveLeft s
          in case res of
            Just (Left (SnakeState (Snake (HeadLeft _) _) _ _ _)) -> True
            Just (Right (Snake (HeadLeft _) _)) -> True
            Nothing -> True
            _ -> False
      , testProperty "point to prev snake" $ \(s@(SnakeState prevSnake _ _ _) :: SnakeState Int [Int]) ->
          let res = runIdentity $ moveLeft s
          in case res of
            Just (Left (SnakeState (Snake (HeadLeft snake) _) _ _ _)) -> snake == prevSnake
            _ -> True
      , testProperty "consume streams correctly" $ \(s@(SnakeState _ _ ls rs) :: SnakeState Int [Int]) ->
          let res = runIdentity $ moveLeft s
          in case res of
            Just (Left (SnakeState (Snake _ a) _ ls' rs')) -> take a (tail ls) == take a rs
                                                             && drop (a + 1) ls == ls'
                                                             && drop a rs == rs'
            Just (Right (Snake _ a)) -> length ls == (a + 1)
                                     && length rs == a
                                     && tail ls == rs
            Nothing -> null ls
      ]
    moveRightTest = testGroup "moveRight"
      [ testProperty "always right" $ \(s :: SnakeState Int [Int]) ->
          let res = runIdentity $ moveRight s
          in case res of
            Just (Left (SnakeState (Snake (HeadRight _) _) _ _ _)) -> True
            Just (Right (Snake (HeadRight _) _)) -> True
            Nothing -> True
            _ -> False
      , testProperty "point to prev snake" $ \(s@(SnakeState prevSnake _ _ _) :: SnakeState Int [Int]) ->
          let res = runIdentity $ moveRight s
          in case res of
            Just (Left (SnakeState (Snake (HeadRight snake) _) _ _ _)) -> snake == prevSnake
            _ -> True
      , testProperty "consume streams correctly" $ \(s@(SnakeState _ _ ls rs) :: SnakeState Int [Int]) ->
          let res = runIdentity $ moveRight s
          in case res of
            Just (Left (SnakeState (Snake _ a) _ ls' rs')) -> take a (tail rs) == take a ls
                                                             && drop (a + 1) rs == rs'
                                                             && drop a ls == ls'
            Just (Right (Snake _ a)) -> length rs == (a + 1)
                                     && length ls == a
                                     && tail rs == ls
            Nothing -> null rs
      ]
    moveCenterTest = testGroup "moveCenter"
      [ testCase "both (same progress)" $
          let leftStream  = "xbc"
              rightStream = "ybc"
              headSnake   = Snake HeadNill 0
              leftSnake   = Snake (HeadLeft headSnake) 0
              rightSnake  = Snake (HeadRight headSnake) 0
              leftState   = SnakeState leftSnake  1 (tail leftStream) rightStream
              rightState  = SnakeState rightSnake 1 leftStream (tail rightStream)
              res = runIdentity $ moveCenter leftState rightState
          in Just (Right (Snake (HeadEither rightSnake leftSnake) 2)) @=? res
      ]
    expandLeftToRightTest = testGroup "expandLeftToRight"
      [ testCase "start" $
        let leftStream  = "xbc"
            rightStream = "ybc"
            headSnake   = Snake HeadNill 0
            headState   = SnakeState headSnake 0 leftStream rightStream
            res = runIdentity $ expandLeftToRight [headState]
        in Left [ SnakeState (Snake (HeadRight headSnake) 0) 1 leftStream (tail rightStream)
                , SnakeState (Snake (HeadLeft  headSnake) 0) 1 (tail leftStream) rightStream
                ] @=? res
      ]
    expandRightToLeftTest = testGroup "expandRightToLeft"
      [ testCase "start" $
        let leftStream  = "xbc"
            rightStream = "ybc"
            headSnake   = Snake HeadNill 0
            headState   = SnakeState headSnake 0 leftStream rightStream
            res = runIdentity $ expandRightToLeft [headState]
        in Left [ SnakeState (Snake (HeadLeft  headSnake) 0) 1 (tail leftStream) rightStream
                , SnakeState (Snake (HeadRight headSnake) 0) 1 leftStream (tail rightStream)
                ] @=? res
      ]
    snakeTest = testGroup "snake"
      [ testCase "empty streams" $
          let res = runIdentity $ snake Nothing [] ([] :: [Int])
          in Just (Snake HeadNill 0) @=? res
      , testProperty "identical streams" $ \(s :: [Int]) ->
          let res = runIdentity $ snake Nothing s s
          in Just (Snake HeadNill $ length s) == res
      , testProperty "first stream empty" $ \(s :: [Int]) ->
          let res = runIdentity $ snake Nothing [] s
          in (Just $ foldr (\_ a -> Snake (HeadRight a) 0) (Snake HeadNill 0) s) == res
      , testProperty "second stream empty" $ \(s :: [Int]) ->
          let res = runIdentity $ snake Nothing s []
          in (Just $ foldr (\_ a -> Snake (HeadLeft a) 0) (Snake HeadNill 0) s) == res
      , testCase "known problem 1" $
          let leftStream  = "xbc"
              rightStream = "ybc"
              res = runIdentity $ snake Nothing leftStream rightStream
              headSnake   = Snake HeadNill 0
          in Just (Snake (HeadEither (Snake (HeadRight headSnake) 0) (Snake (HeadLeft headSnake) 0)) 2) @=? res
      , testCase "known problem 2" $
          let leftStream  = "axxbxx"
              rightStream = "bxxaxxbxx"
              res = runIdentity $ snake Nothing leftStream rightStream
          in Just (Snake (HeadRight (Snake (HeadRight (Snake (HeadRight (Snake HeadNill 0)) 0)) 0)) 6) @=? res
      , testProperty "all snakes of same length" $ \(SmallList s1 :: SmallList Int)
                                                    (SmallList s2 :: SmallList Int) ->
          let res = map pathLength $ allPaths $ fromJust $ runIdentity $ snake Nothing s1 s2
          in all (head res ==) res
      ]
    allPaths :: Snake a -> [[Snake a]]
    allPaths = go id [] []
      where
        go k tree path s@(Snake HeadNill _) = k ((s:path):tree)
        go k tree path s@(Snake (HeadLeft l) _) = go k tree (s:path) l
        go k tree path s@(Snake (HeadRight r) _) = go k tree (s:path) r
        go k tree path s@(Snake (HeadEither l r) _) = go (\acc -> go k acc (s:path) r) tree (s:path) l

    pathLength :: (Num a) => [Snake a] -> a
    pathLength = foldl (\acc s -> acc + snakeLength s) 0
      where
        snakeLength (Snake HeadNill a) = a * 2
        snakeLength (Snake _ a) = a * 2 + 1

streamTest :: Test
streamTest = testGroup "Stream"
  [ testProperty "streamToList" $ \(s :: [Int]) ->
      runIdentity (streamToList s) == s
  ]

main :: IO ()
main = defaultMain
  [ snakesTest
  , streamTest
  ]
