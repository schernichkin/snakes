{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Functor.Identity
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

snakesTest :: Test
snakesTest = testGroup "Snakes"
  [ slideDownTest
  , reflectHeadTest
  , reflectSnakeTest
  , reflectStateTest
  , snakeLeftTest
  , snakeRightTest
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
    reflectHeadTest = testGroup "reflectHead"
      [ testCase "reflect nil" $
          HeadNill @=? reflectHead (HeadNill :: SnakeHead Int)
      , testProperty "reflect left" $ \(s :: Snake Int) ->
          reflectHead (HeadLeft s) == HeadRight s
      , testProperty "reflect right" $ \(s :: Snake Int) ->
          reflectHead (HeadRight s) == HeadLeft s
      , testProperty "reflect either" $ \(s1 :: Snake Int) (s2 :: Snake Int) ->
          reflectHead (HeadEither s1 s2) == HeadEither s2 s1
      , testProperty "reflect . reflect == id" $ \(h :: SnakeHead Int) ->
          (reflectHead . reflectHead) h == h
      ]
    reflectSnakeTest = testGroup "reflectSnake"
      [ testProperty "reflect . reflect == id" $ \(s :: Snake Int) ->
          (reflectSnake . reflectSnake) s == s
      ]
    reflectStateTest = testGroup "reflectState"
      [ testProperty "swap streams" $ \(s@(SnakeState _ _ ls rs) :: SnakeState Int [Int]) ->
          let SnakeState _ _ ls' rs' = reflectState s
          in rs' == ls && ls' == rs
      , testProperty "reflect . reflect == id" $ \(s :: SnakeState Int [Int]) ->
          (reflectState . reflectState) s == s
      ]
    snakeLeftTest = testGroup "snakeLeft"
      [ testProperty "always left" $ \(s :: SnakeState Int [Int]) ->
          let res = runIdentity $ snakeLeft s
          in case res of
            Just (Left (SnakeState (Snake (HeadLeft _) _) _ _ _)) -> True
            Just (Right (Snake (HeadLeft _) _)) -> True
            Nothing -> True
            _ -> False
      , testProperty "point to prev snake" $ \(s@(SnakeState prevSnake _ _ _) :: SnakeState Int [Int]) ->
          let res = runIdentity $ snakeLeft s
          in case res of
            Just (Left (SnakeState (Snake (HeadLeft snake) _) _ _ _)) -> snake == prevSnake
            _ -> True
      , testProperty "consume streams correctly" $ \(s@(SnakeState _ _ ls rs) :: SnakeState Int [Int]) ->
          let res = runIdentity $ snakeLeft s
          in case res of
            Just (Left (SnakeState (Snake _ a) _ ls' rs')) -> take a (tail ls) == take a rs
                                                             && drop (a + 1) ls == ls'
                                                             && drop a rs == rs'
            Just (Right (Snake _ a)) -> length ls == (a + 1)
                                     && length rs == a
                                     && tail ls == rs
            Nothing -> null ls
      ]
    snakeRightTest = testGroup "snakeRight"
      [ testProperty "always right" $ \(s :: SnakeState Int [Int]) ->
          let res = runIdentity $ snakeRight s
          in case res of
            Just (Left (SnakeState (Snake (HeadRight _) _) _ _ _)) -> True
            Just (Right (Snake (HeadRight _) _)) -> True
            Nothing -> True
            _ -> False
      , testProperty "point to prev snake" $ \(s@(SnakeState prevSnake _ _ _) :: SnakeState Int [Int]) ->
          let res = runIdentity $ snakeRight s
          in case res of
            Just (Left (SnakeState (Snake (HeadRight snake) _) _ _ _)) -> snake == prevSnake
            _ -> True
      , testProperty "consume streams correctly" $ \(s@(SnakeState _ _ ls rs) :: SnakeState Int [Int]) ->
          let res = runIdentity $ snakeRight s
          in case res of
            Just (Left (SnakeState (Snake _ a) _ ls' rs')) -> take a (tail rs) == take a ls
                                                             && drop (a + 1) rs == rs'
                                                             && drop a ls == ls'
            Just (Right (Snake _ a)) -> length rs == (a + 1)
                                     && length ls == a
                                     && tail rs == ls
            Nothing -> null rs
      ]

main :: IO ()
main = defaultMain
  [ snakesTest
  ]
