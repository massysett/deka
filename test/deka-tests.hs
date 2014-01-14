module Main where

import Control.Monad
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Deka

newtype DekaA = DekaA { unDekaA :: Deka }
  deriving (Eq, Ord, Show)

instance Arbitrary DekaA where
  arbitrary = fmap DekaA $ liftM2 Deka arbitrary arbitrary

newtype ZeroA = ZeroA { unZeroA :: Deka }
  deriving (Eq, Ord, Show)

instance Arbitrary ZeroA where
  arbitrary = fmap ZeroA $ liftM2 Deka (return 0) arbitrary

newtype OneA = OneA { unOneA :: Deka }
  deriving (Eq, Ord, Show)

instance Arbitrary OneA where
  arbitrary = do
    NonNegative e <- arbitrary
    return . OneA $ Deka (1 * 10 ^ e) (negate e)

main :: IO ()
main = defaultMain $ testGroup "Deka" tests

isAssociative
  :: (Arbitrary b, Show b, Eq a)
  => (b -> a)
  -> (a -> a -> a)
  -> TestTree
isAssociative u f = testProperty "is associative" $
  \wx wy wz ->
  let (x, y, z) = (u wx, u wy, u wz)
  in (x `f` (y `f` z)) == ((x `f` y) `f` z)

isCommutative
  :: (Arbitrary b, Show b, Eq a)
  => (b -> a)
  -> (a -> a -> a)
  -> TestTree
isCommutative u f = testProperty "is commutative" $
  \wx wy ->
  let (x, y) = (u wx, u wy)
  in f x y == f y x

tests :: [TestTree]
tests =
  [ testGroup "Eq instance"
    [ testProperty "Eq if coef and expt are the same" $
      \c e -> Deka c e == Deka c e

    , testProperty "Not Eq if coef are different" $
      \c1 c2 e -> c1 /= c2 ==> Deka c1 e /= Deka c2 e

    , testProperty "Not Eq if expt are different" $
      \c e1 e2 -> e1 /= e2 ==> Deka c e1 /= Deka c e2
    ]
  
  , testGroup "equalizeExponents"
    [ testProperty "gives equal exponents" $
      \(DekaA x) (DekaA y) ->
      let (x', y') = equalizeExponents x y
      in expt x' == expt y'

    , testGroup "never reduces the absolute value of a coefficient"
      [ testProperty "left" $
        \(DekaA x) (DekaA y) ->
        let (x', _) = equalizeExponents x y
        in abs (coef x') >= abs (coef x)

      , testProperty "right" $
        \(DekaA x) (DekaA y) ->
        let (_, y') = equalizeExponents x y
        in abs (coef y') >= abs (coef y)
      ]

    , testGroup "never increases an exponent"
      [ testProperty "left" $
        \(DekaA x) (DekaA y) ->
        let (x', _) = equalizeExponents x y
        in expt x' <= expt x

      , testProperty "right" $
        \(DekaA x) (DekaA y) ->
        let (_, y') = equalizeExponents x y
        in expt y' <= expt y
      ]

    , testProperty "only changes one of the operands" $
      \(DekaA x) (DekaA y) ->
      let (x', y') = equalizeExponents x y
      in x' == x || y' == y

    , testGroup "division of changed coefficient by old one"
      [ testProperty "left" $
        \(DekaA x) (DekaA y) ->
        let (x', _) = equalizeExponents x y
        in coef x' /= coef x ==>
           ((== 0) . (`rem` 10) $ (coef x' `quot` coef x))

      , testProperty "right" $
        \(DekaA x) (DekaA y) ->
        let (_, y') = equalizeExponents x y
        in coef y' /= coef y ==>
           ((== 0) . (`rem` 10) $ (coef y' `quot` coef y))
      ]
    ]
 
  , testGroup "compareValues"
    [ testProperty ("is same as comparing coefficients"
                    ++ " when exponents are equal") $
      \s1 s2 e ->
      compare s1 s2 == compareValues (Deka s1 e) (Deka s2 e)

    , testProperty "is always EQ for zeroes" $
      \e1 e2 -> compareValues (Deka 0 e1) (Deka 0 e2) == EQ
    ]

  , testGroup "equivalent"
    [ testProperty ("is same as comparing coefficients"
                    ++ " when exponents are equal") $
      \s1 s2 e ->
      (s1 == s2) == equivalent (Deka s1 e) (Deka s2 e)

    , testProperty "is always true for zeroes" $
      \e1 e2 -> equivalent (Deka 0 e1) (Deka 0 e2)

    , testProperty "is True when compareValues is EQ" $
      \(DekaA x) (DekaA y) ->
      let eq = equivalent x y
          cmp = compareValues x y
      in if eq then cmp == EQ else cmp /= EQ
    ]

  , testGroup "addition"
    [ isAssociative unDekaA (+)
    , isCommutative unDekaA (+)
    , testProperty "zero is the identity" $
    \(DekaA x) (ZeroA z) -> x + z ==~ x
    ]

  , testGroup "subtraction"
    [ testProperty "(x + y - y) `equivalent` x" $
      \(DekaA x) (DekaA y) ->
      (x + y - y) ==~ x

    , testProperty "zero is the identity" $
      \(DekaA x) (ZeroA z) -> x - z ==~  x

    , testProperty "zero - x is the same as negation" $
      \(DekaA x) (ZeroA z) -> z - x ==~ negate x
    ]

  , testGroup "multiplication"
    [ isAssociative unDekaA (*)
    , isCommutative unDekaA (*)

    , testProperty "one is the identity" $
      \(DekaA x) (OneA o) -> x * o ==~ x

    , testProperty "x * 0 ==~ 0" $
      \(DekaA x) (ZeroA z) -> x * z ==~ z
    ]
  ]
