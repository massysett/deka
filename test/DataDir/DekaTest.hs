{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir.DekaTest where

import Data.Maybe
import Control.Exception
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as Q
import DataDir.DekaDir.QuadTest
import Data.Deka.Quad
import Data.Deka
import qualified Data.ByteString.Char8 as BS8

-- | Tests that a binary operator never produces non-finite values.
noNonFinite
  :: String
  -- ^ Name
  -> (Deka -> Deka -> Deka)
  -> TestTree
noNonFinite n f = testProperty
  (n ++ " does not produce non-finite values") prop
  where
    prop =
      forAll genFinite $ \d1 ->
      forAll genFinite $ \d2 ->
      monadicIO $ do
        mayR <- run (doCalc d1 d2)
        case mayR of
          Nothing -> Q.assert True
          Just r -> Q.assert . isFinite . unDeka $ r
    doCalc x y =
      let (xD, yD) = (toDeka x, toDeka y)
          outer = do
            r <- evaluate $ f xD yD
            return . Just $ r
          catcher e = let _types = e :: DekaError in return Nothing
      in Control.Exception.catch outer catcher

-- | Puts finite Quad into a Deka.  Calls "error" if it fails.

toDeka :: Decoded -> Deka
toDeka = fromMaybe (error "toDeka failed") . quadToDeka . fromBCD

tests = testGroup "Deka"
  [ testGroup "integralToDeka"
    [ testProperty "succeeds when <= Pmax digits" $
      let r = (negate i, i)
          i = biggestDigs coefficientLen
      in forAll (choose r) $ \int -> isJust (integralToDeka int)
    ]

  , testGroup "strToDeka"
    [ testProperty "fails on non-finite strings; succeeds on finites" $
      forAll genDecoded $ \d ->
      let r = strToDeka . BS8.unpack . toByteString . fromBCD $ d
      in case dValue d of
          Finite _ _ -> isJust r
          _ -> isNothing r
    ]

  , testGroup "quadToDeka"
    [ testProperty "fails and succeeds as it should" $
      forAll genDecoded $ \d ->
      let r = quadToDeka $ fromBCD d
      in if dIsFinite d then isJust r else isNothing r
    ]
  
  , testGroup "Deka"
    [ testProperty "equivalent Deka are Eq" $
      forAll genEquivalent $ \(d1, d2) ->
      let (q1, q2) = (toDeka d1, toDeka d2)
      in q1 == q2

    , noNonFinite "+" (+)
    , noNonFinite "-" (-)
    , noNonFinite "*" (*)
    ]
  ]
