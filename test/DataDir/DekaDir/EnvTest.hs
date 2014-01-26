{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir.DekaDir.EnvTest where

import Control.Monad
import Test.Tasty
import Data.Maybe
import qualified Data.Deka.Env as E
import Data.Deka.Pure (runEnvPure, evalEnvPure)
import Data.Deka.Decnumber
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen

-- | The largest number with the given number of digits.
biggestDigs :: Int -> Integer
biggestDigs i = 10 ^ i - 1

-- | The smallest positive number with the given number of digits.
smallestDigs :: Int -> Integer
smallestDigs i = 10 ^ (i - 1)

maxSize :: Int -> Gen a -> Gen a
maxSize s g = sized $ \o -> resize (min o s) g

numDigits :: Integer -> Int
numDigits i = if i == 0 then 1 else go (abs i, 0)
  where
    go (left, tot) =
      if left == 0
      then tot
      else go (left `div` 10, tot + 1)

genSign :: Gen E.Sign
genSign = elements [ E.Positive, E.Negative ]

genNaN :: Gen E.NaN
genNaN = elements [ E.Quiet, E.Signaling ]

genSignificand :: Gen E.Significand
genSignificand = do
  i <- choose (0, biggestDigs c'DECQUAD_Pmax)
  case E.significand i of
    Nothing -> error "genSignificand failed"
    Just r -> return r

genPayload :: Gen E.Payload
genPayload = do
  i <- choose (0, biggestDigs (c'DECQUAD_Pmax - 1))
  case E.payload i of
    Nothing -> error "genPayload failed"
    Just r -> return r

genExponent :: Gen E.Exponent
genExponent = do
  i <- choose (c'DECQUAD_Emin, c'DECQUAD_Emax)
  case E.exponent i of
    Nothing -> error "genExponent failed"
    Just r -> return r

genValue :: Gen E.Value
genValue = oneof
  [ liftM2 E.Finite genSignificand genExponent
  , return E.Infinite
  , liftM2 E.NaN genNaN genPayload
  ]

genDecoded :: Gen E.Decoded
genDecoded = liftM2 E.Decoded genSign genValue

genFromDecoded :: Gen E.Dec
genFromDecoded = do
  d <- genDecoded
  return . fst . runEnvPure . E.encode $ d


tests = testGroup "Env"
  [ testGroup "helper functions"
    [ testGroup "biggestDigs"
      [ testProperty "generates correct number of digits" $
        forAll (choose (1, 500)) $ \i ->
        numDigits (biggestDigs i) == i

      , testProperty "adding one increases number of digits" $
        forAll (choose (1, 500)) $ \i ->
        let r = biggestDigs i
            n = numDigits r
            n' = numDigits (r + 1)
        in n' == n + 1
      ]

      , testGroup "smallestDigs"
        [ testProperty "generates correct number of digits" $
          forAll (choose (1, 500)) $ \i ->
          numDigits (smallestDigs i) == i

        , testProperty "subtracting one decreases number of digits" $
          forAll (choose (1, 500)) $ \i ->
          let r = smallestDigs i
          in r > 1 ==> numDigits r - 1 == numDigits (r - 1)
        ]
    ]
  
  , testGroup "conversions"
    [ testGroup "Significand"
      [ testGroup "significand"
        [ testProperty "fails on negative numbers" $
          isNothing . E.significand . negate . getPositive

        , testProperty "fails when number of digits > Pmax" $
          isNothing . E.significand . smallestDigs $ c'DECQUAD_Pmax + 1
        ]

      , testGroup "Payload"
        [ testGroup "payload"
          [ testProperty "fails on negative numbers" $
            isNothing . E.significand . negate . getPositive

          , testProperty "succeeds when number of digits <= Pmax - 1" $
            isJust . E.significand . smallestDigs $ c'DECQUAD_Pmax - 1
          ]
        ]

      , testGroup "Exponent"
        [ testGroup "exponent"
          [ testProperty "fails when exponent < c'DECFLOAT_Emin" $
            isNothing . E.exponent $ c'DECQUAD_Emin - 1

          , testProperty "fails when exponent > c'DECFLOAT_Emax" $
            isNothing . E.exponent $ c'DECQUAD_Emax + 1

          , testProperty "succeeds when exponent between min and max" $
            forAll (choose (c'DECQUAD_Emin, c'DECQUAD_Emax)) $ \i ->
            isJust . E.exponent $ i
          ]
        ]

      , testGroup "decode and encode"
        [ testProperty "round trip from Dec" $
          forAll (fmap Blind genFromDecoded) $ \(Blind d) ->
          evalEnvPure $ do
            dcd <- E.decode d
            ecd <- E.encode dcd
            r <- E.compareTotal ecd d
            E.isZero r

        , testProperty "round trip from Decoded" $
          forAll genDecoded $ \d ->
          evalEnvPure $ do
            ecd <- E.encode d
            dcd <- E.decode ecd
            return $ dcd == d
        ]
      ]
    ]
  ]
