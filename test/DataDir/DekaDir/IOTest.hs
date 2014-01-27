{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir.DekaDir.IOTest where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad
import Test.Tasty
import Data.Maybe
import qualified Data.Deka.IO as E
import Data.Deka.Pure (runEnv, evalEnv)
import Data.Deka.Decnumber
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen

isLeft :: Either a b -> Bool
isLeft e = case e of { Left _ -> True; _ -> False }

isRight :: Either a b -> Bool
isRight e = case e of { Right _ -> True; _ -> False }

-- | Maximum Integer for testing purposes.
maxInteger :: Integer
maxInteger = 10 ^ (100 :: Int)

-- | Minimum Integer for testing purposes.
minInteger :: Integer
minInteger = negate (10 ^ (100 :: Int))

-- | The largest number with the given number of digits.
biggestDigs :: Int -> Integer
biggestDigs i = 10 ^ i - 1

-- | The smallest positive number with the given number of digits.
smallestDigs :: Int -> Integer
smallestDigs i = 10 ^ (i - 1)

maxSize :: Int -> Gen a -> Gen a
maxSize s g = sized $ \o -> resize (min o s) g

numDigits :: (Num a, Show a) => a -> Int
numDigits = length . show . abs

-- # Generators

genSign :: Gen E.Sign
genSign = elements [ E.Positive, E.Negative ]

genNaN :: Gen E.NaN
genNaN = elements [ E.Quiet, E.Signaling ]

genCoefficient :: Gen E.Coefficient
genCoefficient = do
  i <- choose (0, biggestDigs c'DECQUAD_Pmax)
  case E.coefficient i of
    Left _ -> error "genCoefficient failed"
    Right g -> return g

genExp
  :: E.Coefficient
  -- ^ Significand
  -> Gen Int
genExp = choose . E.minMaxExp

genCoeffExp :: Gen E.CoeffExp
genCoeffExp = do
  s <- genCoefficient
  e <- genExp s
  case E.coeffExp s e of
    Left _ -> error "genCoeffExp failed"
    Right g -> return g

genPayload :: Gen E.Payload
genPayload = do
  i <- choose (0, biggestDigs (c'DECQUAD_Pmax - 1))
  case E.payload i of
    Nothing -> error "genPayload failed"
    Just r -> return r

genValue :: Gen E.Value
genValue = oneof
  [ liftM E.Finite genCoeffExp
  , return E.Infinite
  , liftM2 E.NaN genNaN genPayload
  ]

genDecoded :: Gen E.Decoded
genDecoded = liftM2 E.Decoded genSign genValue

genFromDecoded :: Gen E.Dec
genFromDecoded = do
  d <- genDecoded
  return . fst . runEnv . E.encode $ d

genFinite :: Gen E.Dec
genFinite = do
  v <- liftM E.Finite genCoeffExp
  s <- genSign
  return . evalEnv . E.encode $ E.Decoded s v

newtype Visible = Visible { unVisible :: E.Dec }

instance Show Visible where
  show = BS8.unpack . evalEnv . E.toString . unVisible

-- # Test builders

associativity
  :: String
  -- ^ Name
  -> (E.Dec -> E.Dec -> E.Env E.Dec)
  -> TestTree
associativity n f = testProperty desc $
  forAll (fmap Visible genFinite) $ \(Visible x) ->
  forAll (fmap Visible genFinite) $ \(Visible y) ->
  forAll (fmap Visible genFinite) $ \(Visible z) -> evalEnv $ do
    r1 <- f x y >>= f z
    r2 <- f y z >>= f x
    c <- E.compareTotal r1 r2
    E.isZero c
  where
    desc = n ++ " is associative on finite numbers"

commutativity
  :: String
  -- ^ Name
  -> (E.Dec -> E.Dec -> E.Env E.Dec)
  -> TestTree
commutativity n f = testProperty desc $
  forAll (fmap Visible genFinite) $ \(Visible x) ->
  forAll (fmap Visible genFinite) $ \(Visible y) -> evalEnv $ do
    r1 <- f x y
    r2 <- f y x
    c <- E.compareTotal r1 r2
    E.isZero c
  where
    desc = n ++ " is commutative on finite numbers"

tests = testGroup "IO"
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
    [ testGroup "coefficient"
      [ testProperty "fails on negative integers" $
        forAll (choose (minInteger, (-1))) $
        isLeft . E.coefficient

      , testProperty "fails when num digits > Pmax" $
        forAll (choose (smallestDigs c'DECQUAD_Pmax, maxInteger)) $
        isLeft . E.coefficient

      , testProperty "succeeds when it should" $
        forAll (choose (0, smallestDigs c'DECQUAD_Pmax - 1)) $
        isRight . E.coefficient
      ]

    , testGroup "coeffExp"
      [ testProperty "fails when exponent is too small" $
        forAll genCoefficient $ \c -> do
          let (l, _) = E.minMaxExp c
          e <- choose (minBound, l - 1)
          return . isLeft $ E.coeffExp c e

      , testProperty "fails when exponent is too large" $
        forAll genCoefficient $ \c -> do
          let (_, h) = E.minMaxExp c
          e <- choose (h + 1, maxBound)
          return . isLeft $ E.coeffExp c e

      , testProperty "fails when exponent is < c'DECQUAD_Emin" $
        forAll genCoefficient $ \c ->
        forAll (choose (minBound, c'DECQUAD_Emin - 1)) $ \e ->
        isLeft $ E.coeffExp c e

      , testProperty "fails when exponent is > c'DECQUAD_Emax" $
        forAll genCoefficient $ \c ->
        forAll (choose (c'DECQUAD_Emax + 1, maxBound)) $ \e ->
        isLeft $ E.coeffExp c e

      , testProperty "succeeds when it should" $
        forAll genCoefficient $ \c -> do
          e <- choose . E.minMaxExp $ c
          return . isRight $ E.coeffExp c e
      ]

      , testGroup "payload"
        [ testProperty "fails on negative numbers" $
          isNothing . E.payload . negate . getPositive

        , testProperty "succeeds when number of digits <= Pmax - 1" $
          isJust . E.payload . smallestDigs $ c'DECQUAD_Pmax - 1

        , testProperty "fails when number of digits > Pmax - 1" $
          forAll (choose (smallestDigs (c'DECQUAD_Pmax - 1), maxInteger)) $
          isNothing . E.payload
        ]

      , testGroup "decode and encode"
        [ testProperty "round trip from Dec" $
          forAll (fmap Blind genFromDecoded) $ \(Blind d) ->
          evalEnv $ do
            dcd <- E.decode d
            ecd <- E.encode dcd
            r <- E.compareTotal ecd d
            E.isZero r

        , testProperty "round trip from Decoded" $
          forAll genDecoded $ \d ->
          evalEnv $ do
            ecd <- E.encode d
            dcd <- E.decode ecd
            return $ dcd == d
        ]
      ]

    , testGroup "arithmetic"
      [ testGroup "add"
        [ associativity "add" E.add
        , commutativity "add" E.add
        ]

      , testGroup "multiply"
        [ associativity "multiply" E.multiply
        , commutativity "multiply" E.multiply
        ]
      ]
    ]
