{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Tests for the IO module.
--
-- The object of these tests is not to test decNumber but, rather,
-- to test Deka to ensure there are no transposed arguments or other
-- glaring errors.  Also, ensures that the FFI binding behaves as it
-- should and that there are no side effects where there shouldn't
-- be any.
--
-- encoding and decoding must also be thoroughly tested as this can
-- be quite error prone.
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

genExp :: Gen Int
genExp = choose E.minMaxExp

genCoeffExp :: Gen E.CoeffExp
genCoeffExp = do
  s <- genCoefficient
  e <- genExp
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

genFromDecoded :: Gen E.Quad
genFromDecoded = do
  d <- genDecoded
  return . fst . runEnv . E.encode $ d

genFinite :: Gen E.Quad
genFinite = do
  v <- liftM E.Finite genCoeffExp
  s <- genSign
  return . evalEnv . E.encode $ E.Decoded s v

genSmallFinite :: Gen Visible
genSmallFinite = do
  c <- choose (0, biggestDigs 5)
  let co = case E.coefficient c of
        Left _ -> error "genSmallFinite: coefficient failed"
        Right g -> g
  e <- choose (-10, 10)
  let ce = case E.coeffExp co e of
        Left _ -> error "genSmallFinite: coeffExp failed"
        Right g -> g
  s <- genSign
  let d = E.Decoded s (E.Finite ce)
      dec = evalEnv (E.encode d)
  return . Visible $ dec

genRound :: Gen E.Round
genRound = elements [ E.roundCeiling, E.roundUp, E.roundHalfUp,
  E.roundHalfEven, E.roundHalfDown, E.roundDown, E.roundFloor,
  E.round05Up, E.roundMax ]

newtype Visible = Visible { unVisible :: E.Quad }

instance Show Visible where
  show = BS8.unpack . evalEnv . E.toString . unVisible

instance Arbitrary Visible where
  arbitrary = fmap Visible genFromDecoded

-- # Test builders

associativity
  :: String
  -- ^ Name
  -> (E.Quad -> E.Quad -> E.Env E.Quad)
  -> TestTree
associativity n f = testProperty desc $
  forAll genSmallFinite $ \(Visible x) ->
  forAll genSmallFinite $ \(Visible y) ->
  forAll genSmallFinite $ \(Visible z) ->
  let (noFlags, resIsZero) = evalEnv $ do
        r1 <- f x y >>= f z
        r2 <- f y z >>= f x
        c <- E.compareTotal r1 r2
        isZ <- E.isZero c
        fl <- E.getStatus
        return (fl == E.emptyFlags, isZ)
  in noFlags ==> resIsZero
  where
    desc = n ++ " is associative on finite numbers"

commutativity
  :: String
  -- ^ Name
  -> (E.Quad -> E.Quad -> E.Env E.Quad)
  -> TestTree
commutativity n f = testProperty desc $
  forAll genSmallFinite $ \(Visible x) ->
  forAll genSmallFinite $ \(Visible y) ->
  let (noFlags, resIsZero) = evalEnv $ do
        r1 <- f x y
        r2 <- f y x
        c <- E.compareTotal r1 r2
        isZ <- E.isZero c
        fl <- E.getStatus
        return (fl == E.emptyFlags, isZ)
  in noFlags ==> resIsZero
  where
    desc = n ++ " is commutative where there are no flags"

imuUni
  :: String
  -- ^ Name
  -> (E.Quad -> E.Env a)
  -> TestTree
imuUni n f = testProperty desc $ \(Visible d) -> evalEnv $ do
  dcd1 <- E.decode d
  _ <- f d
  dcd2 <- E.decode d
  return $ dcd1 == dcd2
  where
    desc = n ++ " (unary function) does not mutate only argument"

imuBinary
  :: String
  -> (E.Quad -> E.Quad -> E.Env a)
  -> TestTree
imuBinary n f = testGroup ("immutability - " ++ n)
  [ imuBinary1st n (arbitrary, unVisible) f
  , imuBinary2nd n (arbitrary, unVisible) f
  ]

imuBinary1st
  :: Show a
  => String
  -- ^ Name
  -> (Gen a, a -> c)
  -> (E.Quad -> c -> E.Env b)
  -> TestTree
imuBinary1st n (genA, getC) f = testProperty desc $
  forAll arbitrary $ \(Visible d) ->
  forAll genA $ \a -> evalEnv $ do
    dcd1 <- E.decode d
    _ <- f d (getC a)
    dcd2 <- E.decode d
    return $ dcd1 == dcd2
  where
    desc = n ++ " (binary function) does not mutate first argument"

imuBinary2nd
  :: Show a
  => String
  -- ^ Name
  -> (Gen a, a -> c)
  -> (c -> E.Quad -> E.Env b)
  -> TestTree
imuBinary2nd n (genA, getC) f = testProperty desc $
  forAll arbitrary $ \(Visible d) ->
  forAll genA $ \a -> evalEnv $ do
    dcd1 <- E.decode d
    _ <- f (getC a) d
    dcd2 <- E.decode d
    return $ dcd1 == dcd2
  where
    desc = n ++ " (binary function) does not mutate second argument"

imuTernary
  :: String
  -> (E.Quad -> E.Quad -> E.Quad -> E.Env a)
  -> TestTree
imuTernary n f = testGroup (n ++ " (ternary function) - immutability")
  [ testProperty "first argument" $
    \(Visible a) (Visible b) (Visible c) -> evalEnv $ do
      dcd1 <- E.decode a
      _ <- f a b c
      dcd2 <- E.decode a
      return $ dcd1 == dcd2

  , testProperty "second argument" $
    \(Visible a) (Visible b) (Visible c) -> evalEnv $ do
      dcd1 <- E.decode b
      _ <- f a b c
      dcd2 <- E.decode b
      return $ dcd1 == dcd2

  , testProperty "third argument" $
    \(Visible a) (Visible b) (Visible c) -> evalEnv $ do
      dcd1 <- E.decode c
      _ <- f a b c
      dcd2 <- E.decode c
      return $ dcd1 == dcd2
  ]

-- # Tests

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

  , testGroup "immutability"
    [ testGroup "conversions"
      [ imuUni "decClass" E.decClass
      , imuUni "decode" E.decode
      , imuUni "toString" E.toString
      , imuUni "toEngString" E.toEngString
      , imuBinary2nd "toInt32" (genRound, id) E.toInt32
      , imuBinary2nd "toInt32Exact" (genRound, id) E.toInt32Exact
      , imuBinary2nd "toUInt32" (genRound, id) E.toUInt32
      , imuBinary2nd "toUInt32Exact" (genRound, id) E.toUInt32Exact
      , imuUni "toIntegralExact" E.toIntegralExact
      , imuBinary2nd "toIntegralValue" (genRound, id) E.toIntegralValue
      ]

    , testGroup "arithmetic"
      [ imuBinary "add" E.add
      , imuBinary "subtract" E.subtract
      , imuBinary "multiply" E.multiply
      , imuTernary "fma" E.fma
      , imuBinary "divide" E.divide
      , imuBinary "divideInteger" E.divideInteger
      , imuBinary "remainder" E.remainder
      , imuBinary "remainderNear" E.remainderNear
      ]

    , testGroup "exponent and coefficient adjustment"
      [ imuBinary "quantize" E.quantize
      , imuUni "reduce" E.reduce
      ]

    , testGroup "comparisons"
      [ imuBinary "compare" E.compare
      , imuBinary "compareSignal" E.compareSignal
      , imuBinary "compareTotal" E.compareTotal
      , imuBinary "compareTotalMag" E.compareTotalMag
      , imuBinary "max" E.max
      , imuBinary "maxMag" E.maxMag
      , imuBinary "min" E.min
      , imuBinary "minMag" E.minMag
      , imuBinary "sameQuantum" E.sameQuantum
      ]

    , let f = imuUni in
      testGroup "tests"
      [ f "isFinite" E.isFinite
      , f "isInfinite" E.isInfinite
      , f "isInteger" E.isInteger
      , f "isLogical" E.isLogical
      , f "isNaN" E.isNaN
      , f "isNegative" E.isNegative
      , f "isNormal" E.isNormal
      , f "isPositive" E.isPositive
      , f "isSignaling" E.isSignaling
      , f "isSigned" E.isSigned
      , f "isSubnormal" E.isSubnormal
      , f "isZero" E.isZero
      ]

    , testGroup "signs"
      [ imuUni "plus" E.plus
      , imuUni "minus" E.minus
      , imuUni "abs" E.abs
      , imuBinary "copySign" E.copySign
      ]

    , testGroup "increment and decrement"
      [ imuUni "nextMinus" E.nextMinus
      , imuUni "nextPlus" E.nextPlus
      , imuBinary "nextToward" E.nextToward
      ]

    , testGroup "logical, bitwise, digit shifting"
      [ imuBinary "and" E.and
      , imuBinary "or" E.or
      , imuBinary "shift" E.shift
      , imuBinary "xor" E.xor
      , imuBinary "rotate" E.rotate
      , imuUni "invert" E.invert
      ]

    , testGroup "transcendental"
      [ imuUni "logB" E.logB
      , imuBinary "scaleB" E.scaleB
      ]

    , testGroup "attributes"
      [ imuUni "digits" E.digits
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
        forAll genCoefficient $ \c ->
        let (l, _) = E.minMaxExp in
        forAll (choose (minBound, l - 1)) $ \e ->
        isLeft $ E.coeffExp c e

      , testProperty "fails when exponent is too large" $
        forAll genCoefficient $ \c ->
          let (_, h) = E.minMaxExp
          in forAll (choose (h + 1, maxBound)) $ \e ->
          isLeft $ E.coeffExp c e

      , testProperty "fails when exponent is < c'DECQUAD_Emin" $
        forAll genCoefficient $ \c ->
        forAll (choose (minBound, c'DECQUAD_Emin - 1)) $ \e ->
        isLeft $ E.coeffExp c e

      , testProperty "fails when exponent is > c'DECQUAD_Emax" $
        forAll genCoefficient $ \c ->
        forAll (choose (c'DECQUAD_Emax + 1, maxBound)) $ \e ->
        isLeft $ E.coeffExp c e

      , testProperty "succeeds when it should" $
        forAll genCoefficient $ \c ->
        forAll (choose E.minMaxExp) $ \e ->
        isRight $ E.coeffExp c e
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
        [ testProperty "round trip from Quad" $
          forAll (fmap Blind genFromDecoded) $ \(Blind d) ->
          evalEnv $ do
            dcd <- E.decode d
            ecd <- E.encode dcd
            r <- E.compareTotal ecd d
            E.isZero r

        , testProperty "round trip from Decoded" $
          forAll genDecoded $ \d ->
          let r = evalEnv $ do
                ecd <- E.encode d
                E.decode ecd
          in printTestCase ("result: " ++ show r) (r == d)
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
