-- | Tests for the Quad module.
--
-- The object of these tests is not to test decNumber but, rather,
-- to test Deka to ensure there are no transposed arguments or other
-- glaring errors.  Also, ensures that the FFI binding behaves as it
-- should and that there are no side effects where there shouldn't
-- be any.
--
-- Every function that takes a Quad as an argument is tested to
-- ensure it does not modify that Quad.
--
-- encoding and decoding must also be thoroughly tested as this can
-- be quite error prone.
module DekaDir.QuadTest where

import Control.Applicative
import Control.Exception (evaluate)
import qualified Data.ByteString.Char8 as BS8
import Control.Monad
import Test.Tasty
import qualified Deka.Class.Internal as E
import qualified Deka.Quad as E
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck hiding (maxSize)
import Test.QuickCheck.Monadic
import Deka.Quad.Internal
import Deka.Context.Internal
import Deka.Decnumber.Context
import Data.Maybe
import Foreign

isLeft :: Either a b -> Bool
isLeft e = case e of { Left _ -> True; _ -> False }

isRight :: Either a b -> Bool
isRight e = case e of { Right _ -> True; _ -> False }

lenCoeff :: E.Decoded -> Maybe Int
lenCoeff dcd = fmap length . fmap E.unCoefficient
  $ case E.dValue dcd of
      E.Finite c _ -> Just c
      _ -> Nothing

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

numDigits :: (Num a, Show a) => a -> Int
numDigits = length . show . abs

increaseAbs :: E.Quad -> E.Ctx E.Quad
increaseAbs q = do
    let neg = E.isNegative q
    if neg
      then E.nextMinus q
      else E.nextPlus q

decreaseAbs :: E.Quad -> E.Ctx E.Quad
decreaseAbs q = do
  let neg = E.isNegative q
  if neg
    then E.nextPlus q
    else E.nextMinus q

-- # Generators

genSign :: Gen E.Sign
genSign = elements [ minBound..maxBound ]

genBinaryMSD :: Gen E.Digit
genBinaryMSD = return E.D1

genBinaryNonMSD :: Gen E.Digit
genBinaryNonMSD = elements [E.D0, E.D1]

binaryDigs :: (Gen E.Digit, Gen E.Digit)
binaryDigs = (genBinaryMSD, genBinaryNonMSD)

genDecimalMSD :: Gen E.Digit
genDecimalMSD = elements [ E.D1, E.D2, E.D3, E.D4, E.D5,
                           E.D6, E.D7, E.D8, E.D9 ]

genDecimalNonMSD :: Gen E.Digit
genDecimalNonMSD = elements
  [ E.D0, E.D1, E.D2, E.D3, E.D4, E.D5,
    E.D6, E.D7, E.D8, E.D9 ]

decimalDigs :: (Gen E.Digit, Gen E.Digit)
decimalDigs = (genDecimalMSD, genDecimalNonMSD)

-- | Given a length, generate a list of digits.  All lists generated
-- will be exactly the length given.
genDigits
  :: Int
  -- ^ Length
  -> (Gen E.Digit, Gen E.Digit)
  -- ^ Generate MSD, remaining digits
  -> Gen [E.Digit]
genDigits l (gm, gr) = do
  msd <- gm
  rs <- vectorOf (l - 1) gr
  return $ msd : rs

-- ## Other generators

genRound :: Gen E.Round
genRound = elements [ E.roundCeiling, E.roundUp, E.roundHalfUp,
  E.roundHalfEven, E.roundHalfDown, E.roundDown, E.roundFloor,
  E.round05Up ]

allFlags :: [E.Flag]
allFlags = [ E.divisionUndefined, E.divisionByZero,
  E.divisionImpossible, E.invalidOperation, E.inexact,
  E.underflow, E.overflow, E.conversionSyntax ]

genFlag :: Gen E.Flag
genFlag = elements allFlags

onePointFive :: E.Quad
onePointFive = E.runQuad . E.fromByteString . BS8.pack $ "1.5"

-- # Test builders

associativity
  :: String
  -- ^ Name
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
associativity n f = testProperty desc $
  forAll genSmallFinite $ \ dx ->
  forAll genSmallFinite $ \ dy ->
  forAll genSmallFinite $ \ dz ->
  let (noFlags, resIsZero) = E.runQuad $ do
        let x = E.fromBCD dx
            y = E.fromBCD dy
            z = E.fromBCD dz
        r1 <- f x y >>= f z
        r2 <- f y z >>= f x
        let c = E.runQuad $ E.compare r1 r2
            isZ = E.isZero c
        fl <- E.getStatus
        return (null fl, isZ)
  in noFlags ==> resIsZero
  where
    desc = n ++ " is associative on finite numbers"

commutativity
  :: String
  -- ^ Name
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
commutativity n f = testProperty desc $
  forAll genSmallFinite $ \dx ->
  forAll genSmallFinite $ \dy ->
  let (noFlags, resIsZero) = E.runQuad $ do
        let x = E.fromBCD dx
            y = E.fromBCD dy
        r1 <- f x y
        r2 <- f y x
        let isZ = E.compareTotal r1 r2 == EQ
        fl <- E.getStatus
        return (null fl, isZ)
  in noFlags ==> resIsZero
  where
    desc = n ++ " is commutative where there are no flags"

-- # Immutability test builders


inContext :: (Ptr C'decContext -> IO Bool) -> PropertyM IO Bool
inContext f =
  run $ alloca $ \pCtx -> do
    _ <- c'decContextDefault pCtx c'DEC_INIT_DECQUAD
    f pCtx

identity
  :: String
  -- ^ Name of thing that is identity (e.g. zero)
  -> Gen E.Decoded
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
identity n g f = testProperty name $
  forAll genFinite $ \ad ->
  forAll g $ \bd -> E.runQuad $ do
    let a = E.fromBCD ad
        b = E.fromBCD bd
    r <- f a b
    c <- E.compare a r
    return $ E.isZero c
  where
    name = n ++ " is the identity for finite numbers"

eitherToOrd :: Either E.Quad Ordering -> Ordering
eitherToOrd = either toOrd id
  where
    toOrd x | E.isNegative x = LT
            | E.isZero x = EQ
            | E.isPositive x = GT
            | otherwise = error "eitherToOrd: unrecognized value"

comparison
  :: String
  -- ^ Name of function
  -> (E.Quad -> E.Ctx E.Quad)
  -- ^ How to make a larger Quad
  -> (E.Quad -> E.Ctx E.Quad)
  -- ^ How to make a smaller Quad
  -> (E.Quad -> E.Quad -> E.Ctx (Either E.Quad Ordering))
  -> TestTree

comparison n fB fS fC = testGroup (n ++ " comparisons")
  [ testProperty "x > y" $ forAll genNonZeroSmallFinite $
    \da -> E.runQuad $ do
      let a = E.fromBCD da
      b <- fB a
      c <- fC b a
      return $ eitherToOrd c == GT

  , testProperty "x < y" $ forAll genNonZeroSmallFinite $
    \da -> E.runQuad $ do
      let a = E.fromBCD da
      b <- fS a
      c <- fC b a
      return $ eitherToOrd c == LT

  , testProperty "x == x" $ forAll genNonZeroSmallFinite $
    \da -> E.runQuad $ do
      let a = E.fromBCD da
      c <- fC a a
      return $ eitherToOrd c == EQ

  , testProperty "transitive" $ forAll genNonZeroSmallFinite $
    \da ->
    forAll genNonZeroSmallFinite $ \db -> E.runQuad $ do
      let a = E.fromBCD da
          b = E.fromBCD db
      c <- fC a b
      case eitherToOrd c of
        EQ -> do
          c' <- fC b a
          return $ eitherToOrd c' == EQ
        o -> do
          c' <- fC b a
          let cOrd = eitherToOrd c'
          return $ case cOrd of
            LT -> o == GT
            GT -> o == LT
            EQ -> False
  ]

testMinMax
  :: String
  -> Bool
  -- ^ True if testing absolute values
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
testMinMax n ab f = testProperty (n ++ " and compare") $
  forAll genSmallFinite $ \da ->
  forAll genSmallFinite $ \db -> E.runQuad $ do
    let aa = E.fromBCD da
        bb = E.fromBCD db
    (a, b) <- if ab
      then do
        aaa <- E.abs aa
        bbb <- E.abs bb
        return $ (aaa, bbb)
      else return (aa, bb)
    r <- E.compare a b
    m <- f a b
    let z = E.isZero r
    if z
      then do
        r' <- E.compare m a
        r'' <- E.compare m b
        let zr' = E.isZero r'
            zr'' = E.isZero r''
        return $ zr' && zr''
      else do
        nw <- f b a
        r' <- E.compare nw m
        return $ E.isZero r' 


decodedSameQuantum :: E.Decoded -> E.Decoded -> Bool
decodedSameQuantum x y = case (E.dValue x, E.dValue y) of
  (E.Finite _ e1, E.Finite _ e2) -> e1 == e2
  (E.Infinite, E.Infinite) -> True
  (E.NaN _ _, E.NaN _ _) -> True
  _ -> False

-- | Tests that what is returned by an operation has the same
-- exponent and sign of the first operand.
sameSignExp
  :: (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
sameSignExp f = testProperty
  "result has same sign and exponent as first argument" $
  forAll genFinite $ \d -> E.runQuad $ do
    let x = E.fromBCD d
    r <- f x E.one
    let d' = E.toBCD r
        sameExp = case (E.dValue d, E.dValue d') of
          (E.Finite _ e, E.Finite _ e') -> e == e'
          _ -> False
    return $ E.dSign d == E.dSign d' && sameExp

-- # Tests

tests :: TestTree
tests = testGroup "Quad"
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


  , testGroup "rounding"
    [ testProperty "default rounding is half even" $
      once . E.runQuad $ do
        r <- E.getRound
        return $ r == E.roundHalfEven

    , testProperty "setRound works" $
      forAll genRound $ \r -> E.runQuad $ do
        E.setRound r
        r' <- E.getRound
        return $ r == r'

    ] -- rounding

  , testGroup "flags"
    [ testProperty "no flags set initially" . once
      . E.runQuad $ do
        fl <- E.getStatus
        return $ null fl
    ]

  , testGroup "string conversions"
    [ testProperty ("Decoded -> Quad -> ByteString"
        ++ " -> Quad -> Decoded") $
      forAll genDecoded $ \d ->
        let q = E.fromBCD d
            bs = E.toByteString q
            q' = E.runQuad $ E.fromByteString bs
            d' = E.toBCD q'
            desc = "toByteString: " ++ BS8.unpack bs
              ++ " toBCD: " ++ show d'
        in printTestCase desc $ d' == d

    , testProperty ("fromBCD and (fromByteString . scientific) "
        ++ "give same result") $
      forAll genDecoded $ \d ->
      let qD = E.fromBCD d
          (qS, fl) = runFlags . E.fromByteString
                      . BS8.pack . E.scientific $ d
          compared = E.compareTotal qD qS == EQ
      in compared && null fl

    , testProperty ("fromBCD and (fromByteString . ordinary) "
        ++ "give results that compare equal") $
      forAll genDecoded $ \d ->
      let qD = E.fromBCD d
          str = E.ordinary d
          (qS, fl) = runFlags . E.fromByteString
                      . BS8.pack $ str
          cmpResult 
            | E.isNormal qD = E.compareOrd qD qS == Just EQ
            | otherwise = E.compareTotal qD qS == EQ
          noFlags f = null f
          desc = "string: " ++ str
            ++ " fromByteString result: " ++ show qS
      in noFlags fl ==> printTestCase desc cmpResult

    , testProperty "toByteString -> fromByteString" $
      forAll genDecoded $ \d ->
      let q = E.fromBCD d
          bs = E.toByteString q
          (q', fl) = runFlags . E.fromByteString $ bs
          cmpRes = E.compareTotal q q' == EQ
      in cmpRes && null fl

    , testProperty "toEngByteString -> fromByteString" $
      forAll genDecoded $ \d ->
      let q = E.fromBCD d
          bs = E.toEngByteString q
          (q', fl) = runFlags . E.fromByteString $ bs
          cmpRes = E.compareOrd q q' == Just EQ
          cmpResTot = E.compareTotal q q' == EQ
          res = if E.isFinite q then cmpRes else cmpResTot
      in null fl ==> res
    ] -- string conversions

  , testGroup "comparisons"
    [ comparison "compare" E.nextPlus E.nextMinus
        (fmap (fmap (fmap Left)) E.compare)

    , comparison "compareSignal" E.nextPlus
        E.nextMinus (fmap (fmap (fmap Left )) E.compareSignal)

    , comparison "compareTotal" E.nextPlus E.nextMinus
        (fmap (fmap (return . Right)) E.compareTotal)

    , comparison "compareTotalMag" increaseAbs decreaseAbs
          (fmap (fmap (return . Right)) E.compareTotalMag)

    , testMinMax "min" False E.min
    , testMinMax "max" False E.max
    , testMinMax "maxMag" True E.maxMag
    , testMinMax "minMag" True E.minMag

    , testGroup "sameQuantum"
      [ testProperty "is true for same Decoded" $
        forAll genDecoded $ \d ->
          let x = E.fromBCD d
          in E.sameQuantum x x

      , testProperty "is false for different Decoded" $
        forAll ( liftM2 (,) genDecoded genDecoded
                  `suchThat` (not . uncurry decodedSameQuantum))
        $ \p -> let qx = E.fromBCD . fst $ p
                    qy = E.fromBCD . snd $ p
                in not $ E.sameQuantum qx qy
      ]
    ] -- comparisons

  , testGroup "tests"
    [ testBoolean "isFinite" genFinite E.dIsFinite E.isFinite

    , testBoolean "isInfinite" (genInfinite genSign)
        E.dIsInfinite E.isInfinite

    , testGroup "isInteger"
      [ testBoolean "isInteger" genInteger E.dIsInteger E.isInteger

      , let e = fromMaybe (error "isInteger exponent failed")
              . E.exponent $ 2
            c = fromMaybe (error "isInteger coefficient failed")
              . E.coefficient $ [E.D3]
            dcd = E.Decoded E.Sign0 (E.Finite c e)
            d = E.fromBCD dcd
        in testProperty "returns False on 3 * 10 ^ 2" . once
            . not . E.isInteger $ d
      ]

    , testBoolean "isLogical" genLogical
        E.dIsLogical E.isLogical

    , testBoolean "isNaN"
      (genNaNDcd genSign genNaN (payloadDigits decimalDigs))
        E.dIsNaN E.isNaN

    , testBoolean "isNegative" genNegative
      E.dIsNegative E.isNegative

    , testBoolean "isNormal"
      (genNormal genSign (sizedDigits E.coefficientLen decimalDigs))
        E.dIsNormal E.isNormal

    , testBoolean "isPositive" genPositive
        E.dIsPositive E.isPositive

    , testBoolean "isSignaling" genSignaling
        E.dIsSignaling E.isSignaling

    , testBoolean "isSigned" genSigned
        E.dIsSigned E.isSigned

    , testBoolean "isSubnormal"
        (genSubnormal genSign (sizedDigits (E.coefficientLen - 1) decimalDigs))
        E.dIsSubnormal E.isSubnormal

    , testBoolean "isZero" genZero E.dIsZero E.isZero

    ] -- tests

  , testGroup "signs"
    [ testGroup "plus"
      [ testProperty "same as 0 + x where 0 has same exponent" $
        forAll genDecoded $ \d ->
        let e = case E.dValue d of
              E.Finite _ ex -> ex
              _ -> E.zeroExponent
            z = E.fromBCD $ E.Decoded E.Sign0
                  (E.Finite E.zeroCoefficient e)
            q = E.fromBCD d
            rAdd = E.runQuad $ E.add z q
            rPlus = E.runQuad $ E.plus q
        in E.compareTotal rAdd rPlus == EQ
      ]

    , testGroup "minus"
      [ testProperty "same as 0 - x where 0 has same exponent" $
        forAll genDecoded $ \d ->
        let e = case E.dValue d of
              E.Finite _ ex -> ex
              _ -> E.zeroExponent
            z = E.fromBCD $ E.Decoded E.Sign0
                  (E.Finite E.zeroCoefficient e)
            q = E.fromBCD d
            rSubt = E.runQuad $ E.subtract z q
            rMinus = E.runQuad $ E.minus q
        in E.compareTotal rSubt rMinus == EQ
      ]

    , testGroup "abs"
      [ testProperty "sign is correctly set" $
        forAll genDecoded $ \d ->
        let expected = case E.dValue d of
              E.Finite _ _ -> E.Sign0
              E.Infinite -> E.Sign0
              E.NaN _ _ -> E.dSign d
            q = E.fromBCD d
            actual = E.dSign . E.toBCD . E.runQuad . E.abs $ q
        in actual == expected
      ]

    , testGroup "copySign"
      [ testProperty "z is copy of x with sign of y" $
        forAll genDecoded $ \dx ->
        forAll genDecoded $ \dy ->
        let expected = dx { E.dSign = E.dSign dy }
            (x, y) = (E.fromBCD dx, E.fromBCD dy)
            r = E.toBCD $ E.copySign x y
        in r == expected
      ]
    ] -- signs

  , testGroup "increment and decrement"
    [ testProperty "nextMinus returns smaller result" $
      forAll genFinite $ \d ->
      let q = E.fromBCD d
          (r, fl) = runFlags $ E.nextMinus q
          cmp = E.runQuad $ E.compare r q
      in null fl ==> E.isNegative cmp

    , testProperty "nextPlus returns larger result" $
      forAll genFinite $ \d ->
      let q = E.fromBCD d
          (r, fl) = runFlags $ E.nextPlus q
          cmp = E.runQuad $ E.compare r q
      in null fl ==> E.isPositive cmp

    , testProperty "nextToward does not change sign of comparison" $
      forAll genFinite $ \dx ->
      forAll genFinite $ \dy ->
      let x = E.fromBCD dx
          y = E.fromBCD dy
          cmp1 = E.runQuad $ E.compare x y
          x' = E.runQuad $ E.nextToward x y
          cmp2 = E.runQuad $ E.compare x' y
          r | E.isNegative cmp1 = E.isNegative cmp2 || E.isZero cmp2
            | E.isZero cmp1 = E.isZero cmp2
            | otherwise = E.isPositive cmp2 || E.isZero cmp2
      in r

    ] -- increment and decrement

  , testGroup "digit-wise"
    [ testGroup "and"
      [ testProperty "x & 0 == 0" $
        forAll genLogical $ \d ->
        let q = E.fromBCD d
            r = E.runQuad $ E.and q E.zero
        in E.isZero r
      ]

    , testGroup "or"
      [ testProperty "x | 0 == x" $
        forAll genLogical $ \d ->
        let r = E.runQuad $ E.or x E.zero
            x = E.fromBCD d
        in E.compareOrd x r == Just EQ

      , testProperty "x | x == x" $
        forAll genLogical $ \d ->
        let r = E.runQuad $ E.or x x
            cmp = E.compareTotal r x
            x = E.fromBCD d
        in cmp == EQ
      ]

    , testGroup "xor"
      [ testProperty "x XOR 0 == x" $
        forAll genLogical $ \d ->
        let r = E.runQuad $ E.xor x E.zero
            cmp = E.compareTotal r x
            x = E.fromBCD d
        in cmp == EQ

      , testProperty "x XOR x == 0" $
        forAll genLogical $ \d ->
        let r = E.runQuad $ E.xor x x
            x = E.fromBCD d
        in E.isZero r

      ]

    , testGroup "invert"
      [ testProperty "invert twice is idempotent" $
        forAll genLogical $ \d -> E.runQuad $ do
          let q = E.fromBCD d
          r1 <- E.invert q
          r2 <- E.invert r1
          return $ E.compareOrd r2 q == Just EQ
      ]

    , testGroup "shift"
      [ sameSignExp E.shift
      ] -- shift

    , testGroup "rotate"
      [ sameSignExp E.rotate
      ]
    ] -- digit-wise

  , testGroup "log and scale"
    [ testGroup "logB"
      [ testProperty "returns adjusted exponent of finite numbers" $
        forAll genFinite $ \d -> E.runQuad $ do
          let q = E.fromBCD d
          lg <- E.logB q
          i <- E.toInt32 E.roundUp lg
          let e = fromIntegral i
              r = case E.dValue d of
                E.Finite c ex ->
                  E.unAdjustedExp (E.adjustedExp c ex) == e
                _ -> False
          return r
      ]

    , testGroup "scaleB"
      [ testProperty "scaleB x 0 == x" $
        forAll genFinite $ \d -> E.runQuad $ do
          let q = E.fromBCD d
          b <- E.scaleB q E.zero
          return $ E.compareOrd q b == Just EQ
      ]
    ] -- log and scale

  , testGroup "attributes"
    [ testGroup "digits"
      [ testProperty "gets same result as length of decoded coeff" $
        forAll genFinite $ \d ->
        let digs = E.digits . E.fromBCD $ d
        in case E.dValue d of
            E.Finite c _ -> length (E.unCoefficient c) == digs
            _ -> False
      ]
    ] -- attributes

  , testGroup "conversions"
    [ testGroup "decode and encode"
      [ testProperty "round trip from Decoded" $
        forAll genDecoded $ \d ->
        let r = E.toBCD . E.fromBCD $ d
        in printTestCase ("result: " ++ show r) (r == d)
      ]
    ] -- conversions

  ]  -- Quad
