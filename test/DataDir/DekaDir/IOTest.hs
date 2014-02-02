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

import Control.Applicative
import qualified Data.ByteString.Char8 as BS8
import Control.Monad
import Test.Tasty
import qualified Data.Deka.IO as E
import Data.Deka.Pure (runCtx, evalCtx, liftEnv, runEnv)
import Data.Deka.Decnumber
import Test.Tasty.QuickCheck
import Test.QuickCheck

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

increaseAbs :: E.Quad -> E.Ctx E.Quad
increaseAbs q = do
    neg <- liftEnv $ E.isNegative q
    if neg
      then E.nextMinus q
      else E.nextPlus q

decreaseAbs :: E.Quad -> E.Ctx E.Quad
decreaseAbs q = do
  neg <- liftEnv $ E.isNegative q
  if neg
    then E.nextPlus q
    else E.nextMinus q

-- # Generators

genNaN :: Gen E.NaN
genNaN = elements [ E.Quiet, E.Signaling ]

genDigit :: Gen E.Digit
genDigit = elements [minBound..maxBound]

genCoefficient :: Gen E.Coefficient
genCoefficient = sized

genFiniteDigits :: Gen E.FiniteDigits
genFiniteDigits = do
  ds <- vectorOf E.finiteDigitsLen genDigit
  case E.finiteDigits ds of
    Nothing -> error "genFiniteDigits failed"
    Just r -> return r

genLogicalFiniteDigits :: Gen E.FiniteDigits
genLogicalFiniteDigits = do
  let g = elements [ E.D0, E.D1 ]
  ds <- vectorOf E.finiteDigitsLen g
  case E.finiteDigits ds of
    Nothing -> error "genLogicalFiniteDigits failed"
    Just r -> return r

genCoeffDigits :: Gen E.CoeffDigits
genCoeffDigits = do
  ds <- vectorOf E.coeffDigitsLen genDigit
  case E.coeffDigits ds of
    Nothing -> error "genCoeffDigits failed"
    Just r -> return r

genPayloadDigits :: Gen E.PayloadDigits
genPayloadDigits = do
  ds <- vectorOf E.payloadDigitsLen genDigit
  case E.payloadDigits ds of
    Nothing -> error "genPayloadDigits failed"
    Just r -> return r

genFiniteExp :: Gen E.FiniteExp
genFiniteExp = do
  i <- choose E.minMaxExp
  case E.finiteExp i of
    Left _ -> error "genFiniteExp failed"
    Right r -> return r

genSign :: Gen E.Sign
genSign = elements [ minBound..maxBound ]

genValue :: Gen E.Value
genValue = oneof
  [ liftM2 E.Finite genFiniteDigits genFiniteExp
  , return E.Infinite
  , liftM2 E.NaN genNaN genPayloadDigits
  ]

genDecoded :: Gen E.Decoded
genDecoded = liftM2 E.Decoded genSign genValue

genFromDecoded :: Gen E.Quad
genFromDecoded = do
  d <- genDecoded
  return . fst . runCtx . liftEnv . E.fromBCD $ d

genFinite :: Gen Visible
genFinite = do
  v <- liftM2 E.Finite genFiniteDigits genFiniteExp
  s <- genSign
  return . Visible . evalCtx . liftEnv . E.fromBCD $ E.Decoded s v

mkDigits :: Integer -> (E.Sign, [E.Digit])
mkDigits p = (s, ds)
  where
    s = if p < 0 then E.Sign1 else E.Sign0
    ds = case E.integralToDigits (abs p) of
      Nothing -> error "digitRange failed"
      Just r -> r

mkFinite
  :: Integer
  -- ^ Coefficient
  -> Int
  -- ^ Exponent
  -> Visible
mkFinite co ex =
  let (s, ds) = mkDigits co
      en = case E.finiteExp ex of
        Left _ -> error "mkFinite: exponent failed"
        Right r -> r
      pad = replicate (E.finiteDigitsLen - length ds) E.D0
      digs = case E.finiteDigits (pad ++ ds) of
        Nothing -> error "genFiniteRange: coefficient failed"
        Just g -> g
      d = E.Decoded s (E.Finite digs en)
      dec = runEnv (E.fromBCD d)
  in Visible dec

genSmallFinite :: Gen Visible
genSmallFinite = liftM2 mkFinite genC genR
  where
    n = biggestDigs 5
    genC = choose (negate n, n)
    genR = choose (-10, 10)

newtype SmallFin = SmallFin { unSmallFin :: E.Quad }

instance Arbitrary SmallFin where
  arbitrary = fmap (SmallFin . unVisible) genSmallFinite

instance Show SmallFin where
  show = BS8.unpack . runEnv . E.toByteString . unSmallFin

-- | Non zero small finite number.
newtype NZSmallFin = NZSmallFin { unNZSmallFin :: E.Quad }

instance Show NZSmallFin where
  show = BS8.unpack . runEnv . E.toByteString . unNZSmallFin

instance Arbitrary NZSmallFin where
  arbitrary = fmap (NZSmallFin . unVisible)
    $ liftM2 mkFinite genC genR
    where
      genC = oneof [ choose (negate n, (-1))
                   , choose (1, n) ]
      n = biggestDigs 5
      genR = choose (-10, 10)

genOne :: Gen Visible
genOne = do
  e <- choose (0, c'DECQUAD_Pmax - 1)
  let expn = negate e
      c = 10 ^ e
  return $ mkFinite c expn

genZero :: Gen Visible
genZero = do
  e <- choose E.minMaxExp
  return $ mkFinite 0 e

genRound :: Gen E.Round
genRound = elements [ E.roundCeiling, E.roundUp, E.roundHalfUp,
  E.roundHalfEven, E.roundHalfDown, E.roundDown, E.roundFloor,
  E.round05Up, E.roundMax ]

newtype Visible = Visible { unVisible :: E.Quad }

instance Show Visible where
  show = BS8.unpack . runEnv . E.toByteString . unVisible

instance Arbitrary Visible where
  arbitrary = fmap Visible genFromDecoded

genExponent :: Gen E.Exponent
genExponent = oneof
  [ return E.EQuiet
  , return E.ESignaling
  , return E.EInf
  , liftM E.EFinite genFiniteExp
  ]

-- # Decoded generators

genDcdFinite :: Gen E.Decoded
genDcdFinite
  = E.Decoded
  <$> genSign
  <*> (E.Finite <$> genFiniteDigits <*> genFiniteExp)

genDcdInfinite :: Gen E.Decoded
genDcdInfinite
  = E.Decoded
  <$> genSign
  <*> pure E.Infinite

genDcdInteger :: Gen E.Decoded
genDcdInteger
  = E.Decoded
  <$> genSign
  <*> (E.Finite <$> genFiniteDigits <*> pure E.zeroFiniteExp)

genDcdLogical :: Gen E.Decoded
genDcdLogical
  = E.Decoded
  <$> pure E.Sign0
  <*> (E.Finite <$> genLogicalFiniteDigits <*> pure E.zeroFiniteExp)

genDcdNaN :: Gen E.Decoded
genDcdNaN
  = E.Decoded
  <$> genSign
  <*> (E.NaN <$> genNaN <*> genPayloadDigits)

genDcdNegative :: Gen E.Decoded
genDcdNegative
  = E.Decoded
  <$> pure E.Sign1
  <*> (oneof [ E.Finite <$> genFiniteDigits <*> genFiniteExp
             , pure E.Infinite ]
      )

isNonZeroFiniteDigits :: E.FiniteDigits -> Bool
isNonZeroFiniteDigits = any (/= E.D0) . E.unFiniteDigits

genDcdNormal :: Gen E.Decoded
genDcdNormal = do
  s <- genSign
  ds <- genFiniteDigits `suchThat` isNonZeroFiniteDigits
  ex <- choose (E.minNormal, snd E.minMaxExp)
  let finExp = case E.finiteExp ex of
        Left _ -> error "genDcdNormal: exponent failed"
        Right r -> r
  return $ E.Decoded s (E.Finite ds finExp)

genDcdPositive :: Gen E.Decoded
genDcdPositive
  = E.Decoded
  <$> pure E.Sign0
  <*> oneof [ finite, pure E.Infinite ]
  where
    finite = E.Finite <$>
      genFiniteDigits `suchThat` isNonZeroFiniteDigits
      <*> genFiniteExp

genDcdSignaling :: Gen E.Decoded
genDcdSignaling
  = E.Decoded
  <$> genSign
  <*> (E.NaN <$> pure E.Signaling <*> genPayloadDigits)

genDcdSigned :: Gen E.Decoded
genDcdSigned
  = E.Decoded
  <$> pure E.Sign1
  <*> oneof [ E.Finite <$> genFiniteDigits <*> genFiniteExp
            , pure E.Infinite
            , E.NaN <$> genNaN <*> genPayloadDigits
            ]

genDcdSubnormal :: Gen E.Decoded
genDcdSubnormal
  = E.Decoded
  <$> genSign
  <*> liftM2 E.Finite
             (genFiniteDigits `suchThat` isNonZeroFiniteDigits)
             genExp
  where
    genExp = do
      e <- choose (fst E.minMaxExp, E.minNormal - 1)
      case E.finiteExp e of
        Left _ -> error "genDcdSubnormal: exponent failed"
        Right g -> return g

genDcdZero :: Gen E.Decoded
genDcdZero = E.Decoded <$> genSign <*> liftM2 E.Finite (return gD) gE
  where
    gD = case E.finiteDigits (replicate E.finiteDigitsLen E.D0) of
      Nothing -> error "genDcdZero: finiteDigits failed"
      Just r -> r
    gE = genFiniteExp

-- # Test builders

associativity
  :: String
  -- ^ Name
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
associativity n f = testProperty desc $
  forAll genSmallFinite $ \(Visible x) ->
  forAll genSmallFinite $ \(Visible y) ->
  forAll genSmallFinite $ \(Visible z) ->
  let (noFlags, resIsZero) = evalCtx $ do
        r1 <- f x y >>= f z
        r2 <- f y z >>= f x
        c <- liftEnv $ E.compareTotal r1 r2
        isZ <- liftEnv $ E.isZero c
        fl <- E.getStatus
        return (fl == E.emptyFlags, isZ)
  in noFlags ==> resIsZero
  where
    desc = n ++ " is associative on finite numbers"

commutativity
  :: String
  -- ^ Name
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
commutativity n f = testProperty desc $
  forAll genSmallFinite $ \(Visible x) ->
  forAll genSmallFinite $ \(Visible y) ->
  let (noFlags, resIsZero) = evalCtx $ do
        r1 <- f x y
        r2 <- f y x
        c <- liftEnv $ E.compareTotal r1 r2
        isZ <- liftEnv $ E.isZero c
        fl <- E.getStatus
        return (fl == E.emptyFlags, isZ)
  in noFlags ==> resIsZero
  where
    desc = n ++ " is commutative where there are no flags"

imuUni
  :: String
  -- ^ Name
  -> (E.Quad -> E.Ctx a)
  -> TestTree
imuUni n f = testProperty desc $ \(Visible d) -> evalCtx $ do
  dcd1 <- liftEnv $ E.toBCD d
  _ <- f d
  dcd2 <- liftEnv $ E.toBCD d
  return $ dcd1 == dcd2
  where
    desc = n ++ " (unary function) does not mutate only argument"

imuBinary
  :: String
  -> (E.Quad -> E.Quad -> E.Ctx a)
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
  -> (E.Quad -> c -> E.Ctx b)
  -> TestTree
imuBinary1st n (genA, getC) f = testProperty desc $
  forAll arbitrary $ \(Visible d) ->
  forAll genA $ \a -> evalCtx $ do
    dcd1 <- liftEnv $ E.toBCD d
    _ <- f d (getC a)
    dcd2 <- liftEnv $ E.toBCD d
    return $ dcd1 == dcd2
  where
    desc = n ++ " (binary function) does not mutate first argument"

imuBinary2nd
  :: Show a
  => String
  -- ^ Name
  -> (Gen a, a -> c)
  -> (c -> E.Quad -> E.Ctx b)
  -> TestTree
imuBinary2nd n (genA, getC) f = testProperty desc $
  forAll arbitrary $ \(Visible d) ->
  forAll genA $ \a -> evalCtx $ do
    dcd1 <- liftEnv $ E.toBCD d
    _ <- f (getC a) d
    dcd2 <- liftEnv $ E.toBCD d
    return $ dcd1 == dcd2
  where
    desc = n ++ " (binary function) does not mutate second argument"

imuTernary
  :: String
  -> (E.Quad -> E.Quad -> E.Quad -> E.Ctx a)
  -> TestTree
imuTernary n f = testGroup (n ++ " (ternary function) - immutability")
  [ testProperty "first argument" $
    \(Visible a) (Visible b) (Visible c) -> evalCtx $ do
      dcd1 <- liftEnv $ E.toBCD a
      _ <- f a b c
      dcd2 <- liftEnv $ E.toBCD a
      return $ dcd1 == dcd2

  , testProperty "second argument" $
    \(Visible a) (Visible b) (Visible c) -> evalCtx $ do
      dcd1 <- liftEnv $ E.toBCD b
      _ <- f a b c
      dcd2 <- liftEnv $ E.toBCD b
      return $ dcd1 == dcd2

  , testProperty "third argument" $
    \(Visible a) (Visible b) (Visible c) -> evalCtx $ do
      dcd1 <- liftEnv $ E.toBCD c
      _ <- f a b c
      dcd2 <- liftEnv $ E.toBCD c
      return $ dcd1 == dcd2
  ]

identity
  :: String
  -- ^ Name of thing that is identity (e.g. zero)
  -> Gen Visible
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
identity n g f = testProperty name $
  forAll g $ \(Visible a) ->
  forAll genFinite $ \(Visible b) -> evalCtx $ do
    r <- f b a
    c <- E.compare b r
    liftEnv $ E.isZero c
  where
    name = n ++ " is the identity for finite numbers"

comparison
  :: String
  -- ^ Name of function
  -> (E.Quad -> E.Ctx E.Quad)
  -- ^ How to make a larger Quad
  -> (E.Quad -> E.Ctx E.Quad)
  -- ^ How to make a smaller Quad
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree

comparison n fB fS fC = testGroup (n ++ " comparisons")
  [ testProperty "x > y" $ \(NZSmallFin a) -> evalCtx $ do
      b <- fB a
      c <- fC b a
      liftEnv $ E.isPositive c

  , testProperty "x < y" $ \(NZSmallFin a) -> evalCtx $ do
      b <- fS a
      c <- fC b a
      liftEnv $ E.isNegative c

  , testProperty "x == x" $ \(NZSmallFin a) -> evalCtx $ do
      c <- fC a a
      liftEnv $ E.isZero c

  , testProperty "transitive"
    $ \(NZSmallFin a) (NZSmallFin b) -> evalCtx $ do
      c <- fC a b
      z <- liftEnv $ E.isZero c
      if z
        then do
          c' <- fC b a
          liftEnv $ E.isZero c'
        else do
          c' <- fC b a
          newSign <- E.minus c
          cmpResults <- E.compare c' newSign
          liftEnv $ E.isZero cmpResults
  ]

testMinMax
  :: String
  -> Bool
  -- ^ True if testing absolute values
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
testMinMax n ab f = testProperty (n ++ " and compare")
  $ \(SmallFin aa) (SmallFin bb) -> evalCtx $ do
    (a, b) <- if ab
      then do
        aaa <- E.abs aa
        bbb <- E.abs bb
        return $ (aaa, bbb)
      else return (aa, bb)
    r <- E.compare a b
    m <- f a b
    z <- liftEnv $ E.isZero r
    if z
      then do
        r' <- E.compare m a
        r'' <- E.compare m b
        zr' <- liftEnv $ E.isZero r'
        zr'' <- liftEnv $ E.isZero r''
        return $ zr' && zr''
      else do
        new <- f b a
        r' <- E.compare new m
        liftEnv $ E.isZero r' 

decodedSameQuantum :: E.Decoded -> E.Decoded -> Bool
decodedSameQuantum x y = case (E.dValue x, E.dValue y) of
  (E.Finite _ e1, E.Finite _ e2) -> e1 == e2
  (E.Infinite, E.Infinite) -> True
  (E.NaN _ _, E.NaN _ _) -> True
  _ -> False

-- | Tests that a boolean function succeeds and fails as it should.

testBoolean
  :: String
  -- ^ Name
  -> Gen E.Decoded
  -- ^ Generates decodes that should succeed
  -> (E.Decoded -> Bool)
  -- ^ This predicate returns True on successful decodes
  -> (E.Quad -> E.Env Bool)
  -- ^ Function to test
  -> TestTree
testBoolean n g pd f = testGroup n
  [ testProperty "predicate returns true on generated decodes" $
    forAll g $ \d -> pd d
  
  , testProperty "succeeds when it should" $
    forAll genP $
    runEnv . f . unVisible . fst

  , testProperty "fails when it should" $
    forAll genF $
    not . runEnv . f . unVisible . fst

  , testProperty "decNumber and Deka predicate return same result"
    $ \(Visible q) -> runEnv $ do
        d <- E.toBCD q
        b <- f q
        return $ b == pd d
  ]
  where
    genP = do
      d <- g
      return (Visible . runEnv . E.fromBCD $ d, d)
    genF = do
      d <- genDecoded `suchThat` (not . pd)
      return (Visible . runEnv . E.fromBCD $ d, d)

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
      [ imuUni "decClass" (fmap liftEnv E.decClass)
      , imuUni "toBCD" (fmap liftEnv E.toBCD)
      , imuUni "toByteString" (fmap liftEnv E.toByteString)
      , imuUni "toEngByteString" (fmap liftEnv E.toEngByteString)
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
      , imuUni "getExponent" (fmap liftEnv E.getExponent)

      , testProperty "setExponent" $
        forAll genExponent $ \ex ->
        \(Visible q) -> evalCtx $ do
          b1 <- liftEnv $ E.toBCD q
          _ <- E.setExponent ex q
          b2 <- liftEnv $ E.toBCD q
          return $ b1 == b2

      , imuUni "getCoefficient" (fmap liftEnv E.getCoefficient)

      , testProperty "setCoefficient" $
        forAll genCoeffDigits $ \ds ->
        forAll genSign $ \sn ->
        \(Visible q) -> runEnv $ do
          b1 <- E.toBCD q
          _ <- E.setCoefficient ds sn q
          b2 <- E.toBCD q
          return $ b1 == b2
      ]

    , testGroup "comparisons"
      [ imuBinary "compare" E.compare
      , imuBinary "compareSignal" E.compareSignal
      , imuBinary "compareTotal"
        (fmap (fmap liftEnv) E.compareTotal)
      , imuBinary "compareTotalMag"
        (fmap (fmap liftEnv) E.compareTotalMag)
      , imuBinary "max" E.max
      , imuBinary "maxMag" E.maxMag
      , imuBinary "min" E.min
      , imuBinary "minMag" E.minMag
      , imuBinary "sameQuantum"
        (fmap (fmap liftEnv) E.sameQuantum)
      ]

    , let f s k = imuUni s (fmap liftEnv k) in
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
      , imuBinary "copySign" (fmap (fmap liftEnv) E.copySign)
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
      [ imuUni "digits" (fmap liftEnv E.digits)
      ]
    ]

  , testGroup "conversions"
    [ testGroup "exponent"
      [ testProperty "fails when exponent is too small" $
        let (l, _) = E.minMaxExp in
        forAll (choose (minBound, l - 1)) $
        isLeft . E.finiteExp

      , testProperty "fails when exponent is too large" $
        let (_, h) = E.minMaxExp in
        forAll (choose (h + 1, maxBound)) $
        isLeft . E.finiteExp

      , testProperty "fails when exponent is < c'DECQUAD_Emin" $
        forAll (choose (minBound, c'DECQUAD_Emin - 1)) $
        isLeft . E.finiteExp

      , testProperty "fails when exponent is > c'DECQUAD_Emax" $
        forAll (choose (c'DECQUAD_Emax + 1, maxBound)) $
        isLeft . E.finiteExp

      , testProperty "succeeds when it should" $
        forAll (choose E.minMaxExp) $
        isRight . E.finiteExp

      ]

    , testGroup "decode and encode"
      [ testProperty "round trip from Quad" $
        forAll (fmap Blind genFromDecoded) $ \(Blind d) ->
        runEnv $ do
          dcd <- E.toBCD d
          ecd <- E.fromBCD dcd
          r <- E.compareTotal ecd d
          E.isZero r

      , testProperty "round trip from Decoded" $
        forAll genDecoded $ \d ->
        let r = runEnv $ do
              ecd <- E.fromBCD d
              E.toBCD ecd
        in printTestCase ("result: " ++ show r) (r == d)

      , testProperty "round trip from subnormal Decoded" $
        forAll genDcdSubnormal $ \d ->
        let r = runEnv $ do
              ecd <- E.fromBCD d
              E.toBCD ecd
        in r == d
      ]

    , testGroup "strings"
      [ testProperty ("Decoded -> Quad -> ByteString"
          ++ " -> Quad -> Decoded") $
        forAll (oneof [genDecoded, genDcdSubnormal]) $ \d ->
          let r = evalCtx $ do
                q <- liftEnv $ E.fromBCD d
                bs <- liftEnv $ E.toByteString q
                q' <- E.fromByteString bs
                d' <- liftEnv $ E.toBCD q'
                return (d', bs)
              desc = "toByteString: " ++ BS8.unpack (snd r)
                ++ " toBCD: " ++ show (fst r)
          in printTestCase desc $ fst r == d
      ]
    ]

  , testGroup "arithmetic"
    [ testGroup "add"
      [ associativity "add" E.add
      , commutativity "add" E.add
      , identity "zero" genZero E.add
      ]

    , testGroup "multiply"
      [ associativity "multiply" E.multiply
      , commutativity "multiply" E.multiply
      , identity "one" genOne E.multiply
      ]

    , testGroup "subtract"
      [ testProperty "is the inverse of add" $
        forAll genSmallFinite $ \(Visible a) ->
        forAll genSmallFinite $ \(Visible b) ->
        let (r, fl) = runCtx $ do
              r1 <- E.add a b
              r2 <- E.subtract r1 b
              c <- E.compare r2 a
              liftEnv $ E.isZero c
        in fl == E.emptyFlags ==> r

      , identity "zero" genZero E.subtract
      ]

    , testGroup "fused multiply add"
      [ testProperty "is same as multiply and add" $
        forAll genSmallFinite $ \(Visible a) ->
        forAll genSmallFinite $ \(Visible b) ->
        forAll genSmallFinite $ \(Visible c) ->
        let (r, fl) = runCtx $ do
              r1 <- E.multiply a b
              r2 <- E.add r1 c
              r2' <- E.fma a b c
              cm <- E.compare r2 r2'
              liftEnv $ E.isZero cm
        in fl == E.emptyFlags ==> r

      , testGroup "divide"
        [ identity "one" genOne E.divide ]

      , testGroup "divideInteger"
        [ testProperty "result has exponent 0" $
          \(SmallFin a) (SmallFin b) ->
          let (e, fl) = runCtx $ do
                c <- E.divideInteger a b
                liftEnv $ E.isInteger c
          in fl == E.emptyFlags ==> e
        ]

      , testGroup "remainder"
        [ testProperty "x = int * y + rem" $
          \(SmallFin x) (SmallFin y) ->
          let (r, fl) = runCtx $ do
                it <- E.divideInteger x y
                rm <- E.remainder x y
                i1 <- E.multiply it y
                i2 <- E.add i1 rm
                c <- E.compare i2 x
                liftEnv $ E.isZero c
          in fl == E.emptyFlags ==> r
        ]

      -- remainderNear - no test - not sure I understand the
      -- semantics

      ]
    ]

  , testGroup "exponent and coefficient adjustment"
    [ testGroup "quantize"
      [ testProperty "result has same quantum" $
        \(SmallFin x) (SmallFin y) ->
        let (r, fl) = runCtx $ do
              c <- E.quantize x y
              exC <- liftEnv $ E.getExponent c
              exY <- liftEnv $ E.getExponent y
              fin <- liftEnv $ E.isFinite c
              return $ fin && exC == exY
        in fl == E.emptyFlags ==> r
      ]

    , testGroup "reduce"
      [ testProperty "result is equivalent" $
        \(SmallFin x) -> evalCtx $ do
            r <- E.reduce x
            c <- E.compare r x
            liftEnv $ E.isZero c

      , testProperty "result has no trailing zeroes" $
        \(SmallFin x) -> evalCtx $ do
            r <- E.reduce x
            dcd <- liftEnv $ E.toBCD r
            return $ case E.dValue dcd of
              E.Infinite -> False
              E.NaN _ _ -> False
              E.Finite ds _ ->
                let digs = E.unFiniteDigits ds
                in all (== E.D0) digs || last digs /= E.D0
      ]
    ]

  , testGroup "comparisons"
    [ comparison "compare" E.nextPlus E.nextMinus E.compare
    , comparison "compareSignal" E.nextPlus
        E.nextMinus E.compare

    , comparison "compareTotal" E.nextPlus E.nextMinus
        (fmap (fmap liftEnv) E.compareTotal)

    , comparison "compareTotalMag" increaseAbs decreaseAbs
          (fmap (fmap liftEnv) E.compareTotalMag)

    , testMinMax "min" False E.min
    , testMinMax "max" False E.max
    , testMinMax "maxMag" True E.maxMag
    , testMinMax "minMag" True E.minMag

    , testGroup "sameQuantum"
      [ testProperty "is true for same Decoded" $
        forAll genDecoded $ \d -> runEnv $ do
          x <- E.fromBCD d
          E.sameQuantum x x

      , testProperty "is false for different Decoded" $
        forAll ( liftM2 (,) genDecoded genDecoded
                  `suchThat` (not . uncurry decodedSameQuantum))
        $ \p -> runEnv $ do
                  qx <- E.fromBCD . fst $ p
                  qy <- E.fromBCD . snd $ p
                  fmap not $ E.sameQuantum qx qy
      ]
    ]

  , testGroup "tests"
    [ testBoolean "isFinite" genDcdFinite E.dIsFinite E.isFinite
    , testBoolean "isInfinite" genDcdInfinite
        E.dIsInfinite E.isInfinite
    , testBoolean "isInteger" genDcdInteger
        E.dIsInteger E.isInteger
    , testBoolean "isLogical" genDcdLogical
        E.dIsLogical E.isLogical
    , testBoolean "isNaN" genDcdNaN
        E.dIsNaN E.isNaN
    , testBoolean "isNegative" genDcdNegative
        E.dIsNegative E.isNegative
    , testBoolean "isNormal" genDcdNormal
        E.dIsNormal E.isNormal
    , testBoolean "isPositive" genDcdPositive
        E.dIsPositive E.isPositive
    , testBoolean "isSignaling" genDcdSignaling
        E.dIsSignaling E.isSignaling
    , testBoolean "isSigned" genDcdSigned
        E.dIsSigned E.isSigned
    , testBoolean "isSubnormal" genDcdSubnormal
        E.dIsSubnormal E.isSubnormal
    , testBoolean "isZero" genDcdZero
        E.dIsZero E.isZero
    ]
  ]
