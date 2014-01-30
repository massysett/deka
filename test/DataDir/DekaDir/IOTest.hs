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
import Data.Deka.Pure (runCtx, evalCtx)
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

increaseAbs :: E.Quad -> E.Ctx E.Quad
increaseAbs q = do
    neg <- E.isNegative q
    if neg
      then E.nextMinus q
      else E.nextPlus q

decreaseAbs :: E.Quad -> E.Ctx E.Quad
decreaseAbs q = do
  neg <- E.isNegative q
  if neg
    then E.nextPlus q
    else E.nextMinus q

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

genExp :: Gen E.Exponent
genExp = fmap f (choose E.minMaxExp)
  where
    f e = case E.exponent e of
      Left _ -> error "genExp failed"
      Right g -> g

genPayload :: Gen E.Payload
genPayload = do
  i <- choose (0, biggestDigs (c'DECQUAD_Pmax - 1))
  case E.payload i of
    Nothing -> error "genPayload failed"
    Just r -> return r

genValue :: Gen E.Value
genValue = oneof
  [ liftM2 E.Finite genCoefficient genExp
  , return E.Infinite
  , liftM2 E.NaN genNaN genPayload
  ]

genDecoded :: Gen E.Decoded
genDecoded = liftM2 E.Decoded genSign genValue

genFromDecoded :: Gen E.Quad
genFromDecoded = do
  d <- genDecoded
  return . fst . runCtx . E.encode $ d

genFinite :: Gen Visible
genFinite = do
  v <- liftM2 E.Finite genCoefficient genExp
  s <- genSign
  return . Visible . evalCtx . E.encode $ E.Decoded s v

genSmallFinite :: Gen Visible
genSmallFinite = do
  c <- choose (0, biggestDigs 5)
  let co = case E.coefficient c of
        Left _ -> error "genSmallFinite: coefficient failed"
        Right g -> g
  e <- choose (-10, 10)
  let en = case E.exponent e of
        Left _ -> error "genSmallFinite: coefficient failed"
        Right g -> g
  s <- genSign
  let d = E.Decoded s (E.Finite co en)
      dec = evalCtx (E.encode d)
  return . Visible $ dec

newtype SmallFin = SmallFin { unSmallFin :: E.Quad }

instance Arbitrary SmallFin where
  arbitrary = fmap (SmallFin . unVisible) genSmallFinite

instance Show SmallFin where
  show = BS8.unpack . evalCtx . E.toString . unSmallFin

-- | Non zero small finite number.
newtype NZSmallFin = NZSmallFin { unNZSmallFin :: E.Quad }

instance Show NZSmallFin where
  show = BS8.unpack . evalCtx . E.toString . unNZSmallFin

instance Arbitrary NZSmallFin where
  arbitrary = do
    c <- choose (1, biggestDigs 5)
    let co = case E.coefficient c of
          Left _ -> error "genSmallFinite: coefficient failed"
          Right g -> g
    e <- choose (-10, 10)
    let en = case E.exponent e of
          Left _ -> error "genSmallFinite: coefficient failed"
          Right g -> g
    s <- genSign
    let d = E.Decoded s (E.Finite co en)
        dec = evalCtx (E.encode d)
    return . NZSmallFin $ dec


genOne :: Gen Visible
genOne = fmap f $ choose (0, c'DECQUAD_Pmax - 1)
  where
    f e = let expn = negate e
              c = 1 * 10 ^ e
              _types = e :: Int
              coef = either
                (const $ error "genOne: coefficient failed")
                id . E.coefficient $ c
              en = either
                (const $ error "genOne: coeffExp failed")
                id $ E.exponent expn
              dcd = E.Decoded E.Positive (E.Finite coef en)
          in Visible . evalCtx . E.encode $ dcd
              
genZero :: Gen Visible
genZero = fmap f $ choose E.minMaxExp
  where
    f e = let expn = either
                (const $ error "genZero: exponent failed")
                id . E.exponent $ e
              coef = either
                (const $ error "genZero: coefficient failed")
                id . E.coefficient $ 0
              dcd = E.Decoded E.Positive (E.Finite coef expn)
          in Visible . evalCtx . E.encode $ dcd


genRound :: Gen E.Round
genRound = elements [ E.roundCeiling, E.roundUp, E.roundHalfUp,
  E.roundHalfEven, E.roundHalfDown, E.roundDown, E.roundFloor,
  E.round05Up, E.roundMax ]

newtype Visible = Visible { unVisible :: E.Quad }

instance Show Visible where
  show = BS8.unpack . evalCtx . E.toString . unVisible

instance Arbitrary Visible where
  arbitrary = fmap Visible genFromDecoded

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
  -> (E.Quad -> E.Quad -> E.Ctx E.Quad)
  -> TestTree
commutativity n f = testProperty desc $
  forAll genSmallFinite $ \(Visible x) ->
  forAll genSmallFinite $ \(Visible y) ->
  let (noFlags, resIsZero) = evalCtx $ do
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
  -> (E.Quad -> E.Ctx a)
  -> TestTree
imuUni n f = testProperty desc $ \(Visible d) -> evalCtx $ do
  dcd1 <- E.decode d
  _ <- f d
  dcd2 <- E.decode d
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
  -> (c -> E.Quad -> E.Ctx b)
  -> TestTree
imuBinary2nd n (genA, getC) f = testProperty desc $
  forAll arbitrary $ \(Visible d) ->
  forAll genA $ \a -> evalCtx $ do
    dcd1 <- E.decode d
    _ <- f (getC a) d
    dcd2 <- E.decode d
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
      dcd1 <- E.decode a
      _ <- f a b c
      dcd2 <- E.decode a
      return $ dcd1 == dcd2

  , testProperty "second argument" $
    \(Visible a) (Visible b) (Visible c) -> evalCtx $ do
      dcd1 <- E.decode b
      _ <- f a b c
      dcd2 <- E.decode b
      return $ dcd1 == dcd2

  , testProperty "third argument" $
    \(Visible a) (Visible b) (Visible c) -> evalCtx $ do
      dcd1 <- E.decode c
      _ <- f a b c
      dcd2 <- E.decode c
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
    E.isZero c
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
      E.isPositive c

  , testProperty "x < y" $ \(NZSmallFin a) -> evalCtx $ do
      b <- fS a
      c <- fC b a
      E.isNegative c

  , testProperty "x == x" $ \(NZSmallFin a) -> evalCtx $ do
      c <- fC a a
      E.isZero c

  , testProperty "transitive"
    $ \(NZSmallFin a) (NZSmallFin b) -> evalCtx $ do
      c <- fC a b
      z <- E.isZero c
      if z
        then do
          c' <- fC b a
          E.isZero c'
        else do
          c' <- fC b a
          newSign <- E.minus c
          cmpResults <- E.compare c' newSign
          E.isZero cmpResults
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
    z <- E.isZero r
    if z
      then do
        r' <- E.compare m a
        r'' <- E.compare m b
        zr' <- E.isZero r'
        zr'' <- E.isZero r''
        return $ zr' && zr''
      else do
        new <- f b a
        r' <- E.compare new m
        E.isZero r' 

decodedSameQuantum :: E.Decoded -> E.Decoded -> Bool
decodedSameQuantum x y = case (E.dValue x, E.dValue y) of
  (E.Finite _ e1, E.Finite _ e2) -> e1 == e2
  (E.Infinite, E.Infinite) -> True
  (E.NaN _ _, E.NaN _ _) -> True
  _ -> False

matchClass
  :: String
  -> (E.Quad -> E.Ctx Bool)
  -> [E.DecClass]
  -- ^ If the function returns True, then the Quad is a member of
  -- one of these classes; otherwise, it is not a member of one of
  -- these classes.
  -> TestTree
matchClass n f ls = testProperty n $ \(Visible a) ->
  let r = evalCtx $ do
        b <- f a
        c <- E.decClass a
        let isElem = c `elem` ls
        return $ if b then (isElem, c) else (not isElem, c)
  in printTestCase (show . snd $ r) (fst r)

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

    , testGroup "exponent"
      [ testProperty "fails when exponent is too small" $
        let (l, _) = E.minMaxExp in
        forAll (choose (minBound, l - 1)) $
        isLeft . E.exponent

      , testProperty "fails when exponent is too large" $
        let (_, h) = E.minMaxExp in
        forAll (choose (h + 1, maxBound)) $
        isLeft . E.exponent

      , testProperty "fails when exponent is < c'DECQUAD_Emin" $
        forAll (choose (minBound, c'DECQUAD_Emin - 1)) $
        isLeft . E.exponent

      , testProperty "fails when exponent is > c'DECQUAD_Emax" $
        forAll (choose (c'DECQUAD_Emax + 1, maxBound)) $
        isLeft . E.exponent

      , testProperty "succeeds when it should" $
        forAll (choose E.minMaxExp) $
        isRight . E.exponent

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
          evalCtx $ do
            dcd <- E.decode d
            ecd <- E.encode dcd
            r <- E.compareTotal ecd d
            E.isZero r

        , testProperty "round trip from Decoded" $
          forAll genDecoded $ \d ->
          let r = evalCtx $ do
                ecd <- E.encode d
                E.decode ecd
          in printTestCase ("result: " ++ show r) (r == d)
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
                E.isZero c
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
                E.isZero cm
          in fl == E.emptyFlags ==> r

        , testGroup "divide"
          [ identity "one" genOne E.divide ]

        , testGroup "divideInteger"
          [ testProperty "result has exponent 0" $
            \(SmallFin a) (SmallFin b) ->
            let (e, fl) = runCtx $ do
                  c <- E.divideInteger a b
                  E.decode c
            in fl == E.emptyFlags ==> E.finiteExponent e == Just 0
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
                  E.isZero c
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
                  dc <- E.decode c
                  dy <- E.decode y
                  return $ E.finiteExponent dc == E.finiteExponent dy
            in fl == E.emptyFlags ==> r
          ]

        , testGroup "reduce"
          [ testProperty "result is equivalent" $
            \(SmallFin x) -> evalCtx $ do
                r <- E.reduce x
                c <- E.compare r x
                E.isZero c

          , testProperty "result has no trailing zeroes" $
            \(SmallFin x) -> evalCtx $ do
                r <- E.reduce x
                dx <- E.decode x
                dr <- E.decode r
                let cr = E.finiteCoefficient dr
                return $ case E.finiteCoefficient dx of
                  Just i
                    | i == 0 -> E.finiteCoefficient dr == Just 0
                    | otherwise -> case cr of
                        Nothing -> False
                        Just i' -> i' `mod` 10 /= 0
                  _ -> False
          ]
        ]

      , testGroup "comparisons"
        [ comparison "compare" E.nextPlus E.nextMinus E.compare
        , comparison "compareSignal" E.nextPlus
            E.nextMinus E.compare

        , comparison "compareTotal" E.nextPlus E.nextMinus
            E.compareTotal

        , comparison "compareTotalMag" increaseAbs decreaseAbs
              E.compareTotalMag

        , testMinMax "min" False E.min
        , testMinMax "max" False E.max
        , testMinMax "maxMag" True E.maxMag
        , testMinMax "minMag" True E.minMag

        , testGroup "sameQuantum"
          [ testProperty "is true for same Decoded" $
            forAll genDecoded $ \d -> evalCtx $ do
              x <- E.encode d
              E.sameQuantum x x

          , testProperty "is false for different Decoded" $
            forAll ( liftM2 (,) genDecoded genDecoded
                      `suchThat` (not . uncurry decodedSameQuantum))
            $ \p -> evalCtx $ do
              qx <- E.encode . fst $ p
              qy <- E.encode . snd $ p
              fmap not $ E.sameQuantum qx qy
           ]
        ]

      , testGroup "tests"
        [ matchClass "isFinite" E.isFinite
          [ E.negNormal, E.negSubnormal, E.negZero,
            E.posZero, E.posSubnormal, E.posNormal ]

        , matchClass "isInfinite" E.isInfinite
            [ E.negInf, E.posInf ]

        , matchClass "isInteger" E.isInteger
            [ E.negNormal, E.negZero, E.posZero, E.posNormal,
              E.negSubnormal, E.posSubnormal ]
        ]
      ]
