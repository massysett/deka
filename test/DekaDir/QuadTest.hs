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

maxSize :: Int -> Gen a -> Gen a
maxSize s g = sized $ \o -> resize (min o s) g

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

-- | Given a maximum length, generate lists of digits that are no
-- longer than the length given.  The list will be of a random
-- length, but it will be no longer than the larger of the size
-- parameter and the given maximum length.  The list will always be
-- at least one element long regardless of the maximum length passed
-- in.
sizedDigits
  :: Int
  -- ^ Maximum length. (Size parameter determines the maximum
  -- length, but it will not exceed this amount.)
  -> (Gen E.Digit, Gen E.Digit)
  -- ^ Generate MSD, remaining digits
  -> Gen [E.Digit]
sizedDigits m (gm, gr) = sized $ \s -> do
  let sz = max 1 s
      maxLen = min sz m
  len <- choose (1, maxLen)
  genDigits len (gm, gr)

-- ## Finite number generators

coeffDigits :: (Gen E.Digit, Gen E.Digit) -> Gen [E.Digit]
coeffDigits p = sized f
  where
    f x | x == 0 = oneof [ sizedDigits 0 p, return [E.D0] ]
        | otherwise = sizedDigits E.coefficientLen p

genFiniteDcd
  :: Gen E.Sign
  -> Gen [E.Digit]
  -- ^ Generate coefficient
  -> (E.Coefficient -> Gen Int)
  -- ^ Generate exponent
  -> Gen E.Decoded
genFiniteDcd gs gc ge = do
  s <- gs
  ds <- gc
  let coe = case E.coefficient ds of
        Nothing -> error "genFinite: coefficient failed"
        Just r -> r
  e <- ge coe
  let ex = case E.exponent e of
        Nothing -> error "genFiniteDcd: exponent failed"
        Just r -> r
  return $ E.Decoded s (E.Finite coe ex)

rangedExponent
  :: (Int, Int)
  -- ^ Minimum and maximum exponent.  Exponent will never exceed
  -- allowable values.
  -> Gen Int
rangedExponent (em, ex) = do
  let (mPE, xPE) = E.minMaxExp
      (mR, xR) = (max em mPE, min ex xPE)
  choose (mR, xR)

sizedExponent :: Gen Int
sizedExponent = sized $ \s ->
  let x = s ^ (2 :: Int)
  in rangedExponent (negate x, x)

fullExpRange :: Gen Int
fullExpRange = rangedExponent E.minMaxExp

-- ## Infinite number generators

genInfinite :: Gen E.Sign -> Gen E.Decoded
genInfinite gs = do
  s <- gs
  return $ E.Decoded s E.Infinite

-- ## NaN number generators

payloadDigits :: (Gen E.Digit, Gen E.Digit) -> Gen [E.Digit]
payloadDigits = sizedDigits E.payloadLen

genNaN :: Gen E.NaN
genNaN = elements [ E.Quiet, E.Signaling ]

genNaNDcd
  :: Gen E.Sign
  -> Gen E.NaN
  -> Gen [E.Digit]
  -- ^ Generate payload
  -> Gen E.Decoded
genNaNDcd gs gn gd = do
  s <- gs
  ds <- gd
  n <- gn
  let pay = case E.payload ds of
        Nothing -> error "genNaNDcd: payload failed"
        Just r -> r
  return $ E.Decoded s (E.NaN n pay)

-- ## Decoded generators

-- | Most general Decoded generator.  Generates throughout the
-- possible range of Decoded.  Depends on the size parameter.
genDecoded :: Gen E.Decoded
genDecoded = frequency [(4, genFinite), (1, inf), (1, nan)]
  where
    inf = genInfinite genSign
    nan = genNaNDcd genSign genNaN (payloadDigits decimalDigs)

-- | Generates finite decoded numbers.
genFinite :: Gen E.Decoded
genFinite = genFiniteDcd genSign (coeffDigits decimalDigs)
            (const sizedExponent)
 

-- ## Specialized finite generators

-- | Generates positive and negative zeroes.
genZero :: Gen E.Decoded
genZero = genFiniteDcd genSign (return [E.D0]) (const fullExpRange)

genNegZero :: Gen E.Decoded
genNegZero = genFiniteDcd (return E.Sign1) (return [E.D0])
  (const fullExpRange)

genPosZero :: Gen E.Decoded
genPosZero = genFiniteDcd (return E.Sign0) (return [E.D0])
  (const fullExpRange)

-- | Generates positive one.
genOne :: Gen E.Decoded
genOne = genFiniteDcd (return E.Sign0) gDigs gExp
  where
    gDigs = sizedDigits E.coefficientLen (return E.D1, return E.D0)
    gExp co = return . negate $ length (E.unCoefficient co) - 1

genSmallFinite :: Gen E.Decoded
genSmallFinite = maxSize 5 genFinite

-- | Generates two values that are equivalent, but with
-- different exponents.

genEquivalent :: Gen (E.Decoded, E.Decoded)
genEquivalent = do
  let genCoeff1 = sizedDigits (E.coefficientLen - 1) decimalDigs
      genExp1 c =
        let (l, h) = E.minMaxExp
            l' = l + (E.coefficientLen - (length . E.unCoefficient $ c))
        in choose (l', h)
  d1 <- genFiniteDcd genSign genCoeff1 genExp1
  let (c1, e1) = case E.dValue d1 of
        E.Finite c e -> (E.unCoefficient c, E.unExponent e)
        _ -> error "genEquivalent failed"
      maxMore = E.coefficientLen - length c1
  more <- choose (1, maxMore)
  let coeff2 = case E.coefficient (c1 ++ replicate more E.D0) of
        Nothing -> error "genEquivalent: coefficient failed"
        Just r -> r
      exp2 = case E.exponent (e1 - more) of
        Nothing -> error "genEquivalent: exponent failed"
        Just r -> r
      d2 = E.Decoded (E.dSign d1) (E.Finite coeff2 exp2)
  b <- arbitrary
  let r = if b then (d1, d2) else (d2, d1)
  return r



genNonZeroSmallFinite :: Gen E.Decoded
genNonZeroSmallFinite = maxSize 5 $ genFiniteDcd genSign
  gd ge
  where
    gd = sizedDigits E.coefficientLen decimalDigs
    ge = (const sizedExponent)

genInteger :: Gen E.Decoded
genInteger = genFiniteDcd genSign
  (coeffDigits decimalDigs) (const . return $ 0)

genLogical :: Gen E.Decoded
genLogical = genFiniteDcd (return E.Sign0)
  (coeffDigits binaryDigs) (const . return $ 0)

genNormal :: Gen E.Sign -> Gen [E.Digit] -> Gen E.Decoded
genNormal gs gc = genFiniteDcd gs gc ge
  where
    ge c = do
      let minNrml = E.unExponent $ E.minNormalExp c
          maxE = snd E.minMaxExp
      choose (minNrml, maxE)

genSubnormal :: Gen E.Sign -> Gen [E.Digit] -> Gen E.Decoded
genSubnormal gs gd = genFiniteDcd gs gd ge
  where
    ge c =
      let minNrml = E.unExponent . E.minNormalExp $ c
          minE = fst E.minMaxExp
          f | minE > minNrml - 1 = error "genSubnormal failed"
            | otherwise = choose (minE, minNrml - 1)
      in f

genPositive :: Gen E.Decoded
genPositive = genFiniteDcd (return E.Sign0) gd ge
  where
    gd = sizedDigits E.coefficientLen decimalDigs
    ge = (const sizedExponent)

genNegative :: Gen E.Decoded
genNegative = genFiniteDcd (return E.Sign1) gd ge
  where
    gd = sizedDigits E.coefficientLen decimalDigs
    ge = (const sizedExponent)

-- ## Specialized other generators

genSignaling :: Gen E.Decoded
genSignaling = genNaNDcd genSign (return E.Signaling)
  (payloadDigits decimalDigs)

genSigned :: Gen E.Decoded
genSigned = oneof
  [ genFiniteDcd (return E.Sign1) (coeffDigits decimalDigs) (const sizedExponent)
  , genNaNDcd (return E.Sign1) genNaN (payloadDigits decimalDigs)
  , genInfinite (return E.Sign1)
  ]

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

{- Also for below, consider this code snippet:

module Main where

import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)

myThing :: String -> Int
myThing s = unsafePerformIO $ putStrLn s >> return 2

main :: IO ()
main = do
  x <- return . Just $ myThing "this will NOT be printed"
  _ <- evaluate x
  y <- return $ myThing "this will be printed"
  _ <- evaluate y
  _ <- evaluate $ myThing "this will be printed too"
  putStrLn "Done"

-}

-- | These functions assume that reducing the return type of the
-- subject function to WHNF will force any associated IO to occur.
-- For example, imuUni will work as intended if you apply it
-- like so:
--
-- > imuUni "okay" (fmap (fmap return) E.decClass)
--
-- In this case, the function passed as an argument to imuUni is
-- run, and the result (Quad) is reduced to WHNF.  This works as
-- intended because it forces the underlying function to perform its
-- IO.
--
-- This would not work, even though it is well-typed:
--
-- > imuUni "broken" (fmap (fmap (return . Just)))
--
-- because in this case, the value returned from the computation is
-- a Ctx Maybe.  Reducing the Maybe to WHNF will not force any
-- underlying IO to occurr, as this just gives you either a Maybe
-- data constructor or _|_.
imuUni
  :: String
  -- ^ Name
  -> (E.Quad -> E.Ctx a)
  -> TestTree
imuUni n f = testProperty desc $
  forAll genDecoded $ \dx ->
  monadicIO $
  let k cPtr = do
        d <- evaluate $ E.fromBCD dx
        dcd1 <- withForeignPtr (unQuad d) peek
        x <- unCtx (f d) cPtr
        _ <- evaluate x
        dcd2 <- withForeignPtr (unQuad d) peek
        return $ dcd1 == dcd2
  in inContext k >>= assert
  where
    desc = n ++ " (unary function) does not mutate only argument"


imuBinary1st
  :: Show a
  => String
  -- ^ Name
  -> (Gen a, a -> c)
  -> (E.Quad -> c -> E.Ctx b)
  -> TestTree
imuBinary1st n (genA, getC) f = testProperty desc $
  forAll genDecoded $ \dx ->
  forAll genA $ \a ->
  monadicIO $
  let k cPtr = do 
        d <- evaluate $ E.fromBCD dx
        dcd1 <- withForeignPtr (unQuad d) peek
        x <- unCtx (f d (getC a)) cPtr
        _ <- evaluate x
        dcd2 <- withForeignPtr (unQuad d) peek
        return $ dcd1 == dcd2
  in inContext k >>= assert
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
  forAll genDecoded $ \dx ->
  forAll genA $ \a ->
  monadicIO $
  let k cPtr = do
        d <- evaluate $ E.fromBCD dx
        dcd1 <- withForeignPtr (unQuad d) peek
        x <- unCtx (f (getC a) d) cPtr
        _ <- evaluate x
        dcd2 <- withForeignPtr (unQuad d) peek
        return $ dcd1 == dcd2
  in inContext k >>= assert
  where
    desc = n ++ " (binary function) does not mutate second argument"

imuBinary
  :: String
  -> (E.Quad -> E.Quad -> E.Ctx a)
  -> TestTree
imuBinary n f = testGroup ("immutability - " ++ n)
  [ imuBinary1st n (genDecoded, E.fromBCD) f
  , imuBinary2nd n (genDecoded, E.fromBCD) f
  ]

imuTernary
  :: String
  -> (E.Quad -> E.Quad -> E.Quad -> E.Ctx a)
  -> TestTree
imuTernary n f = testGroup (n ++ " (ternary function) - immutability")
  [ testProperty "first argument" $
    forAll gen3 $ \(ga, gb, gc) ->
    monadicIO $
    let k cPtr = do
          a <- evaluate $ E.fromBCD ga
          b <- evaluate $ E.fromBCD gb
          c <- evaluate $ E.fromBCD gc 
          dcd1 <- withForeignPtr (unQuad a) peek
          x <- unCtx (f a b c) cPtr
          _ <- evaluate x
          dcd2 <- withForeignPtr (unQuad a) peek
          return $ dcd1 == dcd2
    in inContext k >>= assert

  , testProperty "second argument" $
    forAll gen3 $ \(ga, gb, gc) ->
    monadicIO $
    let k cPtr = do
          a <- evaluate $ E.fromBCD ga
          b <- evaluate $ E.fromBCD gb
          c <- evaluate $ E.fromBCD gc 
          dcd1 <- withForeignPtr (unQuad b) peek
          x <- unCtx (f a b c) cPtr
          _ <- evaluate x
          dcd2 <- withForeignPtr (unQuad b) peek
          return $ dcd1 == dcd2
    in inContext k >>= assert

  , testProperty "third argument" $
    forAll gen3 $ \(ga, gb, gc) ->
    monadicIO $
    let k cPtr = do
          a <- evaluate $ E.fromBCD ga
          b <- evaluate $ E.fromBCD gb
          c <- evaluate $ E.fromBCD gc 
          dcd1 <- withForeignPtr (unQuad c) peek
          x <- unCtx (f a b c) cPtr
          _ <- evaluate x
          dcd2 <- withForeignPtr (unQuad c) peek
          return $ dcd1 == dcd2
    in inContext k >>= assert
  ]
  where
    gen3 = (,,) <$> genDecoded <*> genDecoded <*> genDecoded

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

-- | Tests that a boolean function succeeds and fails as it should.

testBoolean
  :: String
  -- ^ Name
  -> Gen E.Decoded
  -- ^ Generates decodes that should succeed
  -> (E.Decoded -> Bool)
  -- ^ This predicate returns True on successful decodes
  -> (E.Quad -> Bool)
  -- ^ Function to test
  -> TestTree
testBoolean n g pd f = testGroup n
  [ testProperty "predicate returns true on generated decodes" $
    forAll g $ \d -> pd d
  
  , testProperty "succeeds when it should" $
    forAll g $ \dcd ->
      let q = E.fromBCD dcd
      in f q

  , testProperty "fails when it should" $
    forAll (genDecoded `suchThat` (not . pd)) $ \dcd ->
      let q = E.fromBCD dcd
      in not $ f q

  , testProperty "decNumber and Deka predicate return same result"
    $ forAll genDecoded $ \dcd ->
      let q = E.fromBCD dcd
          b = f q
      in b == pd dcd
  ]

-- | Tests functions that deal with DecClass.
testDecClass
  :: E.Class
  -- ^ Class being tested
  -> Gen E.Decoded
  -- ^ Generates Decoded that are in this class
  -> (E.Decoded -> Bool)
  -- ^ This function should return True on Decoded that are in the
  -- class
  -> TestTree

testDecClass c ge f = testGroup (show c)
  [ testProperty "predicate returns True on generated decodes" $
    forAll ge f

  , testProperty "decClass returns matching class" $
    forAll ge $ \dcd -> let q = E.fromBCD dcd in E.decClass q == c

  , testProperty "decClass does not return matching class otherwise" $
    forAll (genDecoded `suchThat` (not . f)) $ \dcd ->
    let q = E.fromBCD dcd in E.decClass q /= c
  ]

genInt32 :: Gen E.C'int32_t
genInt32 = choose (minBound, maxBound)

genUInt32 :: Gen E.C'uint32_t
genUInt32 = choose (minBound, maxBound)

runFlags :: Ctx a -> (a, [E.Flag])
runFlags c = E.runQuad $ do
  r <- c
  f <- E.getStatus
  return (r, f)

intConversion
  :: (Show a, Eq a)
  => String
  -- ^ Name
  -> Gen a
  -> (a -> E.Quad)
  -- ^ Convert from C int
  -> (E.Round -> E.Quad -> E.Ctx a)
  -- ^ Convert to C int
  -> TestTree
intConversion n gen fr to = testGroup (n ++ " conversions")
  [ testProperty "convert from C integer to Quad and back" $
    forAll genRound $ \r ->
    forAll gen $ \i ->
    let q = fr i
        (i', fl) = runFlags $ to r q
    in null fl && i' == i
  ]

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


  , testGroup "immutability"
    [ testGroup "conversions"
      [ imuUni "decClass" (fmap return E.decClass)
      , imuUni "toBCD" (fmap return E.toBCD)
      , imuUni "toByteString" (fmap return E.toByteString)
      , imuUni "toEngByteString" (fmap return E.toEngByteString)
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
      , imuBinary "compareTotal"
        (fmap (fmap return) E.compareTotal)
      , imuBinary "compareTotalMag"
        (fmap (fmap return) E.compareTotalMag)
      , imuBinary "max" E.max
      , imuBinary "maxMag" E.maxMag
      , imuBinary "min" E.min
      , imuBinary "minMag" E.minMag
      , imuBinary "sameQuantum"
        (fmap (fmap return) E.sameQuantum)
      ]

    , let f s k = imuUni s (fmap return k) in
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
      , imuBinary "copySign" (fmap (fmap return) E.copySign)
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

    , testGroup "log and scale"
      [ imuUni "logB" E.logB
      , imuBinary "scaleB" E.scaleB
      ]

    , testGroup "attributes"
      [ imuUni "digits" (fmap return E.digits)
      ]
    ] -- immutability

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

  , testGroup "classes"
    [ testDecClass E.sNaN
      (genNaNDcd genSign (return E.Signaling) (payloadDigits decimalDigs))
      E.dIsNaN

    , testDecClass E.qNaN
      (genNaNDcd genSign (return E.Quiet) (payloadDigits decimalDigs))
      E.dIsNaN

    , testDecClass E.negInf
      (genInfinite (return E.Sign1)) E.dIsNegInf

    , testDecClass E.negNormal
      (genNormal (return E.Sign1)
        (sizedDigits E.coefficientLen decimalDigs)) E.dIsNegNormal

    , testDecClass E.negSubnormal
      (genSubnormal (return E.Sign1)
        (sizedDigits (E.coefficientLen - 1) decimalDigs))
        E.dIsNegSubnormal

    , testDecClass E.negZero genNegZero E.dIsNegZero
    , testDecClass E.posZero genPosZero E.dIsPosZero

    , testDecClass E.posSubnormal
      (genSubnormal (return E.Sign0)
        (sizedDigits (E.coefficientLen - 1) decimalDigs))
        E.dIsPosSubnormal

    , testDecClass E.posNormal
      (genNormal (return E.Sign0)
        (sizedDigits E.coefficientLen decimalDigs)) E.dIsPosNormal

    , testDecClass E.posInf
      (genInfinite (return E.Sign0)) E.dIsPosInf

    ] -- classes

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

  , testGroup "integer conversions"
    [ intConversion "int32" genInt32 E.fromInt32 E.toInt32
    , intConversion "uint32" genUInt32 E.fromUInt32 E.toUInt32
    , intConversion "int32 exact" genInt32 E.fromInt32 E.toInt32Exact
    , intConversion "uint32 exact" genUInt32 E.fromUInt32 E.toUInt32Exact
    ] -- integer conversions

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
        forAll genSmallFinite $ \da ->
        forAll genSmallFinite $ \db ->
        let (r, fl) = runFlags $ do
              let a = E.fromBCD da
                  b = E.fromBCD db
              r1 <- E.add a b
              r2 <- E.subtract r1 b
              c <- E.compare r2 a
              return $ E.isZero c
        in null fl ==> r

      , identity "zero" genZero E.subtract
      ]

    , testGroup "fused multiply add"
      [ testProperty "is same as multiply and add" $
        forAll genSmallFinite $ \da ->
        forAll genSmallFinite $ \db ->
        forAll genSmallFinite $ \dc ->
        let (r, fl) = runFlags $ do
              let a = E.fromBCD da
                  b = E.fromBCD db
                  c = E.fromBCD dc
              r1 <- E.multiply a b
              r2 <- E.add r1 c
              r2' <- E.fma a b c
              cm <- E.compare r2 r2'
              return $ E.isZero cm
        in null fl ==> r
      ]

    , testGroup "divide"
      [ identity "one" genOne E.divide ]

    , testGroup "divideInteger"
      [ testProperty "result has exponent 0" $
        forAll genSmallFinite $ \da ->
        forAll genSmallFinite $ \db ->
        let (e, fl) = runFlags $ do
              let a = E.fromBCD da
                  b = E.fromBCD db
              c <- E.divideInteger a b
              return $ E.isInteger c
        in null fl ==> e
      ]

    , testGroup "remainder"
      [ testProperty "x = int * y + rem" $
        forAll genSmallFinite $ \dx ->
        forAll genSmallFinite $ \dy ->
        let (r, fl) = runFlags $ do
              let x = E.fromBCD dx
                  y = E.fromBCD dy
              it <- E.divideInteger x y
              rm <- E.remainder x y
              i1 <- E.multiply it y
              i2 <- E.add i1 rm
              c <- E.compare i2 x
              return $ E.isZero c
        in null fl ==> r
      ]
      -- remainderNear - no test - not sure I understand the
      -- semantics

    ] -- arithmetic

  , testGroup "exponent and coefficient adjustment"
    [ testGroup "quantize"
      [ testProperty "result has same quantum" $
        forAll genSmallFinite $ \dx ->
        forAll genSmallFinite $ \dy ->
        let (r, fl) = runFlags $ do
              let x = E.fromBCD dx
                  y = E.fromBCD dy
              c <- E.quantize x y
              let getExp a = do
                    let dcd = E.toBCD a
                    return $ case E.dValue dcd of
                      E.Finite _ e -> Just e
                      _ -> Nothing
              exC <- getExp c
              exY <- getExp y
              let fin = E.isFinite c
              return $ fin && exC == exY
        in null fl ==> r
      ]

    , testGroup "reduce"
      [ testProperty "result is equivalent" $
        forAll genSmallFinite $ \dx -> E.runQuad $ do
            let x = E.fromBCD dx
            r <- E.reduce x
            c <- E.compare r x
            return $ E.isZero c

      , testProperty "result has no trailing zeroes" $
        forAll genSmallFinite $ \dx -> E.runQuad $ do
            let x = E.fromBCD dx
            r <- E.reduce x
            let dcd = E.toBCD r
            return $ case E.dValue dcd of
              E.Infinite -> False
              E.NaN _ _ -> False
              E.Finite c _ ->
                let digs = E.unCoefficient c
                in all (== E.D0) digs || last digs /= E.D0
      ]
    ] -- exponent and coefficient adjustment

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
