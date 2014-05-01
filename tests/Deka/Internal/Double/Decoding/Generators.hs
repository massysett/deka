module Deka.Internal.Quad.Decoding.Generators where

import Deka.Decoded
import Deka.Internal.Quad.Decoding
import Deka.Decoded.Generators
import Test.QuickCheck hiding (maxSize)
import Prelude hiding (exponent)
import Util

-- ## Infinite number generators

genInfinite :: Gen Sign -> Gen Decoded
genInfinite gs = do
  s <- gs
  return $ Decoded s Infinite

-- ## NaN number generators

payloadDigits :: (Gen Digit, Gen Digit) -> Gen [Digit]
payloadDigits = sizedDigits payloadLen

genNaN :: Gen NaN
genNaN = elements [ Quiet, Signaling ]

genNaNDcd
  :: Gen Sign
  -> Gen NaN
  -> Gen [Digit]
  -- ^ Generate payload
  -> Gen Decoded
genNaNDcd gs gn gd = do
  s <- gs
  ds <- gd
  n <- gn
  let pay = case payload ds of
        Nothing -> error "genNaNDcd: payload failed"
        Just r -> r
  return $ Decoded s (NaN n pay)

-- ## Finite number generators

coeffDigits :: (Gen Digit, Gen Digit) -> Gen [Digit]
coeffDigits p = sized f
  where
    f x | x == 0 = oneof [ sizedDigits 0 p, return [D0] ]
        | otherwise = sizedDigits coefficientLen p

genFiniteDcd
  :: Gen Sign
  -> Gen [Digit]
  -- ^ Generate coefficient
  -> (Coefficient -> Gen Int)
  -- ^ Generate exponent
  -> Gen Decoded
genFiniteDcd gs gc ge = do
  s <- gs
  ds <- gc
  let coe = case coefficient ds of
        Nothing -> error "genFinite: coefficient failed"
        Just r -> r
  e <- ge coe
  let ex = case exponent e of
        Nothing -> error "genFiniteDcd: exponent failed"
        Just r -> r
  return $ Decoded s (Finite coe ex)

rangedExponent
  :: (Int, Int)
  -- ^ Minimum and maximum exponent.  Exponent will never exceed
  -- allowable values.
  -> Gen Int
rangedExponent (em, ex) = do
  let (mPE, xPE) = minMaxExp
      (mR, xR) = (max em mPE, min ex xPE)
  choose (mR, xR)

sizedExponent :: Gen Int
sizedExponent = sized $ \s ->
  let x = s ^ (2 :: Int)
  in rangedExponent (negate x, x)

fullExpRange :: Gen Int
fullExpRange = rangedExponent minMaxExp

-- ## Decoded generators

-- | Most general Decoded generator.  Generates throughout the
-- possible range of Decoded.  Depends on the size parameter.
genDecoded :: Gen Decoded
genDecoded = frequency [(4, genFinite), (1, inf), (1, nan)]
  where
    inf = genInfinite genSign
    nan = genNaNDcd genSign genNaN (payloadDigits decimalDigs)

-- | Generates finite decoded numbers.
genFinite :: Gen Decoded
genFinite = genFiniteDcd genSign (coeffDigits decimalDigs)
            (const sizedExponent)
 

-- ## Specialized finite generators

-- | Generates positive and negative zeroes.
genZero :: Gen Decoded
genZero = genFiniteDcd genSign (return [D0]) (const fullExpRange)

genNegZero :: Gen Decoded
genNegZero = genFiniteDcd (return Neg) (return [D0])
  (const fullExpRange)

genPosZero :: Gen Decoded
genPosZero = genFiniteDcd (return NonNeg) (return [D0])
  (const fullExpRange)

-- | Generates positive one.
genOne :: Gen Decoded
genOne = genFiniteDcd (return NonNeg) gDigs gExp
  where
    gDigs = sizedDigits coefficientLen (return D1, return D0)
    gExp co = return . negate $ length (unCoefficient co) - 1

genSmallFinite :: Gen Decoded
genSmallFinite = maxSize 5 genFinite

-- | Generates two values that are equivalent, but with
-- different exponents.

genEquivalent :: Gen (Decoded, Decoded)
genEquivalent = do
  let genCoeff1 = sizedDigits (coefficientLen - 1) decimalDigs
      genExp1 c =
        let (l, h) = minMaxExp
            l' = l + (coefficientLen - (length . unCoefficient $ c))
        in choose (l', h)
  d1 <- genFiniteDcd genSign genCoeff1 genExp1
  let (c1, e1) = case dValue d1 of
        Finite c e -> (unCoefficient c, unExponent e)
        _ -> error "genEquivalent failed"
      maxMore = coefficientLen - length c1
  more <- choose (1, maxMore)
  let coeff2 = case coefficient (c1 ++ replicate more D0) of
        Nothing -> error "genEquivalent: coefficient failed"
        Just r -> r
      exp2 = case exponent (e1 - more) of
        Nothing -> error "genEquivalent: exponent failed"
        Just r -> r
      d2 = Decoded (dSign d1) (Finite coeff2 exp2)
  b <- arbitrary
  let r = if b then (d1, d2) else (d2, d1)
  return r



genNonZeroSmallFinite :: Gen Decoded
genNonZeroSmallFinite = maxSize 5 $ genFiniteDcd genSign
  gd ge
  where
    gd = sizedDigits coefficientLen decimalDigs
    ge = (const sizedExponent)

genInteger :: Gen Decoded
genInteger = genFiniteDcd genSign
  (coeffDigits decimalDigs) (const . return $ 0)

genLogical :: Gen Decoded
genLogical = genFiniteDcd (return NonNeg)
  (coeffDigits binaryDigs) (const . return $ 0)

genNormal :: Gen Sign -> Gen [Digit] -> Gen Decoded
genNormal gs gc = genFiniteDcd gs gc ge
  where
    ge c = do
      let minNrml = unExponent $ minNormalExp c
          maxE = snd minMaxExp
      choose (minNrml, maxE)

genSubnormal :: Gen Sign -> Gen [Digit] -> Gen Decoded
genSubnormal gs gd = genFiniteDcd gs gd ge
  where
    ge c =
      let minNrml = unExponent . minNormalExp $ c
          minE = fst minMaxExp
          f | minE > minNrml - 1 = error "genSubnormal failed"
            | otherwise = choose (minE, minNrml - 1)
      in f

genPositive :: Gen Decoded
genPositive = genFiniteDcd (return NonNeg) gd ge
  where
    gd = sizedDigits coefficientLen decimalDigs
    ge = (const sizedExponent)

genNegative :: Gen Decoded
genNegative = genFiniteDcd (return Neg) gd ge
  where
    gd = sizedDigits coefficientLen decimalDigs
    ge = (const sizedExponent)

-- ## Specialized other generators

genSignaling :: Gen Decoded
genSignaling = genNaNDcd genSign (return Signaling)
  (payloadDigits decimalDigs)

genSigned :: Gen Decoded
genSigned = oneof
  [ genFiniteDcd (return Neg) (coeffDigits decimalDigs) (const sizedExponent)
  , genNaNDcd (return Neg) genNaN (payloadDigits decimalDigs)
  , genInfinite (return Neg)
  ]

