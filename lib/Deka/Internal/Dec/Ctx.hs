{-# LANGUAGE Safe #-}
module Deka.Internal.Dec.Ctx where

import qualified Data.ByteString.Char8 as BS8
import Deka.Internal.Context
import Deka.Internal.Dec.Util
import Deka.Internal.Dec.CtxFree
import Deka.Internal.Decnumber.DecNumber
import Deka.Internal.Decnumber.Context
import Deka.Internal.Dec.Dec
import Deka.Internal.Class
import Foreign.Safe
import Deka.Decoded

-- | Converts a character string to a 'Dec'.  Implements the
-- _to-number_ conversion from the General Decimal Arithmetic
-- specification.
--
-- The conversion is exact provided that the numeric string has no
-- more significant digits than are specified in the 'Precision' in
-- the 'Ctx' and the adjusted exponent is in the range specified by
-- 'Emin' and 'Emax' in the 'Ctx'. If there are more than
-- 'Precision' digits in the string, or the exponent is out of
-- range, the value will be rounded as necessary using the 'Round'
-- rounding mode. The 'Precision' therefore both determines the
-- maximum precision for unrounded numbers and defines the minimum
-- size of the 'Dec' structure required.
--
-- Possible errors are 'conversionSyntax' (the string does not have
-- the syntax of a number, which depends on 'setExtended' in the
-- 'Ctx'), 'overflow' (the adjusted exponent of the number is larger
-- than 'Emax'), or 'underflow' (the adjusted exponent is less than
-- 'Emin' and the conversion is not exact). If any of these
-- conditions are set, the number structure will have a defined
-- value as described in the arithmetic specification (this may be a
-- subnormal or infinite value).

fromByteString :: BS8.ByteString -> Ctx Dec
fromByteString bs = Ctx $ \pCtx ->
  newDec pCtx >>= \dn ->
  withForeignPtr (unDec dn) $ \pDn ->
  BS8.useAsCString bs $ \cstr ->
  c'decNumberFromString pDn cstr pCtx >>
  return dn

-- | Convert a 'Dec' to an unsigned 32-bit integer.
toUInt32 :: Dec -> Ctx Word32
toUInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDec dn) $ \pDn ->
  c'decNumberToUInt32 pDn pCtx

-- | Convert a 'Dec' to a signed 32-bit integer.
toInt32 :: Dec -> Ctx Int32
toInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDec dn) $ \pDn ->
  c'decNumberToInt32 pDn pCtx

type Unary
  = Ptr C'decNumber
  -- ^ Result
  -> Ptr C'decNumber
  -- ^ Input
  -> Ptr C'decContext
  -> IO (Ptr C'decNumber)

unary :: Unary -> Dec -> Ctx Dec
unary f x = Ctx $ \pCtx ->
  newDec pCtx >>= \res ->
  withForeignPtr (unDec res) $ \pRes ->
  withForeignPtr (unDec x) $ \pX ->
  f pRes pX pCtx >>
  return res

type Binary
   = Ptr C'decNumber
   -- ^ Result
   -> Ptr C'decNumber
   -- ^ Input X
   -> Ptr C'decNumber
   -- ^ Input Y
   -> Ptr C'decContext
   -> IO (Ptr C'decNumber)

binary :: Binary -> Dec -> Dec -> Ctx Dec
binary f x y = Ctx $ \pCtx ->
  newDec pCtx >>= \res ->
  withForeignPtr (unDec res) $ \pRes ->
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  f pRes pX pY pCtx >>
  return res

type Ternary
   = Ptr C'decNumber
   -- ^ Result
   -> Ptr C'decNumber
   -- ^ Input X
   -> Ptr C'decNumber
   -- ^ Input Y
   -> Ptr C'decNumber
   -- ^ Input Z
   -> Ptr C'decContext
   -> IO (Ptr C'decNumber)

ternary :: Ternary -> Dec -> Dec -> Dec -> Ctx Dec
ternary f x y z = Ctx $ \pCtx ->
  newDec pCtx >>= \res ->
  withForeignPtr (unDec res) $ \pRes ->
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  withForeignPtr (unDec z) $ \pZ ->
  f pRes pX pY pZ pCtx >>
  return res

-- | Returns the absolute value.  The same effect as 'plus' unless
-- the operand is negative, in which case it is the same as 'minus'.
abs :: Dec -> Ctx Dec
abs = unary c'decNumberAbs

-- | Addition.
add :: Dec -> Dec -> Ctx Dec
add = binary c'decNumberAdd

-- | Digit-wise logical _and_.
and :: Dec -> Dec -> Ctx Dec
and = binary c'decNumberAnd

-- | @compare x y@ compares to numbers numerically.  If @x@ is less
-- than @y@, returns @-1@.  If they are equal (that is, when
-- subtracted the result would be 0), returns @0@.  If @y@ is
-- greater than @x@, returns @1@.  If the operands are not
-- comparable (that is, one or both is a NaN) the result is NaN.
compare :: Dec -> Dec -> Ctx Dec
compare = binary c'decNumberCompare

-- | Identical to 'compare' except that all NaNs (including quiet
-- NaNs) signal.
compareSignal :: Dec -> Dec -> Ctx Dec
compareSignal = binary c'decNumberCompareSignal

-- | @compareTotal x y@ compares to numbers using the IEEE 754 total
-- ordering.  If @x@ is less
-- than @y@, returns @-1@.  If they are equal (that is, when
-- subtracted the result would be 0), returns @0@.  If @y@ is
-- greater than @x@, returns @1@.  
--
-- Here is the total ordering:
--
-- @-NaN < -sNaN < -Infinity < -finites < -0 < +0 < +finites
--  < +Infinity < +SNaN < +NaN@
--
-- Also, @1.000@ < @1.0@ (etc.) and NaNs are ordered by payload.
compareTotal :: Dec -> Dec -> Ctx Dec
compareTotal = binary c'decNumberCompareTotal

-- | Same as 'compareTotal' except that the signs of the operands
-- are ignored and taken to be 0 (non-negative).
compareTotalMag :: Dec -> Dec -> Ctx Dec
compareTotalMag = binary c'decNumberCompareTotalMag

-- | Division.
divide :: Dec -> Dec -> Ctx Dec
divide = binary c'decNumberDivide

-- | Returns the integer part of the result of division.  It must be
-- possible to express the result as an integer.  That is, it must
-- have no more digits than 'Precision' in the 'Ctx'.  If it does
-- then 'divisionImpossible' is raised.
divideInteger :: Dec -> Dec -> Ctx Dec
divideInteger = binary c'decNumberDivideInteger

-- | Exponentiation.  Result is rounded if necessary using the
-- 'Precision' in the 'Ctx' and using the 'roundHalfEven' rounding
-- method.
--
-- Finite results will always be full precision and inexact, except
-- when rhs is a zero or -Infinity (giving 1 or 0 respectively).
-- Inexact results will almost always be correctly rounded, but may
-- be up to 1 ulp (unit in last place) in error in rare cases.
--
-- This is a mathematical function; the @10 ^ 6@ restrictions on
-- precision and range apply as described above.
exp :: Dec -> Ctx Dec
exp = unary c'decNumberExp

-- @fma x y z@ multiplies @x@ by @y@ and then adds @z@ to that
-- intermediate result.  It is equivalent to a multiplication
-- followed by an addition except that the intermediate result is
-- not rounded and will not cause overflow or underflow. That is,
-- only the final result is rounded and checked.
--
-- This is a mathematical function; the @10 ^ 6@ restrictions on
-- precision and range apply as described above.
fma :: Dec -> Dec -> Dec -> Ctx Dec
fma = ternary c'decNumberFMA

-- | Digit-wise inversion (a @0@ becomes a @1@ and vice versa).
invert :: Dec -> Ctx Dec
invert = unary c'decNumberInvert

-- | Natural logarithm, rounded if necessary using the 'Precision'
-- in the 'Ctx' and using the 'roundHalfEven' rounding algorithm.
-- The operand must be positive or a zero.
--
-- Finite results will always be full precision and inexact, except
-- when rhs is equal to 1, which gives an exact result of 0. Inexact
-- results will almost always be correctly rounded, but may be up to
-- 1 ulp (unit in last place) in error in rare cases.
--
-- This is a mathematical function; the @10 ^ 6@ restrictions on
-- precision and range apply as described above.
ln :: Dec -> Ctx Dec
ln = unary c'decNumberLn

-- | Returns the adjusted exponent of the operand, according to the
-- rules for @logB@ of IEEE 754.  This returns the exponent of the
-- operand as though its decimal point had been moved to follow the
-- first digit while keeping the same value.  The result is not
-- limited by 'Emin' or 'Emax'.
logB :: Dec -> Ctx Dec
logB = unary c'decNumberLogB

-- | Base 10 logarithm, rounded if necessary using the 'Precision'
-- in the 'Ctx' and using the 'roundHalfEven' rounding algorithm.
-- The operand must be positive or a zero.
--
-- Finite results will always be full precision and inexact, except
-- when rhs is equal to an integral power of ten, in which case the
-- result is the exact integer.
--
-- Inexact results will almost always be correctly rounded, but may
-- be up to 1 ulp (unit in last place) in error in rare cases.
--
-- This is a mathematical function; the @10 ^ 6@ restrictions on
-- precision and range apply as described above.

log10 :: Dec -> Ctx Dec
log10 = unary c'decNumberLog10

-- | Compares two numbers numerically and returns the larger.  If
-- the numbers compare equal then number is chosen with regard to
-- sign and exponent. Unusually, if one operand is a quiet NaN and
-- the other a number, then the number is returned.
max :: Dec -> Dec -> Ctx Dec
max = binary c'decNumberMax

-- | Compares the magnitude of two numbers numerically and sets
-- number to the larger. It is identical to 'max' except that
-- the signs of the operands are ignored and taken to be 0
-- (non-negative).
maxMag :: Dec -> Dec -> Ctx Dec
maxMag = binary c'decNumberMaxMag

-- | Compares two numbers numerically and sets number to the
-- smaller. If the numbers compare equal then number is chosen with
-- regard to sign and exponent. Unusually, if one operand is a quiet
-- NaN and the other a number, then the number is returned.
min :: Dec -> Dec -> Ctx Dec
min = binary c'decNumberMin

-- | Compares the magnitude of two numbers numerically and sets
-- number to the smaller. It is identical to 'min' except
-- that the signs of the operands are ignored and taken to be 0
-- (non-negative).
minMag :: Dec -> Dec -> Ctx Dec
minMag = binary c'decNumberMinMag

-- | Returns the result of subtracting the operand from zero.  hat
-- is, it is negated, following the usual arithmetic rules; this may
-- be used for implementing a prefix minus operation.
minus :: Dec -> Ctx Dec
minus = unary c'decNumberMinus

-- | Multiplication.
multiply :: Dec -> Dec -> Ctx Dec
multiply = binary c'decNumberMultiply

-- | Digit-wise logical inclusive or.
or :: Dec -> Dec -> Ctx Dec
or = binary c'decNumberOr

-- | Returns the result of adding the operand to zero.  This takes
-- place according to the settings given in the 'Ctx', following the
-- usual arithmetic rules. This may therefore be used for rounding
-- or for implementing a prefix plus operation.
plus :: Dec -> Ctx Dec
plus = unary c'decNumberPlus

power :: Dec -> Dec -> Ctx Dec
power = binary c'decNumberPower

quantize :: Dec -> Dec -> Ctx Dec
quantize = binary c'decNumberQuantize

reduce :: Dec -> Ctx Dec
reduce = unary c'decNumberReduce

remainder :: Dec -> Dec -> Ctx Dec
remainder = binary c'decNumberRemainder

remainderNear :: Dec -> Dec -> Ctx Dec
remainderNear = binary c'decNumberRemainderNear

rescale :: Dec -> Dec -> Ctx Dec
rescale = binary c'decNumberRescale

rotate :: Dec -> Dec -> Ctx Dec
rotate = binary c'decNumberRotate

scaleB :: Dec -> Dec -> Ctx Dec
scaleB = binary c'decNumberScaleB

shift :: Dec -> Dec -> Ctx Dec
shift = binary c'decNumberShift

squareRoot :: Dec -> Ctx Dec
squareRoot = unary c'decNumberSquareRoot

subtract :: Dec -> Dec -> Ctx Dec
subtract = binary c'decNumberSubtract

toIntegralExact :: Dec -> Ctx Dec
toIntegralExact = unary c'decNumberToIntegralExact

toIntegralValue :: Dec -> Ctx Dec
toIntegralValue = unary c'decNumberToIntegralValue

xor :: Dec -> Dec -> Ctx Dec
xor = binary c'decNumberXor

nextMinus :: Dec -> Ctx Dec
nextMinus = unary c'decNumberNextMinus

nextPlus :: Dec -> Ctx Dec
nextPlus = unary c'decNumberNextPlus

nextToward :: Dec -> Dec -> Ctx Dec
nextToward = binary c'decNumberNextToward

numClass :: Dec -> Ctx Class
numClass (Dec fp) = Ctx $ \pCtx ->
  withForeignPtr fp $ \pd ->
  c'decNumberClass pd pCtx >>= \cl ->
  return (Class cl)

isNormal :: Dec -> Ctx Bool
isNormal (Dec d) = Ctx $ \pCtx ->
  withForeignPtr d $ \pd ->
  c'decNumberIsNormal pd pCtx >>= \int ->
  return (toBool int)

isSubnormal :: Dec -> Ctx Bool
isSubnormal (Dec d) = Ctx $ \pCtx ->
  withForeignPtr d $ \pd ->
  c'decNumberIsSubnormal pd pCtx >>= \int ->
  return (toBool int)

-- | Encodes non-special numbers (also known as finite numbers.)
-- Uses information from the context to determine whether subnormal
-- values are allowed.
nonSpecial
  :: Sign
  -> Coefficient
  -> Exponent
  -> Ctx (Maybe Dec)
  -- ^ Fails if the exponent is out of range
nonSpecial sgn coe rawEx = Ctx $ \pCtx -> do
  ext <- fmap toBool . peek . p'decContext'extended $ pCtx
  let getPc | ext = fmap (Just . Precision) . peek
                . p'decContext'digits $ pCtx
            | otherwise = return Nothing
  pc <- getPc
  nonSpecialCtxFree pc sgn coe rawEx
