{-# LANGUAGE Safe #-}
module Deka.Internal.Double.Ctx where

import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Foreign.Safe hiding
  ( void
  , isSigned
  , rotate
  , shift
  , xor
  )
import Prelude hiding
  ( abs
  , and
  , compare
  , isInfinite
  , isNaN
  , max
  , min
  , or
  , subtract
  , significand
  , exponent
  , Double
  )
import Deka.Context
import Deka.Internal.Context

import Deka.Internal.DecNum.DecNum
import Deka.Internal.Decnumber.DecDouble
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.Decimal64
import Deka.Internal.Double.Double

type Unary
  = Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

unary
  :: Unary
  -> Double
  -> Ctx Double
unary f d = Ctx $ \ptrC ->
  newDouble >>= \r ->
  withForeignPtr (unDouble d) $ \ptrX ->
  withForeignPtr (unDouble r) $ \ptrR ->
  f ptrR ptrX ptrC >>
  return r

type Binary
  = Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

binary
  :: Binary
  -> Double
  -> Double
  -> Ctx Double
binary f x y = Ctx $ \pC ->
  newDouble >>= \r ->
  withForeignPtr (unDouble r) $ \pR ->
  withForeignPtr (unDouble x) $ \pX ->
  withForeignPtr (unDouble y) $ \pY ->
  f pR pX pY pC >>
  return r

type Ternary
  = Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

ternary
  :: Ternary
  -> Double
  -> Double
  -> Double
  -> Ctx Double
ternary f x y z = Ctx $ \pC ->
  newDouble >>= \r ->
  withForeignPtr (unDouble r) $ \pR ->
  withForeignPtr (unDouble x) $ \pX ->
  withForeignPtr (unDouble y) $ \pY ->
  withForeignPtr (unDouble z) $ \pZ ->
  f pR pX pY pZ pC
  >> return r

type GetRounded a
  = Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO a

getRounded
  :: GetRounded a
  -> Round
  -> Double
  -> Ctx a
getRounded f (Round r) d = Ctx $ \pC ->
  withForeignPtr (unDouble d) $ \pD ->
  f pD pC r

-- # End Helpers

-- | Absolute value.  NaNs are handled normally (the sign of an NaN
-- is not affected, and an sNaN sets 'invalidOperation'.
abs :: Double -> Ctx Double
abs = unary c'decDoubleAbs

add :: Double -> Double -> Ctx Double
add = binary c'decDoubleAdd

-- | Digit-wise logical and.  Operands must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
and :: Double -> Double -> Ctx Double
and = binary c'decDoubleAnd

-- | Compares two 'Double' numerically.  The result might be @-1@, @0@,
-- @1@, or NaN, where @-1@ means x is less than y, @0@ indicates
-- numerical equality, @1@ means y is greater than x.  NaN is
-- returned only if x or y is an NaN.
--
-- Thus, this function does not return an 'Ordering' because the
-- result might be an NaN.
--
compare :: Double -> Double -> Ctx Double
compare = binary c'decDoubleCompare

-- | Same as 'compare', but a quietNaN is treated like a signaling
-- NaN (sets 'invalidOperation').
compareSignal :: Double -> Double -> Ctx Double
compareSignal = binary c'decDoubleCompareSignal

divide :: Double -> Double -> Ctx Double
divide = binary c'decDoubleDivide

-- | @divideInteger x y@ returns the integer part of the result
-- (rounded toward zero), with an exponent of 0.  If the the result
-- would not fit because it has too many digits,
-- 'divisionImpossible' is set.
divideInteger :: Double -> Double -> Ctx Double
divideInteger = binary c'decDoubleDivideInteger

-- | Fused multiply add; @fma x y z@ calculates @x * y + z@.  The
-- multiply is carried out first and is exact, so the result has
-- only one final rounding.
fma :: Double -> Double -> Double -> Ctx Double
fma = ternary c'decDoubleFMA

-- | Reads a ByteString, which can be in scientific, engineering, or
-- \"regular\" decimal notation.  Also reads NaN, Infinity, etc.
-- Will return a signaling NaN and set 'invalidOperation' if the
-- string given is invalid.
--
-- In the decNumber C library, this function was called
-- @fromString@; the name was changed here because it doesn't take a
-- regular Haskell 'String'.
fromByteString :: BS8.ByteString -> Ctx Double
fromByteString s = Ctx $ \pC ->
  newDouble >>= \r ->
  withForeignPtr (unDouble r) $ \pR ->
  BS8.useAsCString s $ \pS ->
  c'decDoubleFromString pR pS pC >>
  return r

-- | Digit-wise logical inversion.  The operand must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
invert :: Double -> Ctx Double
invert = unary c'decDoubleInvert

-- | @logB x@ Returns the adjusted exponent of x, according to IEEE
-- 754 rules.  If @x@ is infinite, returns +Infinity.  If @x@ is
-- zero, the result is -Infinity, and 'divisionByZero' is set.  If
-- @x@ is less than zero, the absolute value of @x@ is used.  If @x@
-- is one, the result is 0.  NaNs are propagated as for arithmetic
-- operations.
logB :: Double -> Ctx Double
logB = unary c'decDoubleLogB

-- | @max x y@ returns the larger argument; if either (but not both)
-- @x@ or @y@ is a quiet NaN then the other argument is the result;
-- otherwise, NaNs, are handled as for arithmetic operations.
max :: Double -> Double -> Ctx Double
max = binary c'decDoubleMax

-- | Like 'max' but the absolute values of the arguments are used.
maxMag :: Double -> Double -> Ctx Double
maxMag = binary c'decDoubleMaxMag

-- | @min x y@ returns the smaller argument; if either (but not both)
-- @x@ or @y@ is a quiet NaN then the other argument is the result;
-- otherwise, NaNs, are handled as for arithmetic operations.
min :: Double -> Double -> Ctx Double
min = binary c'decDoubleMin

-- | Like 'min' but the absolute values of the arguments are used.
minMag :: Double -> Double -> Ctx Double
minMag = binary c'decDoubleMinMag

-- | Negation.  Result has the same effect as @0 - x@ when the
-- exponent of the zero is the same as that of @x@, if @x@ is
-- finite.
minus :: Double -> Ctx Double
minus = unary c'decDoubleMinus

multiply :: Double -> Double -> Ctx Double
multiply = binary c'decDoubleMultiply

-- | Decrements toward negative infinity.
nextMinus :: Double -> Ctx Double
nextMinus = unary c'decDoubleNextMinus

-- | Increments toward positive infinity.
nextPlus :: Double -> Ctx Double
nextPlus = unary c'decDoubleNextPlus

-- | @nextToward x y@ returns the next 'Double' in the direction of
-- @y@.
nextToward :: Double -> Double -> Ctx Double
nextToward = binary c'decDoubleNextToward

-- | Digit wise logical inclusive Or.  Operands must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
or :: Double -> Double -> Ctx Double
or = binary c'decDoubleOr

-- | Same effect as @0 + x@ where the exponent of the zero is the
-- same as that of @x@ if @x@ is finite).  NaNs are handled as for
-- arithmetic operations.
plus :: Double -> Ctx Double
plus = unary c'decDoublePlus

-- | @quantize x y@ returns @z@ which is @x@ set to have the same
-- quantum as @y@; that is, numerically the same value but rounded
-- or padded if necessary to have the same exponent as @y@.  Useful
-- for rounding monetary quantities.
quantize :: Double -> Double -> Ctx Double
quantize = binary c'decDoubleQuantize

-- | Reduces coefficient to its shortest possible form without
-- changing the value of the result by removing all possible
-- trailing zeroes.
reduce :: Double -> Ctx Double
reduce = unary c'decDoubleReduce

-- | Remainder from integer division.  If the intermediate integer
-- does not fit within a Double, 'divisionImpossible' is raised.
remainder :: Double -> Double -> Ctx Double
remainder = binary c'decDoubleRemainder

-- | Like 'remainder' but the nearest integer is used for for the
-- intermediate result instead of the result from 'divideInteger'.
remainderNear :: Double -> Double -> Ctx Double
remainderNear = binary c'decDoubleRemainderNear

-- | @rotate x y@ rotates the digits of x to the left (if @y@ is
-- positive) or right (if @y@ is negative) without adjusting the
-- exponent or sign of @x@.  @y@ is the number of positions to
-- rotate and must be in the range @negate 'coefficientLen'@ to
-- @'coefficentLen'@.
--
-- NaNs are propagated as usual.  No status is set unless @y@ is
-- invalid or an operand is an NaN.
rotate :: Double -> Double -> Ctx Double
rotate = binary c'decDoubleRotate

-- | @scaleB x y@ calculates @x * 10 ^ y@.  @y@ must be an integer
-- (finite with exponent of 0) in the range of plus or minus @2 *
-- 'coefficientLen' + 'coefficientLen')@, typically resulting from
-- 'logB'.  Underflow and overflow might occur; NaNs propagate as
-- usual.
scaleB :: Double -> Double -> Ctx Double
scaleB = binary c'decDoubleScaleB

-- | @shift x y@ shifts digits the digits of x to the left (if @y@
-- is positive) or right (if @y@ is negative) without adjusting the
-- exponent or sign of @x@.  Any digits shifted in from the left or
-- right will be 0.
--
-- @y@ is a count of positions to shift; it must be a finite
-- integer in the range @negate 'coefficientLen'@ to
-- 'coefficientLen'.  NaNs propagate as usual.  If @x@ is infinite
-- the result is an infinity of the same sign.  No status is set
-- unless y is invalid or the operand is an NaN.
shift :: Double -> Double -> Ctx Double
shift = binary c'decDoubleShift

-- omitted: Show

subtract :: Double -> Double -> Ctx Double
subtract = binary c'decDoubleSubtract

-- | Uses the rounding method given rather than the one in the
-- 'Ctx'.  If the operand is infinite, an NaN, or if the result of
-- rounding is outside the range of a 'Int32', then
-- 'invalidOperation' is set.  'inexact' is not set even if rounding
-- occurred.
toInt32 :: Round -> Double -> Ctx Int32
toInt32 = getRounded c'decDoubleToInt32

-- | Like 'toInt32' but if rounding removes non-zero digits then
-- 'inexact' is set.
toInt32Exact :: Round -> Double -> Ctx Int32
toInt32Exact = getRounded c'decDoubleToInt32Exact

-- | Rounds to an integral using the rounding mode set in the 'Ctx'.
-- If the operand is infinite, an infinity of the same sign is
-- returned.  If the operand is an NaN, the result is the same as
-- for other arithmetic operations.  If rounding removes non-zero
-- digits then 'inexact' is set.
toIntegralExact :: Double -> Ctx Double
toIntegralExact = unary c'decDoubleToIntegralExact

-- | @toIntegralValue r x@ returns an integral value of @x@ using
-- the rounding mode @r@ rather than the one specified in the 'Ctx'.
-- If the operand is an NaN, the result is the same as for other
-- arithmetic operations.  'inexact' is not set even if rounding
-- occurred.
toIntegralValue :: Round -> Double -> Ctx Double
toIntegralValue (Round rnd) d = Ctx $ \pC ->
  withForeignPtr (unDouble d) $ \pD ->
  newDouble >>= \r ->
  withForeignPtr (unDouble r) $ \pR ->
  c'decDoubleToIntegralValue pR pD pC rnd >>
  return r

-- toByteString - moved to Internal so that Double can Show in a
-- non-orphan instance

-- | @toUInt32 r x@ returns the value of @x@, rounded to an integer
-- if necessary using the rounding mode @r@ rather than the one
-- given in the 'Ctx'.  If @x@ is infinite, or outside of the range
-- of a 'Word32', then 'invalidOperation' is set.  'inexact' is
-- not set even if rounding occurs.
--
-- The negative zero converts to 0 and is valid, but negative
-- numbers are not valid.
toUInt32 :: Round -> Double -> Ctx Word32
toUInt32 = getRounded c'decDoubleToUInt32

-- | Same as 'toUInt32' but if rounding removes non-zero digits then
-- 'inexact' is set.
toUInt32Exact :: Round -> Double -> Ctx Word32
toUInt32Exact = getRounded c'decDoubleToUInt32Exact

-- | Runs a computation with the 'initDouble' default context.
runDouble :: Ctx a -> a
runDouble = Deka.Context.runCtx initDouble

-- | Runs a computation with the decimal64 default context, and
-- returns any resulting flags.
runDoubleStatus :: Ctx a -> (a, [Flag])
runDoubleStatus a = runDouble $ do
  r <- a
  f <- getStatus
  return (r, f)

-- | Digit-wise logical exclusive or.  Operands must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.

xor :: Double -> Double -> Ctx Double
xor = binary c'decDoubleXor

-- Conversion from decNumber

-- | Converts a 'DecNum' to a 'Double'.  The possible errors are the
-- same as for the 'fromByteString' function, except that
-- 'conversionSyntax' is not possible.

fromNumber :: DecNum -> Ctx Double
fromNumber (DecNum fpd) = Ctx $ \pCtx ->
  newDouble >>= \r ->
  withForeignPtr (unDouble r) $ \pR ->
  withForeignPtr fpd $ \pDn ->
  c'decimal64FromNumber (downcast pR) pDn pCtx >>
  return r

downcast :: Ptr C'decDouble -> Ptr C'decimal64
downcast = castPtr
