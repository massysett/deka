{-# LANGUAGE Safe #-}
module Deka.Internal.Quad.Ctx where

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
  )
import Deka.Context
import Deka.Internal.Context

import Deka.Internal.DecNum.DecNum
import Deka.Internal.Decnumber.DecQuad
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.Types
import Deka.Internal.Decnumber.Decimal128
import Deka.Internal.Quad.Quad

type Unary
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

unary
  :: Unary
  -> Quad
  -> Ctx Quad
unary f d = Ctx $ \ptrC ->
  newQuad >>= \r ->
  withForeignPtr (unQuad d) $ \ptrX ->
  withForeignPtr (unQuad r) $ \ptrR ->
  f ptrR ptrX ptrC >>
  return r

type Binary
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

binary
  :: Binary
  -> Quad
  -> Quad
  -> Ctx Quad
binary f x y = Ctx $ \pC ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  f pR pX pY pC >>
  return r

type Ternary
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

ternary
  :: Ternary
  -> Quad
  -> Quad
  -> Quad
  -> Ctx Quad
ternary f x y z = Ctx $ \pC ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  withForeignPtr (unQuad z) $ \pZ ->
  f pR pX pY pZ pC
  >> return r

type GetRounded a
  = Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO a

getRounded
  :: GetRounded a
  -> Round
  -> Quad
  -> Ctx a
getRounded f (Round r) d = Ctx $ \pC ->
  withForeignPtr (unQuad d) $ \pD ->
  f pD pC r

-- # End Helpers

-- | Absolute value.  NaNs are handled normally (the sign of an NaN
-- is not affected, and an sNaN sets 'invalidOperation'.
abs :: Quad -> Ctx Quad
abs = unary unsafe'c'decQuadAbs

add :: Quad -> Quad -> Ctx Quad
add = binary unsafe'c'decQuadAdd

-- | Digit-wise logical and.  Operands must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
and :: Quad -> Quad -> Ctx Quad
and = binary unsafe'c'decQuadAnd

-- | Compares two 'Quad' numerically.  The result might be @-1@, @0@,
-- @1@, or NaN, where @-1@ means x is less than y, @0@ indicates
-- numerical equality, @1@ means y is greater than x.  NaN is
-- returned only if x or y is an NaN.
--
-- Thus, this function does not return an 'Ordering' because the
-- result might be an NaN.
--
compare :: Quad -> Quad -> Ctx Quad
compare = binary unsafe'c'decQuadCompare

-- | Same as 'compare', but a quietNaN is treated like a signaling
-- NaN (sets 'invalidOperation').
compareSignal :: Quad -> Quad -> Ctx Quad
compareSignal = binary unsafe'c'decQuadCompareSignal

divide :: Quad -> Quad -> Ctx Quad
divide = binary unsafe'c'decQuadDivide

-- | @divideInteger x y@ returns the integer part of the result
-- (rounded toward zero), with an exponent of 0.  If the the result
-- would not fit because it has too many digits,
-- 'divisionImpossible' is set.
divideInteger :: Quad -> Quad -> Ctx Quad
divideInteger = binary unsafe'c'decQuadDivideInteger

-- | Fused multiply add; @fma x y z@ calculates @x * y + z@.  The
-- multiply is carried out first and is exact, so the result has
-- only one final rounding.
fma :: Quad -> Quad -> Quad -> Ctx Quad
fma = ternary unsafe'c'decQuadFMA

-- | Reads a ByteString, which can be in scientific, engineering, or
-- \"regular\" decimal notation.  Also reads NaN, Infinity, etc.
-- Will return a signaling NaN and set 'invalidOperation' if the
-- string given is invalid.
--
-- In the decNumber C library, this function was called
-- @fromString@; the name was changed here because it doesn't take a
-- regular Haskell 'String'.
fromByteString :: BS8.ByteString -> Ctx Quad
fromByteString s = Ctx $ \pC ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  BS8.useAsCString s $ \pS ->
  unsafe'c'decQuadFromString pR pS pC >>
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
invert :: Quad -> Ctx Quad
invert = unary unsafe'c'decQuadInvert

-- | @logB x@ Returns the adjusted exponent of x, according to IEEE
-- 754 rules.  If @x@ is infinite, returns +Infinity.  If @x@ is
-- zero, the result is -Infinity, and 'divisionByZero' is set.  If
-- @x@ is less than zero, the absolute value of @x@ is used.  If @x@
-- is one, the result is 0.  NaNs are propagated as for arithmetic
-- operations.
logB :: Quad -> Ctx Quad
logB = unary unsafe'c'decQuadLogB

-- | @max x y@ returns the larger argument; if either (but not both)
-- @x@ or @y@ is a quiet NaN then the other argument is the result;
-- otherwise, NaNs, are handled as for arithmetic operations.
max :: Quad -> Quad -> Ctx Quad
max = binary unsafe'c'decQuadMax

-- | Like 'max' but the absolute values of the arguments are used.
maxMag :: Quad -> Quad -> Ctx Quad
maxMag = binary unsafe'c'decQuadMaxMag

-- | @min x y@ returns the smaller argument; if either (but not both)
-- @x@ or @y@ is a quiet NaN then the other argument is the result;
-- otherwise, NaNs, are handled as for arithmetic operations.
min :: Quad -> Quad -> Ctx Quad
min = binary unsafe'c'decQuadMin

-- | Like 'min' but the absolute values of the arguments are used.
minMag :: Quad -> Quad -> Ctx Quad
minMag = binary unsafe'c'decQuadMinMag

-- | Negation.  Result has the same effect as @0 - x@ when the
-- exponent of the zero is the same as that of @x@, if @x@ is
-- finite.
minus :: Quad -> Ctx Quad
minus = unary unsafe'c'decQuadMinus

multiply :: Quad -> Quad -> Ctx Quad
multiply = binary unsafe'c'decQuadMultiply

-- | Decrements toward negative infinity.
nextMinus :: Quad -> Ctx Quad
nextMinus = unary unsafe'c'decQuadNextMinus

-- | Increments toward positive infinity.
nextPlus :: Quad -> Ctx Quad
nextPlus = unary unsafe'c'decQuadNextPlus

-- | @nextToward x y@ returns the next 'Quad' in the direction of
-- @y@.
nextToward :: Quad -> Quad -> Ctx Quad
nextToward = binary unsafe'c'decQuadNextToward

-- | Digit wise logical inclusive Or.  Operands must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
or :: Quad -> Quad -> Ctx Quad
or = binary unsafe'c'decQuadOr

-- | Same effect as @0 + x@ where the exponent of the zero is the
-- same as that of @x@ if @x@ is finite).  NaNs are handled as for
-- arithmetic operations.
plus :: Quad -> Ctx Quad
plus = unary unsafe'c'decQuadPlus

-- | @quantize x y@ returns @z@ which is @x@ set to have the same
-- quantum as @y@; that is, numerically the same value but rounded
-- or padded if necessary to have the same exponent as @y@.  Useful
-- for rounding monetary quantities.
quantize :: Quad -> Quad -> Ctx Quad
quantize = binary unsafe'c'decQuadQuantize

-- | Reduces coefficient to its shortest possible form without
-- changing the value of the result by removing all possible
-- trailing zeroes.
reduce :: Quad -> Ctx Quad
reduce = unary unsafe'c'decQuadReduce

-- | Remainder from integer division.  If the intermediate integer
-- does not fit within a Quad, 'divisionImpossible' is raised.
remainder :: Quad -> Quad -> Ctx Quad
remainder = binary unsafe'c'decQuadRemainder

-- | Like 'remainder' but the nearest integer is used for for the
-- intermediate result instead of the result from 'divideInteger'.
remainderNear :: Quad -> Quad -> Ctx Quad
remainderNear = binary unsafe'c'decQuadRemainderNear

-- | @rotate x y@ rotates the digits of x to the left (if @y@ is
-- positive) or right (if @y@ is negative) without adjusting the
-- exponent or sign of @x@.  @y@ is the number of positions to
-- rotate and must be in the range @negate 'coefficientLen'@ to
-- @'coefficentLen'@.
--
-- NaNs are propagated as usual.  No status is set unless @y@ is
-- invalid or an operand is an NaN.
rotate :: Quad -> Quad -> Ctx Quad
rotate = binary unsafe'c'decQuadRotate

-- | @scaleB x y@ calculates @x * 10 ^ y@.  @y@ must be an integer
-- (finite with exponent of 0) in the range of plus or minus @2 *
-- 'coefficientLen' + 'coefficientLen')@, typically resulting from
-- 'logB'.  Underflow and overflow might occur; NaNs propagate as
-- usual.
scaleB :: Quad -> Quad -> Ctx Quad
scaleB = binary unsafe'c'decQuadScaleB

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
shift :: Quad -> Quad -> Ctx Quad
shift = binary unsafe'c'decQuadShift

-- omitted: Show

subtract :: Quad -> Quad -> Ctx Quad
subtract = binary unsafe'c'decQuadSubtract

-- | Uses the rounding method given rather than the one in the
-- 'Ctx'.  If the operand is infinite, an NaN, or if the result of
-- rounding is outside the range of a 'C'int32_t', then
-- 'invalidOperation' is set.  'inexact' is not set even if rounding
-- occurred.
toInt32 :: Round -> Quad -> Ctx C'int32_t
toInt32 = getRounded unsafe'c'decQuadToInt32

-- | Like 'toInt32' but if rounding removes non-zero digits then
-- 'inexact' is set.
toInt32Exact :: Round -> Quad -> Ctx C'int32_t
toInt32Exact = getRounded unsafe'c'decQuadToInt32Exact

-- | Rounds to an integral using the rounding mode set in the 'Ctx'.
-- If the operand is infinite, an infinity of the same sign is
-- returned.  If the operand is an NaN, the result is the same as
-- for other arithmetic operations.  If rounding removes non-zero
-- digits then 'inexact' is set.
toIntegralExact :: Quad -> Ctx Quad
toIntegralExact = unary unsafe'c'decQuadToIntegralExact

-- | @toIntegralValue r x@ returns an integral value of @x@ using
-- the rounding mode @r@ rather than the one specified in the 'Ctx'.
-- If the operand is an NaN, the result is the same as for other
-- arithmetic operations.  'inexact' is not set even if rounding
-- occurred.
toIntegralValue :: Round -> Quad -> Ctx Quad
toIntegralValue (Round rnd) d = Ctx $ \pC ->
  withForeignPtr (unQuad d) $ \pD ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  unsafe'c'decQuadToIntegralValue pR pD pC rnd >>
  return r

-- toByteString - moved to Internal so that Quad can Show in a
-- non-orphan instance

-- | @toUInt32 r x@ returns the value of @x@, rounded to an integer
-- if necessary using the rounding mode @r@ rather than the one
-- given in the 'Ctx'.  If @x@ is infinite, or outside of the range
-- of a 'C'uint32_t', then 'invalidOperation' is set.  'inexact' is
-- not set even if rounding occurs.
--
-- The negative zero converts to 0 and is valid, but negative
-- numbers are not valid.
toUInt32 :: Round -> Quad -> Ctx C'uint32_t
toUInt32 = getRounded unsafe'c'decQuadToUInt32

-- | Same as 'toUInt32' but if rounding removes non-zero digits then
-- 'inexact' is set.
toUInt32Exact :: Round -> Quad -> Ctx C'uint32_t
toUInt32Exact = getRounded unsafe'c'decQuadToUInt32Exact

-- | Runs a computation with the decimal128 default context.
runQuad :: Ctx a -> a
runQuad = Deka.Context.runCtx initDecimal128

-- | Runs a computation with the decimal128 default context, and
-- returns any resulting flags.
runQuadStatus :: Ctx a -> (a, [Flag])
runQuadStatus a = runQuad $ do
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

xor :: Quad -> Quad -> Ctx Quad
xor = binary unsafe'c'decQuadXor

-- Conversion from decNumber

-- | Converts a 'DecNum' to a 'Quad'.  The possible errors are the
-- same as for the 'fromByteString' function, except that
-- 'conversionSyntax' is not possible.

fromNumber :: DecNum -> Ctx Quad
fromNumber (DecNum fpd) = Ctx $ \pCtx ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  withForeignPtr fpd $ \pDn ->
  c'decimal128FromNumber (castPtr pR) pDn pCtx >>
  return r
