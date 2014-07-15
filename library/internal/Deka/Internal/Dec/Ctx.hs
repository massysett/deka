{-# LANGUAGE Safe, OverloadedStrings #-}
module Deka.Internal.Dec.Ctx where

import qualified Data.ByteString.Char8 as BS8
import Deka.Internal.Context
import Deka.Internal.Mpdec
import Deka.Internal.Util.Ctx
import Data.String

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
  newDec $ \dn ->
  BS8.useAsCString bs $ \cstr ->
  c'mpd_set_string dn cstr pCtx

-- | Returns the absolute value.  The same effect as 'plus' unless
-- the operand is negative, in which case it is the same as 'minus'.
abs :: Dec -> Ctx Dec
abs = unary c'mpd_abs

-- | Addition.
add :: Dec -> Dec -> Ctx Dec
add = binary c'mpd_add

-- | Digit-wise logical @and@.
and :: Dec -> Dec -> Ctx Dec
and = binary c'mpd_and

-- | @compare x y@ returns @-1@ if a is less than b, 0 if a is equal
-- to b, and 1 if a is greater than b.  'invalidOperation' is set if
-- at least one of the operands is a signaling NaN.
compare :: Dec -> Dec -> Ctx Dec
compare = binary c'mpd_compare

-- | Identical to 'Deka.Dec.compare' except that all NaNs
-- (including quiet NaNs) set the 'invalidOperation' condition.
compareSignal :: Dec -> Dec -> Ctx Dec
compareSignal = binary c'mpd_compare_signal

-- | Division.
divide :: Dec -> Dec -> Ctx Dec
divide = binary c'mpd_div

-- | Returns the integer part of the result of division.  It must be
-- possible to express the result as an integer.  That is, it must
-- have no more digits than 'Precision' in the 'Ctx'.  If it does
-- then 'divisionImpossible' is raised.
divideInteger :: Dec -> Dec -> Ctx Dec
divideInteger = binary c'mpd_divint

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
exp = unary c'mpd_exp

-- | @fma x y z@ multiplies @x@ by @y@ and then adds @z@ to that
-- intermediate result.  It is equivalent to a multiplication
-- followed by an addition except that the intermediate result is
-- not rounded and will not cause overflow or underflow. That is,
-- only the final result is rounded and checked.
--
-- This is a mathematical function; the @10 ^ 6@ restrictions on
-- precision and range apply as described above.
fma :: Dec -> Dec -> Dec -> Ctx Dec
fma = ternary c'mpd_fma

-- | Digit-wise inversion (a @0@ becomes a @1@ and vice versa).
invert :: Dec -> Ctx Dec
invert = unary c'mpd_invert

-- | Natural logarithm.  Results are correctly rounded if
-- 'setAllCorrectRound' is True.
ln :: Dec -> Ctx Dec
ln = unary c'mpd_ln

-- | Returns the adjusted exponent of the operand, according to the
-- rules for @logB@ of IEEE 754.  This returns the exponent of the
-- operand as though its decimal point had been moved to follow the
-- first digit while keeping the same value.  The result is not
-- limited by 'Emin' or 'Emax'.

-- | If operand is an NaN, the general rules apply.  If operand is
-- infinite, the result is +Infinity.  If operand is zero, result is
-- -Infinity and 'invalidOperation' is set.  Otherwise, the result
-- is the same as the adjusted exponent of the operand, or
-- @floor(log10(a))@ where @a@ is the operand.
logB :: Dec -> Ctx Dec
logB = unary c'mpd_logb

-- | Base 10 logarithm.  Results are correctly rounded if
-- 'setAllCorrectRound' is True.
log10 :: Dec -> Ctx Dec
log10 = unary c'mpd_log10

-- | Compares two numbers numerically and returns the larger.  If
-- the numbers compare equal then number is chosen with regard to
-- sign and exponent. Unusually, if one operand is a quiet NaN and
-- the other a number, then the number is returned.
max :: Dec -> Dec -> Ctx Dec
max = binary c'mpd_max

-- | Compares the magnitude of two numbers numerically and sets
-- number to the larger. It is identical to 'Deka.Dec.max' except
-- that the signs of the operands are ignored and taken to be 0
-- (non-negative).
maxMag :: Dec -> Dec -> Ctx Dec
maxMag = binary c'mpd_max_mag

-- | Compares two numbers numerically and sets number to the
-- smaller. If the numbers compare equal then number is chosen with
-- regard to sign and exponent. Unusually, if one operand is a quiet
-- NaN and the other a number, then the number is returned.
min :: Dec -> Dec -> Ctx Dec
min = binary c'mpd_min

-- | Compares the magnitude of two numbers numerically and sets
-- number to the smaller. It is identical to 'Deka.Dec.min' except
-- that the signs of the operands are ignored and taken to be 0
-- (non-negative).
minMag :: Dec -> Dec -> Ctx Dec
minMag = binary c'mpd_min_mag

-- | Returns the result of subtracting the operand from zero.  hat
-- is, it is negated, following the usual arithmetic rules; this may
-- be used for implementing a prefix minus operation.
minus :: Dec -> Ctx Dec
minus = unary c'mpd_minus

-- | Multiplication.
multiply :: Dec -> Dec -> Ctx Dec
multiply = binary c'mpd_mul

-- | Digit-wise logical inclusive or.
or :: Dec -> Dec -> Ctx Dec
or = binary c'mpd_or

-- | Returns the result of adding the operand to zero.  This takes
-- place according to the settings given in the 'Ctx', following the
-- usual arithmetic rules. This may therefore be used for rounding
-- or for implementing a prefix plus operation.
plus :: Dec -> Ctx Dec
plus = unary c'mpd_plus

-- | @power b e@ returns @b@ raised to the power of @e@.  Integer
-- powers are exact, provided that the result is finite and fits
-- into 'Precision'.
--
-- Results are not correctly rounded, even if 'setAllCorrectRound'
-- is True.  The error of the function is less than @1ULP + t@,
-- where @t@ has a maximum of @0.1ULP@, but is almost always less
-- than @0.001ULP@.

power :: Dec -> Dec -> Ctx Dec
power = binary c'mpd_pow

-- | @quantize a b@ returns the number that is equal in value to
-- @a@, but has the exponent of @b@.
quantize :: Dec -> Dec -> Ctx Dec
quantize = binary c'mpd_quantize

-- overflow/underflow checks, returns @a@ in its simplest form with
-- all trailing zeros removed.
reduce :: Dec -> Ctx Dec
reduce = unary c'mpd_reduce

-- | @remainder a b@ returns the remainder of @a / b@.
remainder :: Dec -> Dec -> Ctx Dec
remainder = binary c'mpd_rem

-- | @remainderNear a b@ returns @a - b * n@, where @n@ is the
-- integer nearest the exact value of @a / b@.  If two integers are
-- equally near then the even one is chosen.
remainderNear :: Dec -> Dec -> Ctx Dec
remainderNear = binary c'mpd_rem_near

-- | @rescale a b@ returns the number that is equal in value
-- to @a@, but has the exponent @b@. Special numbers are copied
-- without signaling. This function is not part of the General
-- Decimal Arithmetic Specification. It
-- is also not equivalent to the rescale function that was removed
-- from the specification.

rescale :: Dec -> Signed -> Ctx Dec
rescale a b = Ctx $ \p -> newDec $ \r ->
  withDec a $ \pa ->
  c'mpd_rescale r pa b p

-- | @rotate x y@ returns @x@ rotated by @y@ places. @y@ must be in
-- the range [-'Precision', 'Precision']. A negative @y@ indicates a
-- right rotation, a positive @y@ a left rotation.

rotate :: Dec -> Dec -> Ctx Dec
rotate = binary c'mpd_rotate

-- | @scaleB a b@ - b must be an integer with exponent 0. If @a@ is
-- infinite, returns @a@. Otherwise, returns @a@ with the
-- value of @b@ added to the exponent.

scaleB :: Dec -> Dec -> Ctx Dec
scaleB = binary c'mpd_scaleb

-- | @shift a b@ returns @a@ shifted by @b@ places. @b@ must be in
-- the range [-'Precision', 'Precision']. A negative @b@ indicates a
-- right shift, a positive @b@ a left shift. Digits that do not fit
-- are discarded.

shift :: Dec -> Dec -> Ctx Dec
shift = binary c'mpd_shift

-- | Returns the square root.  This function is always correctly
-- rounded using the 'roundHalfEven' method.

squareRoot :: Dec -> Ctx Dec
squareRoot = unary c'mpd_sqrt

-- | Returns the reciprocal of the square root.  This function
-- always uses 'roundHalfEven'.  Results are not correctly rounded
-- even if 'setAllCorrectRound' is True.

inverseSquareRoot :: Dec -> Ctx Dec
inverseSquareRoot = unary c'mpd_invroot


-- | Subtraction.

subtract :: Dec -> Dec -> Ctx Dec
subtract = binary c'mpd_sub

-- | Round to an integer, using the rounding mode of the context.
-- Only a signaling NaN causes an 'invalidOperation'
-- condition.

toIntegralExact :: Dec -> Ctx Dec
toIntegralExact = unary c'mpd_round_to_intx

-- | Like 'toIntegralExact', but 'inexact' and 'rounded' are never
-- set.
toIntegralValue :: Dec -> Ctx Dec
toIntegralValue = unary c'mpd_round_to_int

floor :: Dec -> Ctx Dec
floor = unary c'mpd_floor

ceiling :: Dec -> Ctx Dec
ceiling = unary c'mpd_ceil

truncate :: Dec -> Ctx Dec
truncate = unary c'mpd_trunc

-- | Digit-wise logical exclusive or.

xor :: Dec -> Dec -> Ctx Dec
xor = binary c'mpd_xor

-- | Returns the closest representable number that is smaller than
-- the operand.
nextMinus :: Dec -> Ctx Dec
nextMinus = unary c'mpd_next_minus

-- | Returns the closest representable number that is larger than
-- the operand.
nextPlus :: Dec -> Ctx Dec
nextPlus = unary c'mpd_next_plus

-- | @nextToward a b@ returns the representable number closest to
-- @a@ in the direction of @b@.

nextToward :: Dec -> Dec -> Ctx Dec
nextToward = binary c'mpd_next_toward

toBool :: Integral a => a -> Bool
toBool i
  | i == 0 = False
  | otherwise = True

-- | False if the decimal is special or zero, or the exponent is
-- less than 'Emin'. True otherwise.

isNormal :: Dec -> Ctx Bool
isNormal d = Ctx $ \p ->
  withDec d $ \pd ->
  c'mpd_isnormal pd p >>= \i ->
  return (toBool i)

-- | False if the decimal is special or zero, or the exponent is
-- greater or equal to 'Emin'. True otherwise.
isSubnormal :: Dec -> Ctx Bool
isSubnormal d = Ctx $ \p ->
  withDec d $ \pd ->
  c'mpd_issubnormal pd p >>= \i ->
  return (toBool i)

data PosNeg = Pos | Neg
  deriving (Eq, Ord, Show)

data Number
  = Infinity
  | Normal
  | Subnormal
  | Zero
  deriving (Eq, Ord, Show)

data Class
  = SNaN
  | NaN
  | Number PosNeg Number
  deriving (Eq, Ord, Show)

strToClass :: IsString a => [(a, Class)]
strToClass =
  [ ("sNaN", SNaN)
  , ("NaN", NaN)
  , ("-Infinity", Number Neg Infinity)
  , ("-Normal", Number Neg Normal)
  , ("-Subnormal", Number Neg Subnormal)
  , ("-Zero", Number Neg Zero)
  , ("+Zero", Number Pos Zero)
  , ("+Subnormal", Number Pos Subnormal)
  , ("+Normal", Number Pos Normal)
  , ("+Infinity", Number Pos Infinity)
  ]

-- | Determines the 'Class' of a 'Dec'.

numClass :: Dec -> Ctx Class
numClass d = Ctx $ \pCtx ->
  withDec d $ \pd ->
  c'mpd_class pd pCtx >>= \chars ->
  BS8.packCString chars >>= \bs ->
  return . maybe (error "numClass: class not found") id
    . lookup bs $ strToClass

