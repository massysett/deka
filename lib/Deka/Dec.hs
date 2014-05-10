{-# LANGUAGE EmptyDataDecls, Trustworthy #-}
-- | Decimal arithmetic.
--
-- Much documentation is copied from documentation for the decNumber
-- C library, available at
--
-- <http://speleotrove.com/decimal/dnnumb.html>
module Deka.Dec
  ( Dec

  -- * Context
  , module Deka.Context

  -- * String Conversions
  , fromByteString
  , toByteString
  , toEngByteString

  -- * Arithmetic
  , add
  , subtract
  , multiply
  , fma
  , divide
  , divideInteger
  , remainder
  , remainderNear

  -- * Plus and minus
  , plus
  , minus

  -- * Signs and absolute value
  , abs
  , invert
  , copyAbs
  , copySign
  , negate

  -- * Comparisons
  , compare
  , compareSignal
  , compareTotal
  , compareTotalMag
  , max
  , maxMag
  , min
  , minMag

  -- * Increments
  , nextMinus
  , nextPlus
  , nextToward

  -- * Exponent testing and adjustment
  , sameQuantum
  , quantize
  , rescale
  , scaleB

  -- * Digit-wise and logical
  , and
  , or
  , xor
  , shift
  , rotate

  -- * Trailing zeroes
  , reduce
  , trim

  -- * Integral rounding
  , toIntegralExact
  , toIntegralValue

  -- * Integer Conversions
  , fromInt32
  , toInt32
  , fromUInt32
  , toUInt32

  -- * Logarithms, exponents, roots
  , exp
  , ln
  , logB
  , log10
  , power
  , squareRoot

  -- * Identification
  , module Deka.Class
  , numClass
  , isNormal
  , isSubnormal
  , isCanonical
  , isFinite
  , isInfinite
  , isNaN
  , isNegative
  , isQNaN
  , isSNaN
  , isSpecial
  , isZero

  -- * Version
  , version

  -- * Constants
  , zero

  -- * Decoding and encoding

  -- | /Encoding/ takes Haskell types and converts them to a C
  -- decNumber type so you can perform arithmetic on them.
  -- /Decoding/ takes a C decNumber type and converts it to Haskell
  -- types.

  -- ** Number components
  , Coefficient
  , coefficient
  , unCoefficient
  , zeroCoefficient
  , oneCoefficient
  , Exponent(..)
  , Sign(..)
  , NaN(..)
  , Payload(..)
  , Decoded(..)

  -- ** Decoding
  , decode

  -- ** Encoding
  , infinity
  , notANumber
  , nonSpecialCtxFree
  , nonSpecial

  -- ** Adjusted exponents
  , AdjExponent
  , unAdjExponent
  , adjExponent
  ) where

import Prelude hiding (abs, and, or, max, min, compare, exp,
  subtract, negate, isNaN, isInfinite, exponent)
import qualified Data.ByteString.Char8 as BS8
import Deka.Internal.Dec.Dec
import Deka.Internal.Dec.CtxFree
  ( Coefficient
  , coefficient
  , unCoefficient
  , zeroCoefficient
  , oneCoefficient
  , Exponent(..)
  , Payload(..)
  , Decoded(..)
  , AdjExponent
  , unAdjExponent
  , adjExponent
  )

import qualified Deka.Internal.Dec.CtxFree as I
import Deka.Context
import Deka.Class
import Deka.Internal.Dec.Ctx
import Deka.Internal.Unsafe
import Deka.Decoded
import Data.Word
import Data.Int

-- | Convert a signed 32-bit integer to a 'Dec'.
fromInt32 :: Int32 -> Dec
fromInt32 = unsafe1 I.fromInt32

-- | Convert an unsigned 32-bit integer to a 'Dec'.
fromUInt32 :: Word32 -> Dec
fromUInt32 = unsafe1 I.fromUInt32

-- | Converts a 'Dec' to engineering notation (the exponent will be
-- a multiple of three, and there may be up to three digits before
-- any decimal point) if an exponent is needed. It implements the
-- _to-engineering-string_ conversion described in the General
-- Decimal Arithmetic Specification.
toEngByteString :: Dec -> BS8.ByteString
toEngByteString = unsafe1 I.toEngByteString

-- | Tests whether the exponents of two
-- numbers are equal. The coefficients and signs of the operands
-- are ignored.  If the exponents of the operands are
-- equal, or if they are both Infinities or they are both NaNs,
-- returns 1. In all other cases, returns 0. No
-- error is possible. 
sameQuantum :: Dec -> Dec -> Dec
sameQuantum = unsafe2 I.sameQuantum

-- skipped: ClassToString, Copy

-- | Copies the absolute value of the content of one 'Dec' to
-- another.  The sign of the result is always 0.
copyAbs
  :: Dec
  -- ^ Source of sign
  -> Dec
  -- ^ Copy sign to this destination
  -> Dec
  -- ^ Result
copyAbs = unsafe2 I.copyAbs

-- | Inverts the sign.
negate :: Dec -> Dec
negate = unsafe1 I.negate

-- | Copies a value to a new 'Dec', while using the sign from a
-- different 'Dec'.
copySign
  :: Dec
  -- ^ Source of content (except sign)
  -> Dec
  -- ^ Source of sign
  -> Dec
copySign = unsafe2 I.copySign

-- | Removes insignificant trailing zeros from a number,
-- uncoditionally. That is, if the number has any fractional
-- trailing zeros they are removed by dividing the coefficient by
-- the appropriate power of ten and adjusting the exponent
-- accordingly. 'reduce' can be used to remove all trailing zeros.
trim :: Dec -> Dec
trim = unsafe1 I.trim

-- | The version of the decNumber C library in use.
version :: BS8.ByteString
version = unsafe0 I.version

-- | Always zero.
zero :: Dec
zero = unsafe0 I.zero

-- | Always returns True because decNumbers are always canonical.
isCanonical :: Dec -> Bool
isCanonical = unsafe1 I.isCanonical

-- | True if a 'Dec' is finite, or false if it is an infinity or
-- NaN.
isFinite :: Dec -> Bool
isFinite = unsafe1 I.isFinite

-- | True if a 'Dec' is infinite; False if it is a finite number or
-- NaN.
isInfinite :: Dec -> Bool
isInfinite = unsafe1 I.isInfinite

-- | True if a 'Dec' is an NaN (quiet or signaling).
isNaN :: Dec -> Bool
isNaN = unsafe1 I.isNaN

-- | True if a 'Dec' is negative (either minus zero, less than zero,
-- or an NaN with a sign of 1).
isNegative :: Dec -> Bool
isNegative = unsafe1 I.isNegative

-- | True if 'Dec' is a quiet NaN.
isQNaN :: Dec -> Bool
isQNaN = unsafe1 I.isQNaN

-- | True if 'Dec' is a signaling NaN.
isSNaN :: Dec -> Bool
isSNaN = unsafe1 I.isSNaN

-- | True if 'Dec' is special; that is, an infinity or an NaN.  This
-- is the inversion of 'isFinite'.
isSpecial :: Dec -> Bool
isSpecial = unsafe1 I.isSpecial

-- | True if 'Dec' is zero (either positive or negative).
isZero :: Dec -> Bool
isZero = unsafe1 I.isZero

-- skipped: radix

-- | Take a C 'Dec' and convert it to Haskell types.
decode :: Dec -> Decoded
decode = unsafe1 I.decode

-- | Encodes positive or negative infinities.
infinity :: Sign -> Dec
infinity = unsafe1 I.infinity

-- | Encodes quiet or signaling NaNs.
notANumber :: Sign -> NaN -> Coefficient -> Dec
notANumber = unsafe3 I.notANumber

-- | Encodes non-special numbers (also known as finite numbers.)
-- Does not need the context; however, you will have to supply
-- information about whether subnormal values are allowed.
nonSpecialCtxFree
  :: Maybe Precision
  -- ^ If Just, allow subnormal values.  In that case, the maximum
  -- number of digits is needed in order to compute the lower limit
  -- for the exponent.  If Nothing, do not allow subnormal values.

  -> Sign
  -> Coefficient
  -> Exponent
  -> Maybe Dec
  -- ^ Fails if the exponent is out of range.
nonSpecialCtxFree = unsafe4 I.nonSpecialCtxFree

{-

Functions implemented from decNumber list:
These are listed in the order in which they appear in the
decNumber documentation.
Functions implemented are written.  Those skipped are in parentheses.

Conversion functions
Have been implemented:

fromString - use fromByteString
toString - use toByteString
toEngString - use toEngByteString

Aritmetic and Logical Functions
All those present in decNumber have been implemented:

abs
add
and
compare
compareSignal
compareTotal
compareTotalMag
divide
divideInteger
exp
fma
invert
ln
logB
log10
max
maxMag
min
minMag
minus
multiply
nextMinus
nextPlus
nextToward
normalize
or
plus
power
quantize
remainder
remainderNear
rescale
rotate
sameQuantum
scaleB
shift
squareRoot
subtract
toIntegralExact
toIntegralValue
xor

Utility Functions

class - use numClass
(classToString) - use the Show instance of Class
(copy)
copyAbs
copyNegate - renamed negate
copySign
fromInt32
fromUInt32
(getBCD) - use decode
isCanonical
isFinite
isInfinite
isNaN
isNegative
isNormal
isQNaN
isSNaN
isSpecial
isSubnormal
isZero
(radix)
reduce
(setBCD) - use encode
toInt32
toUInt32
trim
version
zero
-}
