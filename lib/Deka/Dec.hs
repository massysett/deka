{-# LANGUAGE EmptyDataDecls, Trustworthy #-}
module Deka.Dec
  ( Dec

  -- * Conversions
  , fromInt32
  , toInt32
  , fromUInt32
  , toUInt32
  , fromByteString
  , toByteString
  , toEngByteString

  -- * Arithmetic and logical functions
  , abs
  , add
  , and
  , compare
  , compareSignal
  , compareTotal
  , compareTotalMag
  , divide
  , divideInteger
  , exp
  , fma
  , invert
  , ln
  , logB
  , log10
  , max
  , maxMag
  , min
  , minMag
  , minus
  , multiply
  , normalize
  , or
  , plus
  , power
  , quantize
  , reduce
  , remainder
  , remainderNear
  , rescale
  , rotate
  , sameQuantum
  , scaleB
  , shift
  , squareRoot
  , subtract
  , toIntegralExact
  , toIntegralValue
  , xor
  , nextMinus
  , nextPlus
  , nextToward

  -- * Utility functions
  , numClass
  , copyAbs
  , negate
  , copySign
  , trim
  , version
  , zero
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

  -- * Re-exports
  , module Deka.Context
  , module Deka.Class

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

  -- * Adjusted exponents
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

fromInt32 :: Int32 -> Dec
fromInt32 = unsafe1 I.fromInt32

fromUInt32 :: Word32 -> Dec
fromUInt32 = unsafe1 I.fromUInt32

toEngByteString :: Dec -> BS8.ByteString
toEngByteString = unsafe1 I.toEngByteString

sameQuantum :: Dec -> Dec -> Dec
sameQuantum = unsafe2 I.sameQuantum

-- skipped: ClassToString, Copy

copyAbs
  :: Dec
  -- ^ Source of sign
  -> Dec
  -- ^ Copy sign to this destination
  -> Dec
  -- ^ Result
copyAbs = unsafe2 I.copyAbs

negate :: Dec -> Dec
negate = unsafe1 I.negate

copySign
  :: Dec
  -- ^ Source of content (except sign)
  -> Dec
  -- ^ Source of sign
  -> Dec
copySign = unsafe2 I.copySign

trim :: Dec -> Dec
trim = unsafe1 I.trim

version :: BS8.ByteString
version = unsafe0 I.version

zero :: Dec
zero = unsafe0 I.zero

isCanonical :: Dec -> Bool
isCanonical = unsafe1 I.isCanonical

isFinite :: Dec -> Bool
isFinite = unsafe1 I.isFinite

isInfinite :: Dec -> Bool
isInfinite = unsafe1 I.isInfinite

isNaN :: Dec -> Bool
isNaN = unsafe1 I.isNaN

isNegative :: Dec -> Bool
isNegative = unsafe1 I.isNegative

isQNaN :: Dec -> Bool
isQNaN = unsafe1 I.isQNaN

isSNaN :: Dec -> Bool
isSNaN = unsafe1 I.isSNaN

isSpecial :: Dec -> Bool
isSpecial = unsafe1 I.isSpecial

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
(copyNegate)
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
