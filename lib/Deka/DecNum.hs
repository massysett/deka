{-# LANGUAGE EmptyDataDecls, Trustworthy #-}
module Deka.DecNum
  ( DecNum

  -- * Conversions
  , C'int32_t
  , fromInt32
  , toInt32
  , C'uint32_t
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
  , NaNtype(..)
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
import qualified Prelude as P
import qualified Data.ByteString.Char8 as BS8
import System.IO.Unsafe (unsafePerformIO)
import Deka.DecNum.DecNum
import Deka.DecNum.Internal
  ( Coefficient
  , coefficient
  , unCoefficient
  , zeroCoefficient
  , oneCoefficient
  , Exponent(..)
  , Sign(..)
  , NaNtype(..)
  , Payload(..)
  , Decoded(..)
  , AdjExponent
  , unAdjExponent
  , adjExponent
  )

import qualified Deka.DecNum.Internal as I
import Deka.Decnumber.Types
import Deka.Context
import Deka.DecNum.Safe

unsafe0 :: IO a -> a
unsafe0 = unsafePerformIO

unsafe1 :: (a -> IO b) -> a -> b
unsafe1 = fmap unsafePerformIO

unsafe2 :: (a -> b -> IO c) -> a -> b -> c
unsafe2 = fmap (fmap unsafePerformIO)

unsafe3 :: (a -> b -> c -> IO d) -> a -> b -> c -> d
unsafe3 = fmap (fmap (fmap unsafePerformIO))

unsafe4 :: (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> e
unsafe4 = fmap (fmap (fmap (fmap unsafePerformIO)))

fromInt32 :: C'int32_t -> DecNum
fromInt32 = unsafe1 I.fromInt32

fromUInt32 :: C'uint32_t -> DecNum
fromUInt32 = unsafe1 I.fromUInt32

toEngByteString :: DecNum -> BS8.ByteString
toEngByteString = unsafe1 I.toEngByteString

sameQuantum :: DecNum -> DecNum -> DecNum
sameQuantum = unsafe2 I.sameQuantum

-- skipped: ClassToString, Copy

copyAbs
  :: DecNum
  -- ^ Source of sign
  -> DecNum
  -- ^ Copy sign to this destination
  -> DecNum
  -- ^ Result
copyAbs = unsafe2 I.copyAbs

negate :: DecNum -> DecNum
negate = unsafe1 I.negate

copySign
  :: DecNum
  -- ^ Source of content (except sign)
  -> DecNum
  -- ^ Source of sign
  -> DecNum
copySign = unsafe2 I.copySign

trim :: DecNum -> DecNum
trim = unsafe1 I.trim

version :: BS8.ByteString
version = unsafe0 I.version

zero :: DecNum
zero = unsafe0 I.zero

isCanonical :: DecNum -> Bool
isCanonical = unsafe1 I.isCanonical

isFinite :: DecNum -> Bool
isFinite = unsafe1 I.isFinite

isInfinite :: DecNum -> Bool
isInfinite = unsafe1 I.isInfinite

isNaN :: DecNum -> Bool
isNaN = unsafe1 I.isNaN

isNegative :: DecNum -> Bool
isNegative = unsafe1 I.isNegative

isQNaN :: DecNum -> Bool
isQNaN = unsafe1 I.isQNaN

isSNaN :: DecNum -> Bool
isSNaN = unsafe1 I.isSNaN

isSpecial :: DecNum -> Bool
isSpecial = unsafe1 I.isSpecial

isZero :: DecNum -> Bool
isZero = unsafe1 I.isZero

-- skipped: radix

-- | Take a C 'DecNum' and convert it to Haskell types.
decode :: DecNum -> Decoded
decode = unsafe1 I.decode

-- | Encodes positive or negative infinities.
infinity :: Sign -> DecNum
infinity = unsafe1 I.infinity

-- | Encodes quiet or signaling NaNs.
notANumber :: Sign -> NaNtype -> Coefficient -> DecNum
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
  -> Maybe DecNum
  -- ^ Fails if the exponent is out of range.
nonSpecialCtxFree = unsafe4 I.nonSpecialCtxFree

