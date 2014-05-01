{-# LANGUAGE Trustworthy #-}
module Deka.Fixed.Double
  (
    -- * Double
    Q.Double

  -- * Converting to and from strings
  , fromByteString
  , Q.toByteString
  , toEngByteString

  -- * Converting to and from integers
  , fromInt32
  , fromUInt32
  , toInt32
  , toInt32Exact
  , toUInt32
  , toUInt32Exact

  -- * Arithmetic
  , add
  , subtract
  , multiply
  , fma
  , divide
  , divideInteger
  , remainder
  , remainderNear

  -- * Exponent and coefficient adjustment
  , quantize
  , reduce

  -- * Comparisons
  , compare
  , compareOrd
  , compareSignal
  , Q.compareTotal
  , compareTotalMag
  , max
  , maxMag
  , min
  , minMag
  , sameQuantum

  -- * Tests
  , decClass
  , isFinite
  , isInfinite
  , isInteger
  , isLogical
  , isNaN
  , isNegative
  , isNormal
  , isPositive
  , isSignaling
  , isSigned
  , isSubnormal
  , isZero

  -- * Signs
  , plus
  , minus
  , abs
  , copySign

  -- * Increment and decrement
  , nextMinus
  , nextPlus
  , nextToward

  -- * Digit-wise
  , and
  , or
  , xor
  , invert
  , shift
  , rotate

  -- * log and scale
  , logB
  , scaleB

  -- * Attributes
  , digits

  -- * Integral rounding

  -- | If you want to round but not to an integral value (e.g. round
  -- to two decimal places), see 'quantize'.
  , toIntegralExact
  , toIntegralValue

  -- * Constants
  , zero
  , one
  , version

  -- * Conversions to 'DecNum'
  , toNumber
  , fromNumber

  -- * Running a computation
  , runDouble
  , runDoubleStatus

  -- * Context and class
  , module Deka.Context
  , module Deka.Class

  -- * Complete encoding and decoding

  -- | These convert a 'Double' to a 'Decoded', which is a pure
  -- Haskell type containing all the information in the 'Double'.

  -- ** Digits
  , Digit(..)
  , digitToInt
  , intToDigit
  , digitToChar
  , digitsToInteger
  , integralToDigits

  -- ** Coefficients
  , coefficientLen
  , payloadLen
  , Coefficient
  , coefficient
  , unCoefficient
  , zeroCoefficient
  , oneCoefficient
  , Payload
  , payload
  , unPayload
  , zeroPayload

  -- ** Exponents
  , Exponent
  , exponent
  , unExponent
  , zeroExponent
  , minMaxExp
  , AdjustedExp
  , adjustedExp
  , unAdjustedExp
  , minNormalAdj
  , minNormalExp
  , adjustedToExponent

  -- ** Sign, NaN, Value, Decoded
  , Sign(..)
  , NaN(..)
  , Value(..)
  , Decoded(..)

  --- ** Conversion functions
  , fromBCD
  , toBCD
  , scientific
  , ordinary
  , decodedToRational

  -- ** Decoded predicates

  -- *** Duplicates of Double tests that return Bool
  -- | These duplicate the tests that are available for the Double
  -- type directly.
  , dIsFinite
  , dIsInfinite
  , dIsInteger
  , dIsLogical
  , dIsNaN
  , dIsNegative
  , dIsNormal
  , dIsPositive
  , dIsSignaling
  , dIsSigned
  , dIsSubnormal
  , dIsZero
  , dDigits

  -- *** Duplicates of Double tests that return 'DecClass'
  , dIsSNaN
  , dIsQNaN
  , dIsNegInf
  , dIsNegNormal
  , dIsNegSubnormal
  , dIsNegZero
  , dIsPosZero
  , dIsPosSubnormal
  , dIsPosNormal
  , dIsPosInf

  ) where

import Deka.Internal.DecNum.DecNum
import qualified Deka.Internal.Double.CtxFree as C
import Deka.Internal.Double.Ctx
import Deka.Decoded
import qualified Deka.Internal.Double.Decoding as D
import Deka.Internal.Double.Decoding hiding (fromBCD, toBCD, one)
import Deka.Context
import qualified Data.ByteString.Char8 as BS8
import Deka.Class
import qualified Deka.Internal.Double.Double as Q
import Deka.Internal.Double.Double (Double)
import Deka.Internal.Unsafe
import Prelude (Bool, Maybe, Ordering)
import Data.Word
import Data.Int

-- | More information about a particular 'Double'.
decClass :: Double -> Class
decClass = unsafe1 C.decClass

-- | True for NaNs.
isNaN :: Double -> Bool
isNaN = unsafe1 C.isNaN

-- | Wrapper for 'compare' that returns an 'Ordering' rather than a
-- 'Double'.  Returns @Just LT@ rather than -1, @Just EQ@ rather than
-- 0, and @Just GT@ rather than 1, and @Nothing@ rather than NaN.
-- This is a pure function; it does not affect the 'Ctx'.

compareOrd :: Double -> Double -> Maybe Ordering
compareOrd = unsafe2 C.compareOrd


-- | Same as 'compareTotal' but compares the absolute value of the
-- two arguments.
compareTotalMag :: Double -> Double -> Ordering
compareTotalMag = unsafe2 C.compareTotalMag


-- | @copySign x y@ returns @z@, which is a copy of @x@ but has the
-- sign of @y@.  This function never raises any signals.
copySign :: Double -> Double -> Double
copySign = unsafe2 C.copySign


-- | Number of significant digits.  If zero or infinite, returns 1.
-- If NaN, returns number of digits in the payload.
digits :: Double -> Int
digits = unsafe1 C.digits

fromInt32 :: Int32 -> Double
fromInt32 = unsafe1 C.fromInt32

fromUInt32 :: Word32 -> Double
fromUInt32 = unsafe1 C.fromUInt32

-- | True if @x@ is neither infinite nor a NaN.
isFinite :: Double -> Bool
isFinite = unsafe1 C.isFinite

-- | True for infinities.
isInfinite :: Double -> Bool
isInfinite = unsafe1 C.isInfinite

-- | True if @x@ is finite and has exponent of @0@; False otherwise.
-- This tests the exponent, not the /adjusted/ exponent.  This can
-- lead to results you may not expect:
--
-- >>> isInteger . evalCtx . fromByteString . pack $ "3.00e2"
-- True
--
-- >>> isInteger . evalCtx . fromByteString . pack $ "3e2"
-- False
--
-- >>> isInteger . evalCtx . fromByteString . pack $ "3.00e0"
-- False
isInteger :: Double -> Bool
isInteger = unsafe1 C.isInteger

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
isLogical :: Double -> Bool
isLogical = unsafe1 C.isLogical

-- | True only if @x@ is finite, non-zero, and not subnormal.
isNormal :: Double -> Bool
isNormal = unsafe1 C.isNormal

-- | True only if @x@ is a signaling NaN.
isSignaling :: Double -> Bool
isSignaling = unsafe1 C.isSignaling

-- | True only if @x@ has a sign of 1.  Note that zeroes and NaNs
-- may have sign of 1.
isSigned :: Double -> Bool
isSigned = unsafe1 C.isSigned

-- | True only if @x@ is subnormal - that is, finite, non-zero, and
-- with a magnitude less than 10 ^ emin.
isSubnormal :: Double -> Bool
isSubnormal = unsafe1 C.isSubnormal

-- | True only if both operands have the same exponent or are both
-- NaNs (quiet or signaling) or both infinite.
sameQuantum :: Double -> Double -> Bool
sameQuantum = unsafe2 C.sameQuantum


-- | Returns a string in engineering notation.
--
-- In the decNumber C library, this is called @toEngString@; the
-- name is changed here because the function does not return a
-- regular Haskell 'String'.
toEngByteString :: Double -> BS8.ByteString
toEngByteString = unsafe1 C.toEngByteString

-- | Identifies the version of the decNumber C library.
version :: BS8.ByteString
version = unsafe0 C.version

-- | Converts a 'Double' to a 'DecNum'.
toNumber :: Double -> DecNum
toNumber = unsafe1 C.toNumber

-- | A Double whose coefficient, exponent, and sign are all 0.
zero :: Double
zero = unsafe0 C.zero

isNegative :: Double -> Bool
isNegative = unsafe1 Q.isNegative

isZero :: Double -> Bool
isZero = unsafe1 Q.isZero

isPositive :: Double -> Bool
isPositive = unsafe1 Q.isPositive

one :: Double
one = unsafe0 D.one

fromBCD :: Decoded -> Double
fromBCD = unsafe1 D.fromBCD

toBCD :: Double -> Decoded
toBCD = unsafe1 D.toBCD
