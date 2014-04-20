module Deka.Quad where
{-
  (
    -- * Quad
    Quad

  -- * Converting to and from strings
  , fromByteString
  , toByteString
  , toEngByteString

  -- * Converting to and from integers
  , C'int32_t
  , C'uint32_t
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
  , compareTotal
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

  -- * Running a computation
  , runQuad
  , runQuadStatus

  -- * Context
  , module Deka.Context

  -- * Complete encoding and decoding

  -- | These convert a 'Quad' to a 'Decoded', which is a pure
  -- Haskell type containing all the information in the 'Quad'.

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

  -- *** Duplicates of Quad tests that return Bool
  -- | These duplicate the tests that are available for the Quad
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

  -- *** Duplicates of Quad tests that return 'DecClass'
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
  -}

