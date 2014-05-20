{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

  -- * Signs and absolute value
  , abs
  , plus
  , minus

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
  , invert

  -- * Trailing zeroes
  , reduce

  -- * Integral rounding
  , toIntegralExact
  , toIntegralValue

  -- * Logarithms, exponents, roots
  , exp
  , ln
  , logB
  , log10
  , power
  , squareRoot

  -- * Identification
  , PosNeg(..)
  , Number(..)
  , Class(..)
  , strToClass
  , numClass
  , isNormal
  , isSubnormal
  , isFinite
  , isInfinite
  , isNaN
  , isNegative
  , isPositive
  , isSigned
  , isQNaN
  , isSNaN
  , isSpecial
  , isZero
  , isZeroCoeff
  , isOddCoeff
  , Sign(..)
  , sign
  , EvenOdd(..)
  , evenOdd

  -- * Version
  , version

  ) where

import Deka.Internal.Dec.CtxFree
import Deka.Internal.Dec.Ctx
import Deka.Internal.Mpdec
import Deka.Context
import Data.ByteString.Char8 as BS8
import Prelude (Show(..), (.))

-- | Same as
--
-- @
-- 'BS8.unpack' . 'toByteString'
-- @
instance Show Dec where
  show = BS8.unpack . toByteString

