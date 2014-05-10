{-# LANGUAGE Trustworthy #-}
-- | /Encoding/ takes Haskell types and converts them to a C
-- decNumber type so you can perform arithmetic on them.
-- /Decoding/ takes a C decNumber type and converts it to Haskell
-- types.

module Deka.Dec.Decoding
  (
  -- * Common components
    module Deka.Decoded

  -- * Number components
  , I.Coefficient
  , I.coefficient
  , I.unCoefficient
  , I.zeroCoefficient
  , I.oneCoefficient
  , I.Exponent(..)
  , Sign(..)
  , NaN(..)
  , I.Payload(..)
  , I.Decoded
  , I.dcdSign
  , I.dcdPayload

  -- * Decoding
  , decode

  -- * Encoding
  , infinity
  , notANumber
  , nonSpecialCtxFree
  , I.nonSpecial

  -- * Adjusted exponents
  , I.AdjExponent
  , I.unAdjExponent
  , I.adjExponent
  ) where

import qualified Deka.Internal.Dec.Decoding as I
import Deka.Decoded
import Deka.Internal.Dec.Dec
import Deka.Internal.Unsafe
import Deka.Context

-- | Take a C 'Dec' and convert it to Haskell types.
decode :: Dec -> I.Decoded
decode = unsafe1 I.decode

-- | Encodes positive or negative infinities.
infinity :: Sign -> Dec
infinity = unsafe1 I.infinity

-- | Encodes quiet or signaling NaNs.
notANumber :: Sign -> NaN -> I.Coefficient -> Dec
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
  -> I.Coefficient
  -> I.Exponent
  -> Maybe Dec
  -- ^ Fails if the exponent is out of range.
nonSpecialCtxFree = unsafe4 I.nonSpecialCtxFree

