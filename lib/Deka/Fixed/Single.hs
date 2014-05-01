{-# LANGUAGE Trustworthy, NoImplicitPrelude #-}
module Deka.Fixed.Single
  (
    -- * Single
    Q.Single

  -- * Converting to and from strings
  , fromByteString
  , Q.toByteString
  , toEngByteString

  -- * Conversions to 'DecNum'
  , toNumber
  , fromNumber

  -- * Running a computation
  , runSingle
  , runSingleStatus

  -- * Context and class
  , module Deka.Context
  , module Deka.Class

  -- * Version
  , version

  -- * Complete encoding and decoding

  -- | These convert a 'Single' to a 'Decoded', which is a pure
  -- Haskell type containing all the information in the 'Single'.

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

  -- *** Returning Boll
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

  -- *** Returning 'DecClass'
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
import qualified Deka.Internal.Single.CtxFree as C
import Deka.Internal.Single.Ctx
import Deka.Decoded
import qualified Deka.Internal.Single.Decoding as D
import Deka.Internal.Single.Decoding hiding (fromBCD, toBCD, one)
import Deka.Context
import qualified Data.ByteString.Char8 as BS8
import Deka.Class
import qualified Deka.Internal.Single.Single as Q
import Deka.Internal.Single.Single (Single)
import Deka.Internal.Unsafe

-- | Returns a string in engineering notation.
--
-- In the decNumber C library, this is called @toEngString@; the
-- name is changed here because the function does not return a
-- regular Haskell 'String'.
toEngByteString :: Single -> BS8.ByteString
toEngByteString = unsafe1 C.toEngByteString

-- | Identifies the version of the decNumber C library.
version :: BS8.ByteString
version = unsafe0 C.version

-- | Converts a 'Single' to a 'DecNum'.
toNumber :: Single -> DecNum
toNumber = unsafe1 C.toNumber

fromBCD :: Decoded -> Single
fromBCD = unsafe1 D.fromBCD

toBCD :: Single -> Decoded
toBCD = unsafe1 D.toBCD
