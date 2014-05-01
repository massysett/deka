{-# LANGUAGE Safe, DeriveDataTypeable #-}

-- | Floating-point decimals.
--
-- This uses the decNumber C library, so you will want to read the
-- documentation about it to fully understand this module:
--
-- <http://speleotrove.com/decimal/decnumber.html>
--
-- <http://speleotrove.com/decimal/decarith.html>
--
-- <http://speleotrove.com/decimal/>
--
-- Many of the comments on what these functions do are taken
-- directly from the documentation for the decNumber C library.
--
-- In particular, this module implements the decSingle type.  decSingle
-- supports up to 34 digits of precision and exponents between -6176
-- and 6111.  It doesn't silently round, overflow, or underflow;
-- rather, the library will notify you if these things happen.
--
-- Many functions in this module clash with Prelude names, so you
-- might want to do
--
-- > import qualified Deka.Single as Q
module Deka.Internal.Single.CtxFree where

-- # Imports

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

import Deka.Internal.Decnumber.DecSingle
import Deka.Internal.Decnumber.Decimal32
import Deka.Internal.DecNum.DecNum
import Deka.Internal.DecNum.Util
import Deka.Internal.Single.Single

-- # Helpers.  Do not export these.

type UnaryGet a
  = Ptr C'decSingle
  -> IO a

unaryGet
  :: UnaryGet a
  -> Single
  -> IO a
unaryGet f d =
  withForeignPtr (unSingle d) $ \pD -> f pD

-- # End Helpers

-- # Functions from decSingle. In alphabetical order.

-- | Returns a string in engineering notation.
--
-- In the decNumber C library, this is called @toEngString@; the
-- name is changed here because the function does not return a
-- regular Haskell 'String'.
toEngByteString :: Single -> IO BS8.ByteString
toEngByteString = mkString c'decSingleToEngString

-- | Identifies the version of the decNumber C library.
version :: IO BS8.ByteString
version =
  c'decSingleVersion >>= BS8.packCString

-- | A Single whose coefficient, exponent, and sign are all 0.
zero :: IO Single
zero =
  newSingle >>= \d ->
  withForeignPtr (unSingle d) $ \pD ->
  c'decSingleZero pD >>
  return d

-- Conversions to decNumber

-- | Converts a Single to a decNumber.
toNumber :: Single -> IO DecNum
toNumber (Single fp) =
  newDecNumSize c'DECSINGLE_Pmax >>= \dn ->
  withForeignPtr fp $ \ptrSingle ->
  withForeignPtr (unDecNum dn) $ \ptrDn ->
  c'decimal32ToNumber (castPtr ptrSingle) ptrDn >>
  return dn

