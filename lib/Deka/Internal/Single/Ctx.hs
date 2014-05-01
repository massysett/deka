{-# LANGUAGE Safe #-}
module Deka.Internal.Single.Ctx where

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
import Deka.Context
import Deka.Internal.Context

import Deka.Internal.DecNum.DecNum
import Deka.Internal.Decnumber.DecSingle
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.Decimal32
import Deka.Internal.Single.Single

type Unary
  = Ptr C'decSingle
  -> Ptr C'decSingle
  -> Ptr C'decContext
  -> IO (Ptr C'decSingle)

unary
  :: Unary
  -> Single
  -> Ctx Single
unary f d = Ctx $ \ptrC ->
  newSingle >>= \r ->
  withForeignPtr (unSingle d) $ \ptrX ->
  withForeignPtr (unSingle r) $ \ptrR ->
  f ptrR ptrX ptrC >>
  return r

type Binary
  = Ptr C'decSingle
  -> Ptr C'decSingle
  -> Ptr C'decSingle
  -> Ptr C'decContext
  -> IO (Ptr C'decSingle)

binary
  :: Binary
  -> Single
  -> Single
  -> Ctx Single
binary f x y = Ctx $ \pC ->
  newSingle >>= \r ->
  withForeignPtr (unSingle r) $ \pR ->
  withForeignPtr (unSingle x) $ \pX ->
  withForeignPtr (unSingle y) $ \pY ->
  f pR pX pY pC >>
  return r

type Ternary
  = Ptr C'decSingle
  -> Ptr C'decSingle
  -> Ptr C'decSingle
  -> Ptr C'decSingle
  -> Ptr C'decContext
  -> IO (Ptr C'decSingle)

ternary
  :: Ternary
  -> Single
  -> Single
  -> Single
  -> Ctx Single
ternary f x y z = Ctx $ \pC ->
  newSingle >>= \r ->
  withForeignPtr (unSingle r) $ \pR ->
  withForeignPtr (unSingle x) $ \pX ->
  withForeignPtr (unSingle y) $ \pY ->
  withForeignPtr (unSingle z) $ \pZ ->
  f pR pX pY pZ pC
  >> return r

type GetRounded a
  = Ptr C'decSingle
  -> Ptr C'decContext
  -> C'rounding
  -> IO a

getRounded
  :: GetRounded a
  -> Round
  -> Single
  -> Ctx a
getRounded f (Round r) d = Ctx $ \pC ->
  withForeignPtr (unSingle d) $ \pD ->
  f pD pC r

-- # End Helpers

-- | Reads a ByteString, which can be in scientific, engineering, or
-- \"regular\" decimal notation.  Also reads NaN, Infinity, etc.
-- Will return a signaling NaN and set 'invalidOperation' if the
-- string given is invalid.
--
-- In the decNumber C library, this function was called
-- @fromString@; the name was changed here because it doesn't take a
-- regular Haskell 'String'.
fromByteString :: BS8.ByteString -> Ctx Single
fromByteString s = Ctx $ \pC ->
  newSingle >>= \r ->
  withForeignPtr (unSingle r) $ \pR ->
  BS8.useAsCString s $ \pS ->
  c'decSingleFromString pR pS pC >>
  return r

-- | Runs a computation with the decimal32 default context.
runSingle :: Ctx a -> a
runSingle = Deka.Context.runCtx initDecimal32

-- | Runs a computation with the decimal32 default context, and
-- returns any resulting flags.
runSingleStatus :: Ctx a -> (a, [Flag])
runSingleStatus a = runSingle $ do
  r <- a
  f <- getStatus
  return (r, f)

-- Conversion from decNumber

-- | Converts a 'DecNum' to a 'Single'.  The possible errors are the
-- same as for the 'fromByteString' function, except that
-- 'conversionSyntax' is not possible.

fromNumber :: DecNum -> Ctx Single
fromNumber (DecNum fpd) = Ctx $ \pCtx ->
  newSingle >>= \r ->
  withForeignPtr (unSingle r) $ \pR ->
  withForeignPtr fpd $ \pDn ->
  c'decimal32FromNumber (castPtr pR) pDn pCtx >>
  return r
