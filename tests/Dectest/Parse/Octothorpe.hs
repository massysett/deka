{-# LANGUAGE OverloadedStrings #-}
-- | Parse octothorpe-containing operands and results.

module Dectest.Parse.Octothorpe
  ( OctoParsers(..)
  , Parsed(..)
  , parseOcto
  ) where

import System.IO.Unsafe
import Control.Monad (void)
import Dectest.Binary
import qualified Deka.DecNum as DN
import Deka.Internal.DecNum.DecNum
import Deka.Context
import Deka.Internal.Context
import qualified Deka.Internal.Decnumber.DecNumber as D
import qualified Deka.Internal.Decnumber.Decimal32 as D32
import qualified Deka.Internal.Decnumber.Decimal64 as D64
import qualified Deka.Internal.Decnumber.Decimal128 as D128
import Foreign.Safe hiding (void)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS

type ApplyPrecision = Bool

data Parsed
  = NotOcto
  | Null
  | Octo OctoParsers

isNull :: BS8.ByteString -> Bool
isNull = (== "#")

-- | When parsing a result token, parseOcto returns a function that
-- you use to compare the result of the test computation to the
-- token.
type CompareResult a = ForeignPtr a -> Ctx Bool

type ConvertOp a = ApplyPrecision -> Ctx (ForeignPtr a)

data OctoParsers = OctoParsers
  { opOperandDec :: ConvertOp D.C'decNumber
  , opOperand32 :: ConvertOp D32.C'decimal32
  , opOperand64 :: ConvertOp D64.C'decimal64
  , opOperand128 :: ConvertOp D128.C'decimal128
  , opResultDec :: CompareResult D.C'decNumber
  , opResult32 :: CompareResult D32.C'decimal32
  , opResult64 :: CompareResult D64.C'decimal64
  , opResult128 :: CompareResult D128.C'decimal128
  }

parseOcto :: BS8.ByteString -> Parsed
parseOcto s
  | isNull s = Null
  | not ('#' `BS8.elem` s) = NotOcto
  | otherwise = Octo $ OctoParsers
      { opOperandDec = opDec s
      , opOperand32 = op32 s
      , opOperand64 = op64 s
      , opOperand128 = op128 s
      , opResultDec = rsDec s
      , opResult32 = rs32 s
      , opResult64 = rs64 s
      , opResult128 = rs128 s
      }

opDec :: BS8.ByteString -> ConvertOp D.C'decNumber
opDec = undefined

op32 :: BS8.ByteString -> ConvertOp D32.C'decimal32
op32 = undefined

op64 :: BS8.ByteString -> ConvertOp D64.C'decimal64
op64 = undefined

op128 :: BS8.ByteString -> ConvertOp D128.C'decimal128
op128 = undefined

rsDec :: BS8.ByteString -> CompareResult D.C'decNumber
rsDec = undefined

rs32 :: BS8.ByteString -> CompareResult D32.C'decimal32
rs32 = undefined

rs64 :: BS8.ByteString -> CompareResult D64.C'decimal64
rs64 = undefined

rs128 :: BS8.ByteString -> CompareResult D128.C'decimal128
rs128 = undefined

data OctoHex
  = H32 (ForeignPtr D32.C'decimal32)
  | H64 (ForeignPtr D64.C'decimal64)
  | H128 (ForeignPtr D128.C'decimal128)

-- | Decode an octothorpe that represents a hex format.  Octothorpe
-- must already be cleaved from the front of the list.

octoHex :: BS8.ByteString -> OctoHex
octoHex bs
  | len == 8 = go H32 32
  | len == 16 = go H64 64
  | len == 32 = go H128 128
  | otherwise = error "octoHex: unknown length"
  where
    len = BS8.length bs
    go ctor bytes = unsafePerformIO $
      let hex = packHexList bs in
      mallocForeignPtrBytes bytes >>= \fp ->
      withForeignPtr fp $ \ptr ->
      BS.useAsCString hex $ \cstr ->
      copyBytes (castPtr ptr) cstr bytes >>
      return (ctor fp)


-- | Decode an octothorpe that represents a string.  The octothorpe
-- and the digits representing the string should still be in the
-- ByteString.

octoString :: BS8.ByteString -> ApplyPrecision -> Ctx OctoHex
octoString bs ap
  | lead == "32" = go H32 f32 32
  | lead == "64" = go H64 f64 64
  | lead == "128" = go H128 f128 128
  | otherwise = error "octoString: unknown length"
  where
    lead = BS8.takeWhile (/= '#') bs
    rest = BS8.drop 1 $ BS8.dropWhile (/= '#') bs
    f32 = \d n c -> void $ D32.c'decimal32FromNumber (castPtr d) n c
    f64 = \d n c -> void $ D64.c'decimal64FromNumber (castPtr d) n c
    f128 = \d n c -> void $ D128.c'decimal128FromNumber (castPtr d) n c
    go ctor mkNum bytes = Ctx $ \pCtx ->
      unCtx getPrecision pCtx >>= \oldP ->
      let newP | ap = oldP
               | otherwise = maybe (error "octoString: precision too large")
                  id (precision (fromIntegral (BS8.length rest))) in
      unCtx (setPrecision newP) pCtx >>= \_ ->
      unCtx (DN.fromByteString rest) pCtx >>= \dn ->
      unCtx (setPrecision oldP) pCtx >>= \_ ->
      withForeignPtr (unDecNum dn) $ \pDn ->
      mallocForeignPtrBytes bytes >>= \fpR ->
      withForeignPtr fpR $ \pR ->
      mkNum pR pDn pCtx >>
      return (ctor fpR)

--
--
--

{- Parsing Operand Octothorpes

   Branch 1.  Octothorpe alone is a null reference.

   Branch 2.  Octothorpe followed by 8, 16, or 32 hexadecimal digits
   is a decimal16, decimal32, or decimal64, respectively.

       Step a.  Decode the hex digits without loss or constraint
       into the respective decimalXX format.

       Step b.  If the decimalXX format must be converted to the format
       being used for the test calculation, do so now.  decimalXX to
       decNumber conversions are lossless.  There are no instances
       of decimalXX formats that require conversion to another
       decimalXX format.

       Step c.  Apply the directives to the operand.  Apply precision
       only when the operation is toSci, toEng, or apply; otherwise,
       use sufficient precision so that rounding is avoided.
       (precision will be an issue only when converting to
       decNumber.)  This step may set flags.

   Branch 3.  2 digits followed by an octothorpe followed by a numeric
   string specifies a number in the specified format.  (These occur
   only in decNumber tests.)

       Step a.  Convert the numeric string to a decNumber.  Then, convert
       the resulting decNumber to the target decimalXX format.  This
       may cause rounding or other conditions; the flags are set as
       usual.

       Step b.  Follow step 2b and 2c above.
-}

{- Parsing Result Octothorpes

   Branch 1.  Octothorpe alone is a null reference.

   Branch 2.  Octothorpe followed by 8, 16, or 32 hexadecimal digits
   is a decimal16, decimal32, or decimal64, respectively.

       Step a.  Decode the hex digits without loss or constraint
       into the respective decimalXX format.

       Step b.  Convert the result of the test calculation to the
       type of the given decoding.  This may modify or clamp the
       result to fit the format, giving a different result than if
       the result had been expressed as a string and possibly
       raising new conditions.

   Branch 3.  2 digits followed by an octothorpe followed by a numeric
   string specifies a number in the specified format.  (These occur
   only in decNumber tests.)

       Step a.  The numeric string is first converted to a decNumber
       and is then encoded in the selected format.  This may cause
       rounding or other conditions, but NO conditions are set by
       this process.

       Step b.  Follow step 2b above.

-}
