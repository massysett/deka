{-# LANGUAGE OverloadedStrings #-}
-- | Parse octothorpe-containing operands and results.

module Dectest.Parse.Octothorpe
  ( OctoParsers(..)
  , Parsed(..)
  , parseOcto
  ) where

import Deka.Internal.Context
import qualified Deka.Internal.Decnumber.DecNumber as D
import qualified Deka.Internal.Decnumber.Decimal32 as D32
import qualified Deka.Internal.Decnumber.Decimal64 as D64
import qualified Deka.Internal.Decnumber.Decimal128 as D128
import Foreign.Safe
import qualified Data.ByteString.Char8 as BS8

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
