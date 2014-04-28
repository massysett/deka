-- | Parses operand tokens.

module Dectest.Parse.Operand where

import Deka.Context
import Deka.Internal.Decnumber.Decimal32
import Deka.Internal.Decnumber.Decimal64
import Deka.Internal.Decnumber.Decimal128
import Foreign.Safe
import qualified Data.ByteString.Char8 as BS8

data NumToken a
  = Null
  | Requested a
  | Dec32 (ForeignPtr C'decimal32)
  | Dec64 (ForeignPtr C'decimal64)
  | Dec128 (ForeignPtr C'decimal128)

type UsePrecision = Bool

parseOperand
  :: UsePrecision
  -- ^ If True, use the precision from the context.  If False, use
  -- sufficient precision so that rounding is avoided.  Only set to
  -- True when the operation is toSci, toEng, or apply.
  -> (BS8.ByteString -> Ctx a)
  -- ^ Use this function to convert to the target number, if there
  -- is no octothorpe

  -> BS8.ByteString
  -- ^ Parse this string

  -> Ctx (NumToken a)
parseOperand = undefined

-- | Parses a result token.  Token is always converted to a number
-- without constraints; no flags are set by this process.
parseResult
  :: (BS8.ByteString -> Ctx a)
  -- ^ Use this function to convert to the targe number, if there is
  -- no octothorpe.

  -> BS8.ByteString
  -- ^ Parse this string
  -> NumToken a
parseResult = undefined
