module Dectest.Interp.Operand where

import Dectest.Interp.Octothorpe
import Deka.Internal.Context
import qualified Data.ByteString.Char8 as BS8
import Foreign.Safe
import qualified Deka.Internal.Decnumber.DecNumber as D
import qualified Deka.Internal.Decnumber.Decimal32 as D32
import qualified Deka.Internal.DecNum.Ctx as D
import Deka.Internal.DecNum.DecNum

operandNum
  :: WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe (ForeignPtr D.C'decNumber))
operandNum wp bs = case parseOcto bs of
  Null -> return Nothing
  Octo op -> fmap Just $ opOperandDec op wp
  NotOcto -> do
    pOld <- getPrecision
    let pNew = case wp of
          FromCtx -> pOld
          DoNotRound -> maybe (error "operandNum: precision too large")
            id . precision . fromIntegral . BS8.length $ bs
    setPrecision pNew
    dn <- D.fromByteString bs
    setPrecision pOld
    return . Just . unDecNum $ dn

operand32
  :: WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe (ForeignPtr D32.C'decimal32))
operand32 = undefined
