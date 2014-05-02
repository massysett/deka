module Dectest.Interp.Operand where

import Dectest.Interp.Octothorpe
import Deka.Internal.Context
import qualified Data.ByteString.Char8 as BS8
import Foreign.Safe
import qualified Deka.Fixed.Single as S
import qualified Deka.Internal.Decnumber.Decimal32 as D32
import qualified Deka.Internal.DecNum.Ctx as D
import Deka.Internal.DecNum.DecNum

operandNum
  :: WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe DecNum)
operandNum wp bs = case parseOcto bs of
  Null -> return Nothing
  Octo op -> fmap (Just . DecNum) $ opOperandDec op wp
  NotOcto -> do
    pOld <- getPrecision
    let pNew = case wp of
          FromCtx -> pOld
          DoNotRound -> maybe (error "operandNum: precision too large")
            id . precision . fromIntegral . BS8.length $ bs
    setPrecision pNew
    dn <- D.fromByteString bs
    setPrecision pOld
    return . Just $ dn

operand32
  :: WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe S.Single)
operand32 = undefined
