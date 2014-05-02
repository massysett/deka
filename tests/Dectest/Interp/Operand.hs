module Dectest.Interp.Operand where

import Dectest.Interp.Octothorpe
import Deka.Internal.Context
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Internal.Decnumber.DecSingle as S
import qualified Deka.Fixed.Single as S
import qualified Deka.Internal.Decnumber.DecDouble as D
import qualified Deka.Fixed.Double as D
import qualified Deka.Internal.Decnumber.DecQuad as Q
import qualified Deka.Fixed.Quad as Q
import qualified Deka.Internal.DecNum.Ctx as N
import Deka.Internal.DecNum.DecNum
import Data.Int

interpOp
  :: (OctoParsers -> WhichPrecision -> Ctx r)
  -- ^ Gets the result from the OctoParsers

  -> Int32
  -- ^ Precision to use for DoNotRound

  -> (BS8.ByteString -> Ctx r)
  -- ^ Parses result if not an octothorpe

  -> WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe r)

interpOp getOcto pcsn prsr wp bs = case parseOcto bs of
  Null -> return Nothing
  Octo op -> fmap Just $ getOcto op wp
  NotOcto -> do
    pOld <- getPrecision
    let pNew = case wp of
          FromCtx -> pOld
          DoNotRound -> maybe (error "interpOp: precision too large")
            id . precision $ pcsn
    setPrecision pNew
    dn <- prsr bs
    setPrecision pOld
    return . Just $ dn

operandNum
  :: WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe DecNum)
operandNum wp bs = interpOp opOperandDec (fromIntegral . BS8.length $ bs)
  N.fromByteString wp bs

operand32
  :: WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe S.Single)
operand32 = interpOp opOperand32 S.c'DECSINGLE_Pmax S.fromByteString

operand64
  :: WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe D.Double)
operand64 = interpOp opOperand64 D.c'DECDOUBLE_Pmax D.fromByteString

operand128
  :: WhichPrecision
  -> BS8.ByteString
  -> Ctx (Maybe Q.Quad)
operand128 = interpOp opOperand128 Q.c'DECQUAD_Pmax Q.fromByteString

