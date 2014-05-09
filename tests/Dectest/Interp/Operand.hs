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

type ParseString r
  = (OctoParsers -> WhichPrecision -> Ctx r)
  -- ^ Gets the result from the OctoParsers

  -> Int32
  -- ^ Precision to use for DoNotRound

  -> (BS8.ByteString -> Ctx r)
  -- ^ Parses result if not an octothorpe

  -> WhichPrecision
  -> Ctx r


interpOp
  :: BS8.ByteString
  -- ^ String to parse
  -> Maybe (ParseString r)

interpOp bs = case parseOcto bs of
  Null -> Nothing
  Octo op -> Just $ \getOcto _ _ wp -> getOcto op wp
  NotOcto -> Just $ \_ pcsn prsr wp -> do
    pOld <- getPrecision
    let pNew = case wp of
          FromCtx -> pOld
          DoNotRound -> maybe (error "interpOp: precision too large")
            id . precision $ pcsn
    setPrecision pNew
    dn <- prsr bs
    setPrecision pOld
    return dn

class Operand a where
  operand :: BS8.ByteString -> Maybe (WhichPrecision -> Ctx a)

instance Operand DecNum where
  operand bs = case interpOp bs of
    Nothing -> Nothing
    Just fParse -> Just $ \wp ->
      fParse opOperandDec (fromIntegral . BS8.length $ bs)
      N.fromByteString wp

instance Operand S.Single where
  operand bs =
    fmap (\f wp -> f opOperand32 S.c'DECSINGLE_Pmax S.fromByteString wp)
    $ interpOp bs

instance Operand D.Double where
  operand bs =
    fmap (\f wp -> f opOperand64 D.c'DECDOUBLE_Pmax D.fromByteString wp)
    $ interpOp bs


instance Operand Q.Quad where
  operand bs =
    fmap (\f wp -> f opOperand128 Q.c'DECQUAD_Pmax Q.fromByteString wp)
    $ interpOp bs


