module Dectest.Interp.Result where

import Dectest.Interp.Octothorpe
import Deka.DecNum
import qualified Deka.DecNum as N
import qualified Deka.Fixed.Single as S
import qualified Deka.Fixed.Double as D
import qualified Deka.Fixed.Quad as Q
import Deka.Context
import qualified Data.ByteString.Char8 as BS8

mkresult
  :: (OctoParsers -> a -> Ctx Bool)
  -> (a -> BS8.ByteString)
  -> BS8.ByteString
  -> Maybe (a -> Ctx Bool)
mkresult getCmp toStr bs = case parseOcto bs of
  Null -> Nothing
  Octo op -> Just $ \a -> getCmp op a
  NotOcto -> Just $ \a -> return $ toStr a == bs

class ToByteString a where
  toByteString :: a -> BS8.ByteString

instance ToByteString DecNum where
  toByteString = N.toByteString

instance ToByteString S.Single where
  toByteString = S.toByteString

instance ToByteString D.Double where
  toByteString = D.toByteString

instance ToByteString Q.Quad where
  toByteString = Q.toByteString

class Result a where
  result :: BS8.ByteString -> Maybe (a -> Ctx Bool)

instance Result DecNum where
  result = mkresult opResultDec N.toByteString

instance Result S.Single where
  result = mkresult opResult32 S.toByteString

instance Result D.Double where
  result = mkresult opResult64 D.toByteString

instance Result Q.Quad where
  result = mkresult opResult128 Q.toByteString

