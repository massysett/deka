module Dectest.Interp.Result where

import Dectest.Interp.Octothorpe
import Deka.DecNum
import qualified Deka.DecNum as N
import qualified Deka.Fixed.Single as S
import qualified Deka.Fixed.Double as D
import qualified Deka.Fixed.Quad as Q
import Deka.Context
import qualified Data.ByteString.Char8 as BS8

result
  :: (OctoParsers -> a -> Ctx Bool)
  -> (a -> BS8.ByteString)
  -> BS8.ByteString
  -> a
  -> Ctx (Maybe Bool)
result getCmp toStr bs a = case parseOcto bs of
  Null -> return Nothing
  Octo op -> fmap Just $ getCmp op a
  NotOcto -> return . Just $ bsR == bs
    where
      bsR = toStr a

resultDec
  :: BS8.ByteString
  -> DecNum
  -> Ctx (Maybe Bool)
resultDec = result opResultDec N.toByteString

result32
  :: BS8.ByteString
  -> S.Single
  -> Ctx (Maybe Bool)
result32 = result opResult32 S.toByteString

result64
  :: BS8.ByteString
  -> D.Double
  -> Ctx (Maybe Bool)
result64 = result opResult64 D.toByteString

result128
  :: BS8.ByteString
  -> Q.Quad
  -> Ctx (Maybe Bool)
result128 = result opResult128 Q.toByteString
