module Dectest.Interp.Result where

import Dectest.Interp.Octothorpe
import Deka.DecNum
import qualified Deka.DecNum as N
import qualified Deka.Fixed.Single as S
import qualified Deka.Fixed.Double as D
import qualified Deka.Fixed.Quad as Q
import Deka.Context
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Class as C
import Data.Char (toLower)

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

instance ToByteString Ordering where
  toByteString = BS8.pack . show

instance ToByteString C.Class where
  toByteString = BS8.pack . show

instance ToByteString Bool where
  toByteString = BS8.pack . show

class Result a where
  result :: BS8.ByteString -> Maybe (a -> Ctx Bool)

instance Result Bool where
  result bs = case reads . BS8.unpack $ bs of
    (x,""):[]
      | x == (1 :: Int) -> Just $ return
      | x == 0 -> Just $ return . not
      | otherwise -> error "result: could not read boolean"
    _ -> error "result: could not read boolean"

instance Result Ordering where
  result bs = case reads . BS8.unpack $ bs of
    (x,""):[]
      | x < (0 :: Int) -> Just $ \a -> return (a == LT)
      | x > 0 -> Just $ \a -> return (a == GT)
      | otherwise -> Just $ \a -> return (a == EQ)
    _ -> error "result: ordering: could not parse number"

instance Result DecNum where
  result = mkresult opResultDec N.toByteString

instance Result S.Single where
  result = mkresult opResult32 S.toByteString

instance Result D.Double where
  result = mkresult opResult64 D.toByteString

instance Result Q.Quad where
  result = mkresult opResult128 Q.toByteString

instance Result C.Class where
  result bs = Just $ \a -> return (a == cls)
    where
      cls
        | s == "snan" = C.sNaN
        | s == "nan" = C.qNaN
        | s == "-infinity" = C.negInf
        | s == "-normal" = C.negNormal
        | s == "-subnormal" = C.negSubnormal
        | s == "-zero" = C.negZero
        | s == "+zero" = C.posZero
        | s == "+subnormal" = C.posSubnormal
        | s == "+normal" = C.posNormal
        | s == "+infinity" = C.posInf
        | otherwise = error "result: could not parse class"
        where
          s = map toLower . BS8.unpack $ bs
