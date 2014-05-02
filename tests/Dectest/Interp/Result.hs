module Dectest.Interp.Result where

import Dectest.Interp.Octothorpe
import Deka.DecNum
import qualified Deka.DecNum as N
import Deka.Context
import qualified Data.ByteString.Char8 as BS8

resultDec
  :: BS8.ByteString
  -> DecNum
  -> Ctx (Maybe Bool)
resultDec bs dn = case parseOcto bs of
  Null -> return Nothing
  Octo op -> fmap Just $ opResultDec op dn
  NotOcto -> undefined
