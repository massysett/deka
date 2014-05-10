{-# LANGUAGE Trustworthy #-}
module Deka.Internal.Dec.Dec where

import qualified Data.ByteString.Char8 as BS8
import Foreign.Safe
import System.IO.Unsafe (unsafePerformIO)
import Deka.Internal.Decnumber.DecNumber

newtype Dec = Dec { unDec :: ForeignPtr C'decNumber }

instance Show Dec where
  show = BS8.unpack . toByteString

toByteStringIO :: Dec -> IO BS8.ByteString
toByteStringIO dn =
  withForeignPtr (unDec dn) $ \pDn ->
  peek (p'decNumber'digits pDn) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToString pDn pStr >>
  BS8.packCString pStr

toByteString :: Dec -> BS8.ByteString
toByteString = fmap unsafePerformIO toByteStringIO

