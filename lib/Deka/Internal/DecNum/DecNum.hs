{-# LANGUAGE EmptyDataDecls, Trustworthy #-}
module Deka.Internal.DecNum.DecNum where

import qualified Data.ByteString.Char8 as BS8
import Foreign.Safe
import System.IO.Unsafe (unsafePerformIO)
import Deka.Internal.Decnumber.DecNumber

data DPtr

newtype DecNum = DecNum { unDecNum :: ForeignPtr DPtr }

instance Show DecNum where
  show = BS8.unpack . toByteString

toByteStringIO :: DecNum -> IO BS8.ByteString
toByteStringIO dn =
  withForeignPtr (unDecNum dn) $ \pDn ->
  peek (p'decNumber'digits (castPtr pDn)) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToString (castPtr pDn) pStr >>
  BS8.packCString pStr

toByteString :: DecNum -> BS8.ByteString
toByteString = fmap unsafePerformIO toByteStringIO

