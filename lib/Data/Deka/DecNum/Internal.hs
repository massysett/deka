{-# LANGUAGE EmptyDataDecls, Trustworthy #-}

module Data.Deka.DecNum.Internal where

import Foreign.Safe
import Data.Deka.Decnumber.DecNumber
import qualified Data.ByteString.Char8 as BS8
import System.IO.Unsafe (unsafePerformIO)

data DPtr

newtype DecNum = DecNum { unDecNum :: ForeignPtr DPtr }

toByteString :: DecNum -> BS8.ByteString
toByteString dn = unsafePerformIO $
  withForeignPtr (unDecNum dn) $ \pDn ->
  peek (p'decNumber'digits (castPtr pDn)) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToString (castPtr pDn) pStr >>
  BS8.packCString pStr

instance Show DecNum where
  show = BS8.unpack . toByteString

