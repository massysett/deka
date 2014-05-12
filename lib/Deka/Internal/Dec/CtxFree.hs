{-# LANGUAGE EmptyDataDecls, Safe #-}

module Deka.Internal.Dec.CtxFree where

import Foreign.Safe
import Deka.Internal.Dec.Dec
import Deka.Internal.Decnumber.DecNumber
import qualified Data.ByteString.Char8 as BS8
import Prelude
import Foreign.C.Types
import Deka.Internal.Dec.Util

-- # Conversions

fromInt32 :: Int32 -> IO Dec
fromInt32 i = do
  dn <- newDecSize 10
  withForeignPtr (unDec dn) $ \ptr -> do
    _ <- c'decNumberFromInt32 ptr i
    return dn

fromUInt32 :: Word32 -> IO Dec
fromUInt32 i = do
  dn <- newDecSize 10
  withForeignPtr (unDec dn) $ \ptr -> do
    _ <- c'decNumberFromUInt32 ptr i
    return dn

toEngByteString :: Dec -> IO BS8.ByteString
toEngByteString dn =
  withForeignPtr (unDec dn) $ \pDn ->
  peek (p'decNumber'digits pDn) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToEngString pDn pStr >>
  BS8.packCString pStr

sameQuantum :: Dec -> Dec -> IO Dec
sameQuantum (Dec x) (Dec y) =
  withForeignPtr x $ \px ->
  withForeignPtr y $ \py ->
  oneDigitDec >>= \o ->
  withForeignPtr (unDec o) $ \po ->
  c'decNumberSameQuantum po px py >>
  return o

copyAbs
  :: Dec
  -- ^ Source of sign
  -> Dec
  -- ^ Copy sign to this destination
  -> IO Dec
  -- ^ Result
copyAbs src dest =
  copyDec dest >>= \r ->
  withForeignPtr (unDec r) $ \pr ->
  withForeignPtr (unDec src) $ \ps ->
  c'decNumberCopyAbs pr ps >>
  return r

-- CopyNegate, CopySign

negate :: Dec -> IO Dec
negate src =
  copyDec src >>= \r ->
  withForeignPtr (unDec r) $ \pr ->
  withForeignPtr (unDec src) $ \ps ->
  c'decNumberCopyNegate pr ps >>
  return r

copySign
  :: Dec
  -- ^ Source of content (except sign)
  -> Dec
  -- ^ Source of sign
  -> IO Dec
copySign src sgn =
  withForeignPtr (unDec src) $ \pc ->
  peek (p'decNumber'digits pc) >>= \dgts ->
  newDecSize dgts >>= \dn' ->
  withForeignPtr (unDec dn') $ \dp' ->
  withForeignPtr (unDec sgn) $ \pn ->
  c'decNumberCopySign dp' pc pn >>
  return dn'

trim :: Dec -> IO Dec
trim src =
  copyDec src >>= \dest ->
  withForeignPtr (unDec dest) $ \pd ->
  c'decNumberTrim pd >>
  return dest

version :: IO BS8.ByteString
version =
  c'decNumberVersion >>= \pv ->
  BS8.packCString pv

zero :: IO Dec
zero =
  oneDigitDec >>= \od ->
  withForeignPtr (unDec od) $ \pod ->
  c'decNumberZero pod >>
  return od

testBool
  :: (Ptr C'decNumber -> IO CInt)
  -> Dec
  -> IO Bool
testBool f (Dec dn) =
  withForeignPtr dn $ \pn ->
  f pn >>= \bl ->
  return (toBool bl)

isCanonical :: Dec -> IO Bool
isCanonical = testBool c'decNumberIsCanonical

isFinite :: Dec -> IO Bool
isFinite = testBool c'decNumberIsFinite

isInfinite :: Dec -> IO Bool
isInfinite = testBool c'decNumberIsInfinite

isNaN :: Dec -> IO Bool
isNaN = testBool c'decNumberIsNaN

isNegative :: Dec -> IO Bool
isNegative = testBool c'decNumberIsNegative

isQNaN :: Dec -> IO Bool
isQNaN = testBool c'decNumberIsQNaN

isSNaN :: Dec -> IO Bool
isSNaN = testBool c'decNumberIsSNaN

isSpecial :: Dec -> IO Bool
isSpecial = testBool c'decNumberIsSpecial

isZero :: Dec -> IO Bool
isZero = testBool c'decNumberIsZero

