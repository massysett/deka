{-# LANGUAGE EmptyDataDecls, Trustworthy #-}

module Deka.DecNum.Internal where

import Foreign.Safe
import Deka.Decnumber.DecNumber
import qualified Data.ByteString.Char8 as BS8
import System.IO.Unsafe (unsafePerformIO)
import Deka.Decnumber.Types
import Deka.Decnumber.Context
import Prelude
import qualified Prelude as P
import Foreign.C.Types

data DPtr

newtype DecNum = DecNum { unDecNum :: ForeignPtr DPtr }

-- # Utilities

newDecNumSize :: C'int32_t -> IO DecNum
newDecNumSize i = do
  let sz = mallocAmount i
  fp <- mallocForeignPtrBytes sz
  return $ DecNum fp

-- | How many bytes must be malloc'ed to hold this many
-- digits?
mallocAmount
  :: C'int32_t
  -- ^ Number of digits
  -> Int
  -- ^ Malloc this many bytes total
mallocAmount s = base + extra
  where
    base = sizeOf (undefined :: C'decNumber)
    baseUnits = c'DECNUMUNITS
    totUnits = (s + c'DECDPUN - 1) `quot` c'DECDPUN
    extraUnits = P.max 0 $ totUnits - baseUnits
    extra = sizeOf (undefined :: C'decNumberUnit) * fromIntegral extraUnits

oneDigitDecNum :: IO DecNum
oneDigitDecNum = do
  fp <- mallocForeignPtrBytes (sizeOf (undefined :: C'decNumber))
  return $ DecNum (castForeignPtr fp)

newDecNum :: Ptr C'decContext -> IO DecNum
newDecNum p = do
  dgts <- peek (p'decContext'digits p)
  let sz = mallocAmount dgts
  fp <- mallocForeignPtrBytes sz
  return $ DecNum fp

copyDecNum :: DecNum -> IO DecNum
copyDecNum (DecNum p) = withForeignPtr p $ \dp ->
  peek (p'decNumber'digits (castPtr dp)) >>= \dgts ->
  newDecNumSize dgts >>= \dn' ->
  withForeignPtr (unDecNum dn') $ \dp' ->
  c'decNumberCopy (castPtr dp') (castPtr dp) >>
  return dn'

unsafe0 :: IO a -> a
unsafe0 = unsafePerformIO

unsafe1 :: (a -> IO b) -> a -> b
unsafe1 = fmap unsafePerformIO

unsafe2 :: (a -> b -> IO c) -> a -> b -> c
unsafe2 = fmap (fmap unsafePerformIO)

-- # Conversions

toByteStringIO :: DecNum -> IO BS8.ByteString
toByteStringIO dn =
  withForeignPtr (unDecNum dn) $ \pDn ->
  peek (p'decNumber'digits (castPtr pDn)) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToString (castPtr pDn) pStr >>
  BS8.packCString pStr

toByteString :: DecNum -> BS8.ByteString
toByteString = unsafe1 toByteStringIO

instance Show DecNum where
  show = BS8.unpack . toByteString

fromInt32 :: C'int32_t -> IO DecNum
fromInt32 i = do
  dn <- newDecNumSize 10
  withForeignPtr (unDecNum dn) $ \ptr -> do
    _ <- c'decNumberFromInt32 (castPtr ptr) i
    return dn

fromUInt32 :: C'uint32_t -> IO DecNum
fromUInt32 i = do
  dn <- newDecNumSize 10
  withForeignPtr (unDecNum dn) $ \ptr -> do
    _ <- c'decNumberFromUInt32 (castPtr ptr) i
    return dn

toEngByteString :: DecNum -> IO BS8.ByteString
toEngByteString dn =
  withForeignPtr (unDecNum dn) $ \pDn ->
  peek (p'decNumber'digits (castPtr pDn)) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToEngString (castPtr pDn) pStr >>
  BS8.packCString pStr

sameQuantum :: DecNum -> DecNum -> IO DecNum
sameQuantum (DecNum x) (DecNum y) =
  withForeignPtr x $ \px ->
  withForeignPtr y $ \py ->
  oneDigitDecNum >>= \o ->
  withForeignPtr (unDecNum o) $ \po ->
  c'decNumberSameQuantum (castPtr po) (castPtr px) (castPtr py) >>
  return o

copyAbs
  :: DecNum
  -- ^ Source of sign
  -> DecNum
  -- ^ Copy sign to this destination
  -> IO DecNum
  -- ^ Result
copyAbs src dest =
  copyDecNum dest >>= \r ->
  withForeignPtr (unDecNum r) $ \pr ->
  withForeignPtr (unDecNum src) $ \ps ->
  c'decNumberCopyAbs (castPtr pr) (castPtr ps) >>
  return r

-- CopyNegate, CopySign

negate :: DecNum -> IO DecNum
negate src =
  copyDecNum src >>= \r ->
  withForeignPtr (unDecNum r) $ \pr ->
  withForeignPtr (unDecNum src) $ \ps ->
  c'decNumberCopyNegate (castPtr pr) (castPtr ps) >>
  return r

copySign
  :: DecNum
  -- ^ Source of content (except sign)
  -> DecNum
  -- ^ Source of sign
  -> IO DecNum
copySign src sgn =
  withForeignPtr (unDecNum src) $ \pc ->
  peek (p'decNumber'digits (castPtr pc)) >>= \dgts ->
  newDecNumSize dgts >>= \dn' ->
  withForeignPtr (unDecNum dn') $ \dp' ->
  withForeignPtr (unDecNum sgn) $ \pn ->
  c'decNumberCopySign (castPtr dp') (castPtr pc) (castPtr pn) >>
  return dn'

trim :: DecNum -> IO DecNum
trim src =
  copyDecNum src >>= \dest ->
  withForeignPtr (unDecNum dest) $ \pd ->
  c'decNumberTrim (castPtr pd) >>
  return dest

version :: IO BS8.ByteString
version =
  c'decNumberVersion >>= \pv ->
  BS8.packCString pv

zero :: IO DecNum
zero =
  oneDigitDecNum >>= \od ->
  withForeignPtr (unDecNum od) $ \pod ->
  c'decNumberZero (castPtr pod) >>
  return od

testBool
  :: (Ptr C'decNumber -> IO CInt)
  -> DecNum
  -> IO Bool
testBool f (DecNum dn) =
  withForeignPtr dn $ \pn ->
  f (castPtr pn) >>= \bl ->
  return (toBool bl)

isCanonical :: DecNum -> IO Bool
isCanonical = testBool c'decNumberIsCanonical

isFinite :: DecNum -> IO Bool
isFinite = testBool c'decNumberIsFinite

isInfinite :: DecNum -> IO Bool
isInfinite = testBool c'decNumberIsInfinite

isNaN :: DecNum -> IO Bool
isNaN = testBool c'decNumberIsNaN

isNegative :: DecNum -> IO Bool
isNegative = testBool c'decNumberIsNegative

isQNaN :: DecNum -> IO Bool
isQNaN = testBool c'decNumberIsQNaN

isSNaN :: DecNum -> IO Bool
isSNaN = testBool c'decNumberIsSNaN

isSpecial :: DecNum -> IO Bool
isSpecial = testBool c'decNumberIsSpecial

isZero :: DecNum -> IO Bool
isZero = testBool c'decNumberIsZero

