{-# LANGUAGE EmptyDataDecls, Trustworthy #-}
module Data.Deka.DecNum where

import Prelude hiding (abs, and, or, max, min)
import qualified Prelude as P
import qualified Data.ByteString.Char8 as BS8
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Safe
import Data.Deka.Decnumber.DecNumber
import Data.Deka.Decnumber.Types
import Data.Deka.Decnumber.Context
import Data.Deka.Context.Internal

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

data DPtr

newtype DecNum = DecNum { unDecNum :: ForeignPtr DPtr }

newDecNum :: Ptr C'decContext -> IO DecNum
newDecNum p = do
  digits <- peek (p'decContext'digits p)
  let sz = mallocAmount digits
  fp <- mallocForeignPtrBytes sz
  return $ DecNum fp

newDecNumSize :: C'int32_t -> IO DecNum
newDecNumSize i = do
  let sz = mallocAmount i
  fp <- mallocForeignPtrBytes sz
  return $ DecNum fp

fromInt32 :: C'int32_t -> DecNum
fromInt32 i = unsafePerformIO $ do
  dn <- newDecNumSize 10
  withForeignPtr (unDecNum dn) $ \ptr -> do
    _ <- c'decNumberFromInt32 (castPtr ptr) i
    return dn

fromUInt32 :: C'uint32_t -> DecNum
fromUInt32 i = unsafePerformIO $ do
  dn <- newDecNumSize 10
  withForeignPtr (unDecNum dn) $ \ptr -> do
    _ <- c'decNumberFromUInt32 (castPtr ptr) i
    return dn

fromByteString :: BS8.ByteString -> Ctx DecNum
fromByteString bs = Ctx $ \pCtx ->
  newDecNum pCtx >>= \dn ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  BS8.useAsCString bs $ \cstr ->
  c'decNumberFromString (castPtr pDn) cstr pCtx >>
  return dn

toByteString :: DecNum -> BS8.ByteString
toByteString dn = unsafePerformIO $
  withForeignPtr (unDecNum dn) $ \pDn ->
  peek (p'decNumber'digits (castPtr pDn)) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToString (castPtr pDn) pStr >>
  BS8.packCString pStr

toEngByteString :: DecNum -> BS8.ByteString
toEngByteString dn = unsafePerformIO $
  withForeignPtr (unDecNum dn) $ \pDn ->
  peek (p'decNumber'digits (castPtr pDn)) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToEngString (castPtr pDn) pStr >>
  BS8.packCString pStr

toUInt32 :: DecNum -> Ctx C'uint32_t
toUInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  c'decNumberToUInt32 (castPtr pDn) pCtx

toInt32 :: DecNum -> Ctx C'int32_t
toInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  c'decNumberToInt32 (castPtr pDn) pCtx

-- skipped: getBCD, setBCD

type Unary
  = Ptr C'decNumber
  -- ^ Result
  -> Ptr C'decNumber
  -- ^ Input
  -> Ptr C'decContext
  -> IO (Ptr C'decNumber)

unary :: Unary -> DecNum -> Ctx DecNum
unary f x = Ctx $ \pCtx ->
  newDecNum pCtx >>= \res ->
  withForeignPtr (unDecNum res) $ \pRes ->
  withForeignPtr (unDecNum x) $ \pX ->
  f (castPtr pRes) (castPtr pX) pCtx >>
  return res

type Binary
   = Ptr C'decNumber
   -- ^ Result
   -> Ptr C'decNumber
   -- ^ Input X
   -> Ptr C'decNumber
   -- ^ Input Y
   -> Ptr C'decContext
   -> IO (Ptr C'decNumber)

binary :: Binary -> DecNum -> DecNum -> Ctx DecNum
binary f x y = Ctx $ \pCtx ->
  newDecNum pCtx >>= \res ->
  withForeignPtr (unDecNum res) $ \pRes ->
  withForeignPtr (unDecNum x) $ \pX ->
  withForeignPtr (unDecNum y) $ \pY ->
  f (castPtr pRes) (castPtr pX) (castPtr pY) pCtx >>
  return res

type Ternary
   = Ptr C'decNumber
   -- ^ Result
   -> Ptr C'decNumber
   -- ^ Input X
   -> Ptr C'decNumber
   -- ^ Input Y
   -> Ptr C'decNumber
   -- ^ Input Z
   -> Ptr C'decContext
   -> IO (Ptr C'decNumber)

ternary :: Ternary -> DecNum -> DecNum -> DecNum -> Ctx DecNum
ternary f x y z = Ctx $ \pCtx ->
  newDecNum pCtx >>= \res ->
  withForeignPtr (unDecNum res) $ \pRes ->
  withForeignPtr (unDecNum x) $ \pX ->
  withForeignPtr (unDecNum y) $ \pY ->
  withForeignPtr (unDecNum z) $ \pZ ->
  f (castPtr pRes) (castPtr pX) (castPtr pY) (castPtr pZ) pCtx >>
  return res

abs :: DecNum -> Ctx DecNum
abs = unary c'decNumberAbs

add :: DecNum -> DecNum -> Ctx DecNum
add = binary c'decNumberAdd
