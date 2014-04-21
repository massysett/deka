{-# LANGUAGE Safe #-}

module Deka.Internal.DecNum.Util where

import Deka.Internal.Decnumber.Types
import Deka.Internal.Decnumber.DecNumber
import Deka.Internal.Decnumber.Context
import Foreign.Safe
import Deka.Internal.DecNum.DecNum

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
    extraUnits = max 0 $ totUnits - baseUnits
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

