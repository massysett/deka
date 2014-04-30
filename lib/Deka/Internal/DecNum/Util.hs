{-# LANGUAGE Safe #-}

module Deka.Internal.DecNum.Util where

import Deka.Internal.Decnumber.Types
import Deka.Internal.Decnumber.DecNumber
import Deka.Internal.Decnumber.Context
import Foreign.Safe
import Deka.Internal.DecNum.DecNum

-- | Allocates a DecNum of sufficient size to hold the given number
-- of digits.
newDecNumSize
  :: Int32
  -- ^ New DecNum will hold this many digits.
  -> IO DecNum
newDecNumSize i = do
  let sz = mallocAmount i
  fp <- mallocForeignPtrBytes sz
  return $ DecNum fp

-- | How many bytes must be malloc'ed to hold this many
-- digits?
mallocAmount
  :: Int32
  -- ^ Number of digits
  -> Int
  -- ^ Malloc this many bytes total
mallocAmount s = base + extra
  where
    base = c'decNumber'sizeOf
    baseUnits = c'DECNUMUNITS
    totUnits = (s + c'DECDPUN - 1) `quot` c'DECDPUN
    extraUnits = max 0 $ totUnits - baseUnits
    extra = sizeOf (undefined :: C'decNumberUnit) * fromIntegral extraUnits

oneDigitDecNum :: IO DecNum
oneDigitDecNum = do
  fp <- mallocForeignPtrBytes c'decNumber'sizeOf
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

