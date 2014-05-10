{-# LANGUAGE Safe #-}

module Deka.Internal.Dec.Util where

import Deka.Internal.Decnumber.DecNumber
import Deka.Internal.Decnumber.Context
import Foreign.Safe
import Deka.Internal.Dec.Dec

-- | Allocates a Dec of sufficient size to hold the given number
-- of digits.
newDecSize
  :: Int32
  -- ^ New Dec will hold this many digits.
  -> IO Dec
newDecSize i = do
  let sz = mallocAmount i
  fp <- mallocForeignPtrBytes sz
  return $ Dec fp

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

oneDigitDec :: IO Dec
oneDigitDec = fmap Dec $
  mallocForeignPtrBytes c'decNumber'sizeOf

newDec :: Ptr C'decContext -> IO Dec
newDec p = do
  dgts <- peek (p'decContext'digits p)
  let sz = mallocAmount dgts
  fp <- mallocForeignPtrBytes sz
  return $ Dec fp

copyDec :: Dec -> IO Dec
copyDec (Dec p) = withForeignPtr p $ \dp ->
  peek (p'decNumber'digits dp) >>= \dgts ->
  newDecSize dgts >>= \dn' ->
  withForeignPtr (unDec dn') $ \dp' ->
  c'decNumberCopy dp' dp >>
  return dn'

