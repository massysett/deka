{-# LANGUAGE Trustworthy #-}
module Deka.Internal.Dec.Dec where

import qualified Data.ByteString.Char8 as BS8
import Foreign.Safe
import System.IO.Unsafe (unsafePerformIO)
import Deka.Internal.Decnumber.DecNumber

-- | A decimal value.  A decimal consists of:
--
-- * an integral /coefficient/,
--
-- * an /exponent/, and
--
-- * a /sign/.
--
-- A decimal may also be a /special value/, which can be:
--
-- * /NaN/ (Not a Number), which may be either /quiet/
-- (propagates quietly through operations) or /signaling/ (raises
-- the /Invalid operation/ condition when encountered), or
--
-- * /Infinity/, either positive or negative.
newtype Dec = Dec { unDec :: ForeignPtr C'decNumber }

-- | Uses 'toByteString'.
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

-- | Converts a 'Dec' to a character string.  Uses scientific
-- notation if an exponent is needed.  Implements the
-- /to-scientific-string/ conversion described in the General
-- Decimal Arithmetic specification.  No error is possible.
toByteString :: Dec -> BS8.ByteString
toByteString = fmap unsafePerformIO toByteStringIO

