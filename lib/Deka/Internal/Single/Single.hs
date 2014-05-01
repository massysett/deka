{-# LANGUAGE Trustworthy #-}
-- | Internal types - for Single use only
--
-- This module is not listed for export in the cabal file.  It
-- contains types that library users have no access to, but which
-- are needed by multiple Deka modules or that the test suite needs
-- access to.
module Deka.Internal.Single.Single where

import Foreign.Safe
import Foreign.C
import qualified Data.ByteString.Char8 as BS8
import Deka.Internal.Decnumber.DecSingle
import Deka.Internal.Unsafe
import Prelude

-- # Helpers

type Boolean
  = Ptr C'decSingle
  -> IO Word32

boolean
  :: Boolean
  -> Single
  -> IO Bool
boolean f d =
  withForeignPtr (unSingle d) $ \pD ->
  f pD >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "boolean: bad return value"

-- | Creates a new Single.  Uninitialized, so don't export this
-- function.
newSingle :: IO Single
newSingle = fmap Single (mallocForeignPtrBytes c'decSingle'sizeOf)

type BinaryCtxFree
  = Ptr C'decSingle
  -> Ptr C'decSingle
  -> Ptr C'decSingle
  -> IO (Ptr C'decSingle)

binaryCtxFree
  :: BinaryCtxFree
  -> Single
  -> Single
  -> IO Single
binaryCtxFree f x y =
  newSingle >>= \r ->
  withForeignPtr (unSingle r) $ \pR ->
  withForeignPtr (unSingle x) $ \pX ->
  withForeignPtr (unSingle y) $ \pY ->
  f pR pX pY >>
  return r

-- | Decimal number.  As indicated in the General Decimal
-- Arithmetic specification, a 'Single' might be a finite number
-- (perhaps the most common type) or it might be infinite or a
-- not-a-number.  'decClass' will tell you a little more about a
-- particular 'Single'.
newtype Single = Single { unSingle :: ForeignPtr C'decSingle }

-- | The Show instance uses 'toByteString'.
instance Show Single where
  show = BS8.unpack . toByteString

-- | Converts a 'Single' to a string.  May use non-scientific
-- notation, but only if that's unambiguous; otherwise, uses
-- scientific notation.
--
-- In the decNumber C library, this is called @toString@; the name
-- was changed here because this function doesn't return a Haskell
-- 'String'.
toByteStringIO :: Single -> IO BS8.ByteString
toByteStringIO = mkString c'decSingleToString

toByteString :: Single -> BS8.ByteString
toByteString = unsafe1 toByteStringIO

type MkString
  = Ptr C'decSingle
  -> CString
  -> IO CString

mkString
  :: MkString
  -> Single
  -> IO BS8.ByteString
mkString f d =
  withForeignPtr (unSingle d) $ \pD ->
  allocaBytes c'DECSINGLE_String $ \pS ->
  f pD pS
  >> BS8.packCString pS

