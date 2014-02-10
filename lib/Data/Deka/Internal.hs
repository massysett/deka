-- | Internal types - for Deka use only
--
-- This module is not listed for export in the cabal file.  It
-- contains types that library users have no access to, but which
-- are needed by multiple Deka modules or that the test suite needs
-- access to.
module Data.Deka.Internal where

import Foreign.Safe
import Foreign.C
import qualified Data.ByteString.Char8 as BS8
import Data.Deka.Decnumber
import Control.Applicative
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

-- # Helpers

type Boolean
  = Ptr C'decQuad
  -> IO C'uint32_t

boolean
  :: Boolean
  -> Quad
  -> Bool
boolean f d = unsafePerformIO $
  withForeignPtr (unQuad d) $ \pD ->
  f pD >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "boolean: bad return value"

-- | Creates a new Quad.  Uninitialized, so don't export this
-- function.
newQuad :: IO Quad
newQuad = fmap Quad mallocForeignPtr

type BinaryCtxFree
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

binaryCtxFree
  :: BinaryCtxFree
  -> Quad
  -> Quad
  -> Quad
binaryCtxFree f x y = unsafePerformIO $
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  f pR pX pY >>
  return r

-- | The Ctx monad
--
-- The General Decimal Arithmetic specification states that most
-- computations occur within a @context@, which affects the manner
-- in which computations are done (for instance, the context
-- determines the rounding algorithm).  The context also carries
-- the flags that computations can set (for instance, a computation might
-- set a flag to indicate that the result is rounded or inexact or
-- was a division by zero.) The Ctx monad carries this context.
newtype Ctx a = Ctx { unCtx :: Ptr C'decContext -> IO a }

instance Functor Ctx where
  fmap = liftM

instance Applicative Ctx where
  pure = return
  (<*>) = ap

instance Monad Ctx where
  return a = Ctx $ \_ -> return a
  Ctx a >>= f = Ctx $ \p -> do
    r1 <- a p
    let b = unCtx $ f r1
    b p
  fail s = Ctx $ \_ -> fail s

-- | Decimal number.  As indicated in the General Decimal
-- Arithmetic specification, a 'Quad' might be a finite number
-- (perhaps the most common type) or it might be infinite or a
-- not-a-number.  'decClass' will tell you a little more about a
-- particular 'Quad'.
newtype Quad = Quad { unQuad :: ForeignPtr C'decQuad }

-- | The Eq instance depends on an IEEE 754 total ordering.  In
-- particular, note that, for example, @7.5@ is not equal to @7.50@.
-- See
--
-- <http://speleotrove.com/decimal/decifaq4.html#order>

instance Eq Quad where
  x == y = case compareTotal x y of
    EQ -> True
    _ -> False

-- | Like the 'Eq' instance, this uses an IEEE 754 total ordering.
instance Ord Quad where
  compare = compareTotal

-- | The Show instance uses 'toByteString'.
instance Show Quad where
  show = BS8.unpack . toByteString

-- | Converts a 'Quad' to a string.  May use non-scientific
-- notation, but only if that's unambiguous; otherwise, uses
-- scientific notation.
--
-- In the decNumber C library, this is called @toString@; the name
-- was changed here because this function doesn't return a Haskell
-- 'String'.
toByteString :: Quad -> BS8.ByteString
toByteString = mkString unsafe'c'decQuadToString

type MkString
  = Ptr C'decQuad
  -> CString
  -> IO CString

mkString
  :: MkString
  -> Quad
  -> BS8.ByteString
mkString f d = unsafePerformIO $
  withForeignPtr (unQuad d) $ \pD ->
  allocaBytes c'DECQUAD_String $ \pS ->
  f pD pS
  >> BS8.packCString pS

-- | Compares using an IEEE 754 total ordering, which takes into
-- account the exponent.  IEEE 754 says that this function might
-- return different results depending upon whether the operands are
-- canonical; 'Quad' are always canonical so you don't need to worry
-- about that here.
compareTotal :: Quad -> Quad -> Ordering
compareTotal x y
  | isNegative c = LT
  | isZero c = EQ
  | isPositive c = GT
  | otherwise = error "compareTotal: unknown result"
  where
    c = binaryCtxFree unsafe'c'decQuadCompareTotal x y

-- # Tests

-- | True only if @x@ is less than zero and is not an NaN.
isNegative :: Quad -> Bool
isNegative = boolean unsafe'c'decQuadIsNegative

-- | True only if @x@ is a zero.
isZero :: Quad -> Bool
isZero = boolean unsafe'c'decQuadIsZero

-- | True only if @x@ is greater than zero and is not an NaN.
isPositive :: Quad -> Bool
isPositive = boolean unsafe'c'decQuadIsPositive

