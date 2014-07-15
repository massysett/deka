{-# LANGUAGE Safe, DeriveDataTypeable #-}

-- | Simple decimal arithmetic.
--
-- 'Deka' provides a decimal arithmetic type. Using this module, the
-- results are never inexact.  Computations will throw exceptions
-- rather than returning an inexact result.  That way, you know that
-- any result you have is exactly correct.
--
-- On 64-bit platforms, you are limited to:
--
-- * a coefficient of ((2 * 10 ^ 17) - 1) digits long
--
-- * a maximum exponent of ((1 * 10 ^ 18) - 1)
--
-- * a minimum exponent of -((1 * 10 ^ 18) + 1)
--
-- On 32-bit platforms, you are limited to:
--
-- * a coefficient of 8.5 * 10 ^ 8 digits long
--
-- * a maximum exponent of 4.25 * 10 ^ 8
--
-- * a minimum exponent of -4.25 * 10 ^ 8
--
-- If you exceed these limits, your computation will throw an
-- exception.
--
-- 'Deka' represents only finite values.  There are no infinities or
-- not-a-number values allowed.
--
-- For more control over your arithmetic, see "Deka.Dec", but
-- for many routine uses this module is sufficient and is more
-- succinct because, unlike 'Dec', 'Deka' is a member of the 'Num'
-- typeclass.

module Deka
  ( Deka
  , unDeka
  , DekaT(..)
  , integralToDeka
  , strToDeka
  , quadToDeka
  , DekaError(..)
  ) where

import Control.Exception
import Data.Typeable
import Deka.Dec hiding (compare)
import qualified Deka.Dec as D
import qualified Data.ByteString.Char8 as BS8

-- | Thrown by arithmetic functions in the Num class, as this is the
-- only way to indicate errors.
data DekaError
  = Flagged Flags
  -- ^ A computation set flags.  This will happen if, for example,
  -- you calculate a result that is out of range.
  deriving (Show, Typeable)

instance Exception DekaError

-- | Deka wraps a 'Dec'.  Only finite 'Dec' may become a 'Deka';
-- no infinities or NaN values are allowed.
--
-- 'Deka' is a member of 'Num', making it easy to use for
-- elementary arithmetic.  Any time you perform arithmetic, the
-- results are always exact.  The arithmetic functions will throw
-- exceptions rather than give you an inexact result.
--
-- 'Deka' is not a member 'Fractional' because it is generally
-- impossible to perform division without getting inexact results,
-- and 'Deka' never holds inexact results.
newtype Deka = Deka { unDeka :: Dec }
  deriving Show

eval :: Ctx a -> a
eval c
  | fl == emptyFlags = r
  | otherwise = throw . Flagged $ fl
  where
    (r, fl) = runCtxStatus c

-- | Eq compares by value.  For instance, @3.5 == 3.500@.
instance Eq Deka where
  Deka x == Deka y = case eval k of
    EQ -> True
    _ -> False
    where
      k = do
        d <- D.compare x y
        let f | isZero d = EQ
              | isPositive d = GT
              | otherwise = LT
        return f

-- | Ord compares by value.  For instance, @compare 3.5 3.500 ==
-- EQ@.
instance Ord Deka where
  compare (Deka x) (Deka y) = eval $ do
    d <- D.compare x y
    let f | isZero d = EQ
          | isPositive d = GT
          | otherwise = LT
    return f

-- | Many of the 'Num' functions will throw 'DekaError' if their
-- arguments are out of range or if they produce results that are
-- out of range or inexact.  For functions that don't throw, you can
-- use 'integralToDeka' rather than 'fromInteger', or you can use
-- "Deka.Dec" instead of 'Deka'.
instance Num Deka where
  Deka x + Deka y = Deka . eval $ D.add x y
  Deka x - Deka y = Deka . eval $ D.subtract x y
  Deka x * Deka y = Deka . eval $ D.multiply x y
  negate = Deka . eval . D.minus . unDeka
  abs = Deka . eval . D.abs . unDeka
  signum (Deka x)
    | f isZero = fromInteger 0
    | f isNegative = fromInteger (-1)
    | otherwise = fromInteger 1
    where
      f g = g x
  fromInteger = Deka . eval . fromByteString . BS8.pack . show

-- | Decimals with a total ordering.
newtype DekaT = DekaT { unDekaT :: Deka }
  deriving Show

-- | Eq compares by a total ordering.
instance Eq DekaT where
  DekaT (Deka x) == DekaT (Deka y)
    | r == EQ = True
    | otherwise = False
    where
      r = compareTotal x y

-- | Ord compares by a total ordering.
instance Ord DekaT where
  compare (DekaT (Deka x)) (DekaT (Deka y)) = compareTotal x y


-- | Convert any integral to a 'Deka'.  Returns 'Nothing' if the
-- integer is too big to fit into a 'Deka'.
integralToDeka :: (Integral a, Show a) => a -> Maybe Deka
integralToDeka i
  | fl == emptyFlags = Just . Deka $ d
  | otherwise = Nothing
  where
    (d, fl) = runCtxStatus . fromByteString . BS8.pack . show $ i

-- | Convert a string to a Deka.  You can use ordinary numeric
-- strings, such as @3.25@, or exponential notation, like @325E-2@.
-- More information on your choices is at:
--
-- <http://speleotrove.com/decimal/daconvs.html#reftonum>
--
-- You cannot use strings that represent an NaN or an infinity.  If
-- you do that, or use an otherwise invalid string, this function
-- returns 'Nothing'.
strToDeka :: String -> Maybe Deka
strToDeka s
  | not (emptyFlags == fl) = Nothing
  | not (isFinite r) = Nothing
  | otherwise = Just (Deka r)
  where
    (r, fl) = runCtxStatus . fromByteString . BS8.pack $ s

-- | Change a 'Dec' to a 'Deka'.  Only succeeds for finite 'Dec'.
quadToDeka :: Dec -> Maybe Deka
quadToDeka d
  | isFinite d = Just $ Deka d
  | otherwise = Nothing
