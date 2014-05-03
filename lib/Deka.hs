{-# LANGUAGE Safe, DeriveDataTypeable #-}

-- | Simple decimal arithmetic.
--
-- 'Deka' provides a decimal arithmetic type.  You are limited to 34
-- digits of precision.  That's 34 digits total, not 34 digits after
-- the decimal point.  For example, the numbers @123.0@ and @0.1230@
-- both have four digits of precision.  Deka remembers significant
-- digits, so @123@ has three digits of precision while @123.0@ has
-- four digits of precision.
--
-- Using this module, the results are never inexact.  Computations
-- will throw exceptions rather than returning an inexact result.
-- That way, you know that any result you have is exactly correct.
--
-- 'Deka' represents only finite values.  There are no infinities or
-- not-a-number values allowed.
--
-- For more control over your arithmetic, see "Deka.Fixed.Quad", but
-- for many routine uses this module is sufficient and is more
-- succinct because, unlike 'Quad', 'Deka' is a member of the 'Num'
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
import Data.Maybe
import Data.Typeable
import Deka.Fixed.Quad
import Deka.Internal.Quad.Decoding (Coefficient(..))
import qualified Deka.Fixed.Quad as P
import qualified Data.ByteString.Char8 as BS8

-- | Thrown by arithmetic functions in the Num class, as this is the
-- only way to indicate errors.
data DekaError
  = IntegerTooBig Integer
  -- ^ Could not convert an integer to a Deka; it is too big.
  | Flagged [Flag]
  -- ^ A computation set flags.  This will happen if, for example,
  -- you calculate a result that is out of range, such as
  --
  -- >>> maxBound + maxBound :: Deka
  deriving (Show, Typeable)

instance Exception DekaError

-- | Deka wraps a 'Quad'.  Only finite 'Quad' may become a 'Deka';
-- no infinities or NaN values are allowed.
--
-- 'Deka' is a member of 'Num' and 'Real', making it easy to use for
-- elementary arithmetic.  Any time you perform arithmetic, the
-- results are always exact.  The arithmetic functions will throw
-- exceptions rather than give you an inexact result.
--
-- 'Deka' is not a member 'Fractional' because it is generally
-- impossible to perform division without getting inexact results,
-- and 'Deka' never holds inexact results.
newtype Deka = Deka { unDeka :: Quad }
  deriving Show

eval :: Ctx a -> a
eval c
  | null fl = r
  | otherwise = throw . Flagged $ fl
  where
    (r, fl) = runCtx initQuad $ do
      res <- c
      f <- getStatus
      return (res, f)

-- | Eq compares by value.  For instance, @3.5 == 3.500@.
instance Eq Deka where
  Deka x == Deka y = case compareOrd x y of
    Just EQ -> True
    Just _ -> False
    _ -> error "Deka: Eq: unexpected result"

-- | Ord compares by value.  For instance, @compare 3.5 3.500 ==
-- EQ@.
instance Ord Deka where
  compare (Deka x) (Deka y) = case compareOrd x y of
    Just r -> r
    _ -> error "Deka: compare: unexpected reslt"

-- | Many of the 'Num' functions will throw 'DekaError' if their
-- arguments are out of range or if they produce results that are
-- out of range or inexact.  For functions that don't throw, you can
-- use 'integralToDeka' rather than 'fromInteger', or you can use
-- "Deka.Fixed.Quad" instead of 'Deka'.
instance Num Deka where
  Deka x + Deka y = Deka . eval $ P.add x y
  Deka x - Deka y = Deka . eval $ P.subtract x y
  Deka x * Deka y = Deka . eval $ P.multiply x y
  negate = Deka . eval . P.minus . unDeka
  abs = Deka . eval . P.abs . unDeka
  signum (Deka x)
    | f isZero = fromInteger 0
    | f isNegative = fromInteger (-1)
    | otherwise = fromInteger 1
    where
      f g = g x
  fromInteger i = fromMaybe (throw (IntegerTooBig i))
    . integralToDeka $ i

instance Real Deka where
  toRational (Deka x) = case decodedToRational . toBCD $ x of
    Nothing -> error "Deka.toRational: failed."
    Just r -> r

instance Bounded Deka where
  minBound = Deka $ fromBCD (Decoded Neg (Finite oneCoeff minBound))
    where
      oneCoeff = Coefficient [D1]
  maxBound = Deka $ fromBCD (Decoded NonNeg (Finite maxBound maxBound))


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


-- | Convert any integral to a Deka.  Returns 'Nothing' if the
-- integer is too big to fit into a Deka (34 digits).
integralToDeka :: Integral a => a -> Maybe Deka
integralToDeka i = do
  coe <- P.coefficient . P.integralToDigits $ i
  let d = Decoded sgn (Finite coe zeroExponent)
      sgn = if i < 0 then Neg else NonNeg
  return . Deka $ fromBCD d

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
  | not (null fl) = Nothing
  | not (isFinite r) = Nothing
  | otherwise = Just (Deka r)
  where
    (r, fl) = runCtx initQuad $ do
      res <- fromByteString . BS8.pack $ s
      f <- getStatus
      return (res, f)

-- | Change a Quad to a Deka.  Only succeeds for finite Quad.
quadToDeka :: Quad -> Maybe Deka
quadToDeka q
  | isFinite q = Just $ Deka q
  | otherwise = Nothing
