-- | Decimal floating point arithmetic.
--
-- Computers typically use binary (that is, base 2) arithmetic.
-- However, binary arithmetic can only approximate many decimal
-- (that is, base 10) numbers.  This means that binary arithmetic is
-- often unsuitable for purposes such as financial calculations.
--
-- For much more on this topic, see
--
-- <http://speleotrove.com/decimal/decifaq.html>
--
-- This module will suffice for simple needs.  Deka is /not/ an
-- instance of many typeclasses, such as 'Fractional'.  That is
-- because there is no straightforward way to implement those
-- operations without rounding and a destruction of precision.  If
-- you need division with correct rounding, ideal would be a Haskell
-- library that implements the specification for decimal arithmetic
-- available at
--
-- <http://speleotrove.com/decimal/>
--
-- However, I am not aware of any Haskell library that implements
-- this specification.
--
-- Another way to perform division is to use 'toRational' on your
-- operands.  You can then divide those operands, or convert them to
-- floats first and divide those.
--
-- Another library implementing decimal arithmetic is Decimal,
-- available on Hackage.  That library differs from this one in a
-- few ways; for example, it allows only for negative exponents, and
-- the exponent is always a Word8.  It also allows the significand
-- to be any Integral.
--
-- Tests are available with the deka package; I encourage you to run
-- them by unpacking the tarball and running
--
-- > cabal test
--
-- The tests also provide documentation of the properties.

module Data.Deka
  ( Deka(..)
  , equalizeExponents
  , compareValues
  , equivalent
  , (==~)
  , dekaToFloat
  , floatToDeka
  ) where

import Control.Arrow
import Data.Ratio

-- | A single decimal number.  Each Deka consists of a Integer
-- coefficient and an Integer exponent.  For a coefficient @c@ and
-- an exponent @e@, the value of any given Deka is @c * 10 ^ e@.
--
-- For the Num instance, all operations preserve full precision by
-- increasing the number of significant digits as needed.
--
-- /WARNING/ - before performing any tests for equality or ordering
--
-- The Eq and Ord instances are derived.  Therefore, for two Deka
-- @x@ and @y@, @x == y@ only if they have the same coefficient and
-- the same exponent.  You may instead want 'equivalent'.
-- Similarly, for two Deka, 'comparing' will indicate they are
-- different as long as they have different coefficients or
-- exponents, even if they indicate the same value.  You may instead
-- want 'compareValues'.
data Deka = Deka
  { coef :: Integer
  , expt :: Integer
  } deriving (Eq, Show, Ord)

-- | Equalize the exponents on two Deka.  Each Deka in the result is
-- equivalent to the corresponding input.  The exponent of one Deka
-- may be lower than its corresponding input.
equalizeExponents :: Deka -> Deka -> (Deka, Deka)
equalizeExponents (Deka s1 e1) (Deka s2 e2) =
  (Deka s1' e', Deka s2' e')
  where
    e' = min e1 e2
    s1' = s1 * 10 ^ (abs $ e1 - e')
    s2' = s2 * 10 ^ (abs $ e2 - e')

-- | Compares two Deka after equalizing their exponents.
compareValues :: Deka -> Deka -> Ordering
compareValues x y
  = uncurry compare 
  . first coef
  . second coef
  $ equalizeExponents x y

-- | Test for equality of Deka after equalizing their exponents.
equivalent :: Deka -> Deka -> Bool
equivalent x y
  = uncurry (==)
  . first coef
  . second coef
  $ equalizeExponents x y

-- | Same as 'equivalent'.  Has same fixity and precedence as '==',
-- which is infix 4.
(==~) :: Deka -> Deka -> Bool
(==~) = equivalent
infix 4 ==~

instance Num Deka where
  x + y = Deka (cx + cy) e
    where
      (Deka cx e, Deka cy _) = equalizeExponents x y

  x - y = Deka (cx - cy) e
    where
      (Deka cx e, Deka cy _) = equalizeExponents x y

  x * y = Deka (cx * cy) (ex + ey)
    where
      (Deka cx ex, Deka cy ey) = (x, y)

  negate d = d { coef = negate (coef d) }
  abs d = d { coef = abs (coef d) }
  signum d = Deka (signum . coef $ d) 0
  fromInteger i = Deka i 0

instance Real Deka where
  toRational (Deka c e)
    | e < 0 = c % (10 ^ abs e)
    | otherwise = (c * 10 ^ e) % 1

-- | Converts a Deka to a floating point type.  Fails if the
-- exponent of the Deka is out of the range of Int.  Uses
-- 'encodeFloat' so note the caveats stated there.
dekaToFloat :: RealFloat a => Deka -> Maybe a
dekaToFloat (Deka c e)
  | e' < (minBound :: Int) || e' > (maxBound :: Int) = Nothing
  | otherwise = Just $ encodeFloat c e'
  where
    e' = fromIntegral e

-- | Converts a floating-point type to a Deka.  Fails if the
-- floating-point type is NaN or infinite.
floatToDeka :: RealFloat a => a -> Maybe Deka
floatToDeka a
  | isNaN a = Nothing
  | isInfinite a = Nothing
  | otherwise = let (m, e) = decodeFloat a
                in Just $ Deka m (fromIntegral e)

