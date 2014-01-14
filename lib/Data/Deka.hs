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
-- There is a specification for decimal arithmetic available at
--
-- <http://speleotrove.com/decimal/>
--
-- However, this library makes no attempt to implement this
-- specification.  I'm not aware of any Haskell library which
-- implements this standard.  This module provides a few operations
-- which should suffice for many common needs.
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
  ) where

import Control.Arrow

-- | A single decimal number.  Each Deka consists of a Integer
-- coefficient and an Integer exponent.  For a coefficient @c@ and
-- an exponent @e@, the value of any given Deka is @c * 10 ^ e@.
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

-- | Same as 'equivalent'
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
