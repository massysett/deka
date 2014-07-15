-- | Representation of numbers in native Haskell types.
--
-- Since deka is a binding to the mpdecimal C library, the data
-- types are held as pointers to data which are managed by C
-- functions.  Therefore there is no direct access to what is inside
-- of the the 'Deka.Dec' data type.  Modules in "Deka.Native"
-- provide Haskell types mirroring the abstract representations
-- given in the General Decimal Arithmetic Specification.  This is
-- useful if you want to manipulate the data in an abstract way.
-- For example, perhaps you want to perform arithmetic on a value,
-- transform it to abstract form, add digit grouping characters, and
-- then use your own functions to pretty print the result.
--
-- The General Decimal Arithmetic Specification gives an abstract
-- representation of each number.  This information is taken from
-- the General Decimal Arithmetic specification at
--
-- <http://speleotrove.com/decimal/damodel.html>
--
-- A number may be /finite/, in
-- which case it has three components: a /sign/, which must be zero
-- (for zero or positive numbers) or one (for negative zero and
-- negative numbers), an integral /coefficient/, which is always
-- zero or positive, and a signed integral /exponent/, which
-- indicates the power of ten by which the number is multiplied.
-- The value of a finite number if given by
--
-- > (-1) ^ sign * coefficient * 10 ^ exponent
--
-- In addition to finite numbers, a number may also be one of three
-- /special values/:
--
-- * /infinity/ - numbers infinitely large in magnitude
--
-- * /quiet NaN/ - an undefined result which does not cause an
-- 'invalidOperation' condition.
--
-- * /signaling NaN/ - an undefined result which will usually cause
-- an 'invalidOperation' condition.
--
-- When a number has one of these special values, its /coefficient/
-- and /exponent/ are undefined.  An NaN, however, may have
-- additional /diagnostic information/, which is a positive integer.
--
-- All special values have a sign.  The sign of an infinity is
-- significant.  The sign of an NaN has no meaning, though it may be
-- considered as part of the diagnostic information.
--
-- You can transform an abstract form to a 'Dec' losslessly by using
-- 'abstractToByteString'.  This gives you a string in scientific
-- notation, as specified in @to-scientific-string@ in the
-- specification.  There is a one-to-one mapping of abstract
-- representations to @scientific-string@ representations.  You can
-- also transform a 'Dec' to an 'Abstract' losslessly by using
-- 'abstractFromByteString'.  This operation will not fail if it is
-- using output from 'toByteString'; but it might fail otherwise, if
-- the input is malformed.
--
-- All standard typeclass instances in these modules are derived; so
-- while the 'Ord' instance might be useful to use 'Abstract' as the
-- key in a Map, don't expect it to tell you anything about how
-- 'Abstract' are situated on the number line.
module Deka.Native
  ( -- * Digits and groups of digits
    Novem(..)
  , Decem(..)
  , Decuple(..)
  , Aut(..)
  , Firmado(..)

  -- * Elements of abstract numbers
  , Coefficient(..)
  , Exponent(..)
  , Diagnostic(..)
  , Noisy(..)
  , NonNum(..)
  , Value(..)
  , Abstract(..)

  -- * Transformations
  , abstractToString
  , abstractToDec
  , stringToAbstract
  , decToAbstract
  ) where

import Deka.Native.Abstract
import Deka.Native.FromString (stringToAbstract, decToAbstract)
