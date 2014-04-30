{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies #-}
module Deka.Internal.Fixed.Decoding where

import Control.Monad
import Data.Maybe
import Data.Ratio
import Foreign.Safe hiding
  ( void
  , isSigned
  , rotate
  , shift
  , xor
  )
import Prelude hiding
  ( abs
  , and
  , compare
  , isInfinite
  , isNaN
  , max
  , min
  , or
  , subtract
  , significand
  , exponent
  )
import qualified Prelude
import Deka.Decoded

import Deka.Internal.Decnumber.DecQuad

class Decodable a b | a -> b where
  coefficientLength :: a -> Int
  payloadLength :: a -> Int
  payloadLength a = coefficientLength a - 1
  minAdjExp :: a -> Int
  maxAdjExp :: a -> Int
  create :: a -> IO a
  unwrap :: a -> a -> ForeignPtr b
  fromBCD :: a -> Ptr b -> Int32 -> Ptr Word8 -> Int32 -> IO (Ptr b)
  toBCD :: a -> Ptr b -> Ptr Int32 -> Ptr Word8 -> IO Int32


-- ## Decoding and encoding helpers

-- | A list of digits, less than or equal to 'coefficientLen' long.
-- Corresponds only to finite numbers.
newtype Coefficient a = Coefficient { unCoefficient :: [Digit] }
  deriving (Eq, Ord, Show)

-- | Coefficient of 'D0'
zeroCoefficient :: Coefficient a
zeroCoefficient = Coefficient [D0]

-- | Coefficient of 'D1'
oneCoefficient :: Coefficient a
oneCoefficient = Coefficient [D1]


-- | Creates a 'Coefficient'.  Checks to ensure it is not null and
-- that it is not longer than 'coefficientLen' and that it does not
-- have leading zeroes (if it is 0, a single 'D0' is allowed).
coefficient :: Decodable a b => a -> [Digit] -> Maybe (Coefficient a)
coefficient a ls
  | null ls = Nothing
  | length ls > 1 && head ls == D0 = Nothing
  | length ls > coefficientLength a = Nothing
  | otherwise = Just . Coefficient $ ls


-- | A list of digits, less than or equal to 'payloadLen'
-- long.  Accompanies an NaN, potentially with diagnostic
-- information (I do not know if decNumber actually makes use of
-- this.)
newtype Payload a = Payload { unPayload :: [Digit] }
  deriving (Eq, Ord, Show)

-- | Creates a 'Payload'.  Checks to ensure it is not null, not
-- longer than 'payloadLen' and that it does not have leading zeroes
-- (if it is 0, a single 'D0' is allowed).
payload :: Decodable a b => a -> [Digit] -> Maybe (Payload a)
payload a ds
  | null ds = Nothing
  | length ds > 1 && head ds == D0 = Nothing
  | length ds > payloadLength a = Nothing
  | otherwise = Just . Payload $ ds

-- | Payload of [D0]
zeroPayload :: Payload a
zeroPayload = Payload [D0]

-- Decimal Arithmetic Specification version 1.70, page 10, says that
-- the minimum and maximum adjusted exponent is given by
--
-- @-x - (c - 1) + 1@ and @x - (c - 1)@
--
-- where @x@ the upper limit on the absolute value of exponent, and
-- @c@ is the length of the coefficient in decimal digits.
--
-- However, the lower bound of the above formula only accounts for
-- normal numbers.  When subnormal numbers are enabled (as they are
-- here), the lower bound on exponents is
--
-- @m - (p - 1)@
--
-- where @m@ is the smallest possible adjusted exponent for normal
-- numbers (called Emin), and p is the working precision.
--
-- Also, the upper bound is different too, becuase decQuad is
-- clamped; see decNumber manual, page 23.  This means the maximum
-- exponent is limited to
--
-- @t - (p - 1)@
--
-- where @t@ is the maximum possible adjusted exponent and p is the
-- working precision.
--
-- The function below uses the minimum and maximum accounting for
-- the clamp and the subnormals.

-- | The minimum and maximum possible exponent.
minMaxExp :: Decodable a b => a -> (Int, Int)
minMaxExp a = (l, h)
  where
    l = minAdjExp a - coefficientLength a + 1
    h = maxAdjExp a - coefficientLength a + 1

-- | The signed integer which indicates the power of ten by which
-- the coefficient is multiplied.
newtype Exponent a = Exponent { unExponent :: Int }
  deriving (Eq, Ord, Show)

-- | Ensures that the exponent is within the range allowed by
-- 'minMaxExp'.
exponent :: Decodable a b => a -> Int -> Maybe (Exponent a)
exponent a i
  | i < l = Nothing
  | i > h = Nothing
  | otherwise = Just . Exponent $ i
  where
    (l, h) = minMaxExp a

-- | An adjusted exponent is the value of an exponent of a number
-- when that number is expressed as though in scientific notation
-- with one digit before any decimal point.  This is the finite
-- exponent + (number of significant digits - 1).
newtype AdjustedExp a = AdjustedExp { unAdjustedExp :: Int }
  deriving (Eq, Show, Ord)

-- | The number of significant digits. Zero returns 1.
dDigits :: Coefficient a -> Int
dDigits (Coefficient ds) = case dropWhile (== D0) ds of
  [] -> 1
  rs -> length rs

adjustedExp :: Coefficient a -> Exponent b -> AdjustedExp c
adjustedExp ds e = AdjustedExp $ unExponent e
  + dDigits ds - 1

adjustedToExponent :: Coefficient a -> AdjustedExp b -> Exponent c
adjustedToExponent ds e = Exponent $ unAdjustedExp e -
  dDigits ds + 1

-- | The smallest possible adjusted exponent that is still normal.
-- Adjusted exponents smaller than this are subnormal.
minNormalAdj :: Decodable a b => a -> AdjustedExp a
minNormalAdj a = AdjustedExp (minAdjExp a)

-- | Like 'minNormalAdj', but returns the size of the regular exponent
-- rather than the adjusted exponent.
minNormalExp :: Decodable a b => a -> Coefficient a -> Exponent a
minNormalExp a c = adjustedToExponent c $ minNormalAdj a

-- | An Exponent whose value is 0.
zeroExponent :: Exponent a
zeroExponent = Exponent 0

data Value a
  = Finite (Coefficient a) (Exponent a)
  | Infinite
  | NaN NaN (Payload a)
  deriving (Eq, Ord, Show)

-- | A pure Haskell type which holds information identical to that
-- in a 'Quad'.
data Decoded a = Decoded
  { dSign :: Sign
  , dValue :: Value a
  } deriving (Eq, Ord, Show)

toDecNumberBCD :: Decodable a b => a -> Decoded a -> (Int32, [Word8], Int32)
toDecNumberBCD a (Decoded s v) = (e, ds, sgn)
  where
    sgn = case s of { NonNeg -> 0; Neg -> c'DECFLOAT_Sign }
    (e, ds) = case v of
      Infinite -> (c'DECFLOAT_Inf, replicate (coefficientLength a) 0)
      NaN n (Payload ps) -> (ns, np)
        where
          ns = case n of
            Quiet -> c'DECFLOAT_qNaN
            Signaling -> c'DECFLOAT_sNaN
          np = pad ++ map digitToInt ps
          pad = replicate (coefficientLength a - length ps) 0
      Finite (Coefficient digs) (Exponent ex) ->
        ( fromIntegral ex, pad ++ map digitToInt digs )
        where
          pad = replicate (coefficientLength a - length digs) 0

encode
  :: Decodable a b
  => a
  -> Decoded a
  -> IO a
encode t dcd =
  create t >>= \d ->
  withForeignPtr (unwrap t d) $ \pD ->
  let (expn, digs, sgn) = toDecNumberBCD t dcd in
  withArray digs $ \pArr ->
  fromBCD t pD expn pArr sgn >>= \_ ->
  return d

-- | A new value with coefficient 'D1', exponent 0, and sign 'NonNeg'.
one :: Decodable a b => a -> IO a
one a = encode a
  $ Decoded NonNeg (Finite (Coefficient [D1]) (Exponent 0))

decode :: Decodable a b => a -> IO (Decoded a)
decode a =
  withForeignPtr (unwrap a a) $ \pD ->
  allocaBytes (coefficientLength a) $ \pArr ->
  alloca $ \pExp ->
  toBCD a pD pExp pArr >>= \sgn ->
  peek pExp >>= \ex ->
  peekArray (coefficientLength a) pArr >>= \coef ->
  return (getDecoded sgn ex coef)

getDecoded
  :: Int32
  -- ^ Sign. Zero if sign is zero; non-zero if sign is not zero
  -- (that is, is negavite.)
  -> Int32
  -- ^ Exponent
  -> [Word8]
  -- ^ Coefficient
  -> Decoded a
getDecoded sgn ex coef = Decoded s v
  where
    s = if sgn == 0 then NonNeg else Neg
    v | ex == c'DECFLOAT_qNaN = NaN Quiet pld
      | ex == c'DECFLOAT_sNaN = NaN Signaling pld
      | ex == c'DECFLOAT_Inf = Infinite
      | otherwise = Finite coe (Exponent $ fromIntegral ex)
      where
        pld = Payload . toDigs . tail $ coef
        coe = Coefficient . toDigs $ coef
        toDigs c = case dropWhile (== D0) . map intToDigit $ c of
          [] -> [D0]
          xs -> xs


-- # Conversions

-- ## Decoded to scientific and ordinary notation

-- | Converts a Decoded to scientific notation.  Unlike
-- 'toByteString' this will always use scientific notation.  For
-- NaNs and infinities, the notation is identical to that of
-- decNumber  (see Decimal Arithmetic Specification page 19).  This
-- means that a quiet NaN is @NaN@ while a signaling NaN is @sNaN@,
-- and infinity is @Infinity@.
--
-- Like decQuadToString, the payload of an NaN is not shown if it is
-- zero.

scientific :: Decoded a -> String
scientific d = sign ++ rest
  where
    sign = case dSign d of
      NonNeg -> ""
      Neg -> "-"
    rest = case dValue d of
      Infinite -> "Infinity"
      Finite c e -> sciFinite c e
      NaN n p -> sciNaN n p

sciFinite :: Coefficient a -> Exponent b -> String
sciFinite c e = sCoe ++ 'E':sExp
  where
    sCoe = case unCoefficient c of
      x:xs -> digitToChar x : case xs of
        [] -> []
        _ -> '.' : map digitToChar xs
      [] -> error "sciFinite: empty coefficient"
    sExp = show . unAdjustedExp . adjustedExp c $ e

sciNaN :: NaN -> Payload a -> String
sciNaN n p = nStr ++ pStr
  where
    nStr = case n of { Quiet -> "NaN"; Signaling -> "sNaN" }
    pStr = case unPayload p of
      [D0] -> ""
      xs -> map digitToChar xs

-- | Converts Decoded a to ordinary decimal notation.  For NaNs and
-- infinities, the notation is identical to that of 'scientific'.
-- Unlike 'scientific', though the result can always be converted back
-- to a 'Quad' using 'fromByteString', the number of significant
-- digits might change.  For example, though @1.2E3@ has two
-- significant digits, using @ordinary@ on this value and then
-- reading it back in with @fromByteString@ will give you @1200E0@,
-- which has four significant digits.

ordinary :: Decoded a -> String
ordinary d = sign ++ rest
  where
    sign = case dSign d of
      NonNeg -> ""
      Neg -> "-"
    rest = case dValue d of
      Infinite -> "Infinity"
      Finite c e -> onyFinite c e
      NaN n p -> sciNaN n p

onyFinite :: Coefficient a -> Exponent b -> String
onyFinite c e
  | coe == [D0] = "0"
  | ex >= 0 = map digitToChar coe ++ replicate ex '0'
  | aex < lCoe =
      let (lft, rt) = splitAt (lCoe - aex) coe
      in map digitToChar lft ++ "." ++ map digitToChar rt
  | otherwise =
      let numZeroes = aex - lCoe
      in "0." ++ replicate numZeroes '0' ++ map digitToChar coe
  where
    ex = unExponent e
    coe = unCoefficient c
    aex = Prelude.abs ex
    lCoe = length coe

-- | Converts a Decoded a to a Rational.  Returns Nothing if the
-- Decoded a is not finite.
decodedToRational :: Decoded a -> Maybe Rational
decodedToRational d = case dValue d of
  (Finite c e) ->
    let int = digitsToInteger . unCoefficient $ c
        ex = unExponent e
        mkSgn = if dSign d == NonNeg then id else negate
        mult = if ex < 0 then 1 % (10 ^ Prelude.abs ex) else 10 ^ ex
    in Just . mkSgn $ fromIntegral int * mult
  _ -> Nothing

-- # Decoded a predicates

dIsFinite :: Decoded a -> Bool
dIsFinite (Decoded _ v) = case v of
  Finite _ _ -> True
  _ -> False

dIsInfinite :: Decoded a -> Bool
dIsInfinite (Decoded _ v) = case v of
  Infinite -> True
  _ -> False

dIsInteger :: Decoded a -> Bool
dIsInteger (Decoded _ v) = case v of
  Finite _ e -> unExponent e == 0
  _ -> False

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
-- The sign must be NonNeg (that is, you cannot have a negative
-- zero.)
dIsLogical :: Decoded a -> Bool
dIsLogical (Decoded s v) = fromMaybe False $ do
  guard $ s == NonNeg
  (d, e) <- case v of
    Finite ds ex -> return (ds, ex)
    _ -> Nothing
  guard $ e == zeroExponent
  return
    . all (\x -> x == D0 || x == D1)
    . unCoefficient $ d

dIsNaN :: Decoded a -> Bool
dIsNaN (Decoded _ v) = case v of
  NaN _ _ -> True
  _ -> False

-- | True only if @x@ is less than zero and is not an NaN.  It's not
-- enough for the sign to be Neg; the coefficient (if finite) must
-- be greater than zero.
dIsNegative :: Decoded a -> Bool
dIsNegative (Decoded s v) = fromMaybe False $ do
  guard $ s == Neg
  return $ case v of
    Finite d _ -> any (/= D0) . unCoefficient $ d
    Infinite -> True
    _ -> False

dIsNormal :: Decodable a b => a -> Decoded a -> Bool
dIsNormal a (Decoded _ v) = case v of
  Finite d e
    | adjustedExp d e < minNormalAdj a -> False
    | otherwise -> any (/= D0) . unCoefficient $ d
  _ -> False

dIsPositive :: Decoded a -> Bool
dIsPositive (Decoded s v)
  | s == Neg = False
  | otherwise = case v of
      Finite d _ -> any (/= D0) . unCoefficient $ d
      Infinite -> True
      _ -> False

dIsSignaling :: Decoded a -> Bool
dIsSignaling (Decoded _ v) = case v of
  NaN Signaling _ -> True
  _ -> False


dIsSigned :: Decoded a -> Bool
dIsSigned (Decoded s _) = s == Neg

dIsSubnormal :: Decodable a b => a -> Decoded a -> Bool
dIsSubnormal a (Decoded _ v) = case v of
  Finite d e -> adjustedExp d e < minNormalAdj a
  _ -> False

-- | True for any zero (negative or positive zero).
dIsZero :: Decoded a -> Bool
dIsZero (Decoded _ v) = case v of
  Finite d _ -> all (== D0) . unCoefficient $ d
  _ -> False

-- # DecClass-like Decoded a predicates

dIsSNaN :: Decoded a -> Bool
dIsSNaN d = case dValue d of
  NaN n _ -> n == Signaling
  _ -> False

dIsQNaN :: Decoded a -> Bool
dIsQNaN d = case dValue d of
  NaN n _ -> n == Quiet
  _ -> False

dIsNegInf :: Decoded a -> Bool
dIsNegInf d
  | dSign d == NonNeg = False
  | otherwise = dValue d == Infinite

dIsNegNormal :: Decodable a b => a -> Decoded a -> Bool
dIsNegNormal a d
  | dSign d == NonNeg = False
  | otherwise = case dValue d of
      Finite c e -> e >= minNormalExp a c
      _ -> False

dIsNegSubnormal :: Decodable a b => a -> Decoded a -> Bool
dIsNegSubnormal a d
  | dSign d == NonNeg = False
  | otherwise = case dValue d of
      Finite c e -> e < minNormalExp a c
      _ -> False

dIsNegZero :: Decoded a -> Bool
dIsNegZero d
  | dSign d == NonNeg = False
  | otherwise = case dValue d of
      Finite c _ -> unCoefficient c == [D0]
      _ -> False

dIsPosZero :: Decoded a -> Bool
dIsPosZero d
  | dSign d == Neg = False
  | otherwise = case dValue d of
      Finite c _ -> unCoefficient c == [D0]
      _ -> False

dIsPosSubnormal :: Decodable a b => a -> Decoded a -> Bool
dIsPosSubnormal a d
  | dSign d == Neg = False
  | otherwise = case dValue d of
      Finite c e -> e < minNormalExp a c
      _ -> False

dIsPosNormal :: Decodable a b => a -> Decoded a -> Bool
dIsPosNormal a d
  | dSign d == Neg = False
  | otherwise = case dValue d of
      Finite c e -> e >= minNormalExp a c
      _ -> False

dIsPosInf :: Decoded a -> Bool
dIsPosInf d
  | dSign d == Neg = False
  | otherwise = dValue d == Infinite

-- # decQuad functions not recreated here:

-- skipped: classString - not needed
-- skipped: copy - not needed
-- skipped: copyAbs - use abs instead
-- skipped: copyNegate - use negate instead
-- skipped: fromNumber - not needed
-- skipped: fromPacked - use fromPackedChecked instead
-- skipped: fromWider - not needed
-- skipped: getExponent, setExponent - use toBCD, fromBCD
-- skipped: getCoefficient, setCoefficient - use toBCD, fromBCD
-- skipped: isCanonical - not needed
-- skipped: radix - not needed
-- skipped: toNumber - not needed
-- skipped: toPacked - use decode function instead
-- skipped: toWider - not needed
-- skipped: show - not needed; impure
