{-# LANGUAGE Safe #-}
module Deka.Internal.Quad.Decoding where

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
import Deka.Internal.Quad.Quad

-- ## Decoding and encoding helpers

-- | A pure Haskell type which holds information identical to that
-- in a 'Quad'.
data Decoded = Decoded
  { dSign :: Sign
  , dValue :: Value
  } deriving (Eq, Ord, Show)

data Value
  = Finite Coefficient Exponent
  | Infinite
  | NaN NaN Payload
  deriving (Eq, Ord, Show)

-- | A list of digits, less than or equal to 'coefficientLen' long.
-- Corresponds only to finite numbers.
newtype Coefficient = Coefficient { unCoefficient :: [Digit] }
  deriving (Eq, Ord, Show)

instance Bounded Coefficient where
  minBound = Coefficient [D0]
  maxBound = Coefficient $ replicate coefficientLen D9

instance Enum Coefficient where
  toEnum i
    | i < 0 = error $ "Deka.Quad.Coefficient.toEnum: argument "
      ++ "out of range; is negative"
    | length r > coefficientLen = error $ "Deka.Quad.Coefficient."
        ++ "toEnum: argument too large"
    | otherwise = Coefficient r
    where
      r = integralToDigits i

  fromEnum i
    | r > (fromIntegral (maxBound :: Int)) =
        error $ "Deka.Quad.Coefficient.fromEnum:"
          ++ " argument too large to fit into Int"
    | otherwise = fromIntegral r
    where
      r = digitsToInteger . unCoefficient $ i

-- | Creates a 'Coefficient'.  Checks to ensure it is not null and
-- that it is not longer than 'coefficientLen' and that it does not
-- have leading zeroes (if it is 0, a single 'D0' is allowed).
coefficient :: [Digit] -> Maybe Coefficient
coefficient ls
  | null ls = Nothing
  | length ls > 1 && head ls == D0 = Nothing
  | length ls > coefficientLen = Nothing
  | otherwise = Just . Coefficient $ ls

-- | Coefficient of 'D0'
zeroCoefficient :: Coefficient
zeroCoefficient = Coefficient [D0]

-- | Coefficient of 'D1'
oneCoefficient :: Coefficient
oneCoefficient = Coefficient [D1]

-- | A list of digits, less than or equal to 'payloadLen'
-- long.  Accompanies an NaN, potentially with diagnostic
-- information (I do not know if decNumber actually makes use of
-- this.)
newtype Payload = Payload { unPayload :: [Digit] }
  deriving (Eq, Ord, Show)

instance Bounded Payload where
  minBound = Payload [D0]
  maxBound = Payload $ replicate payloadLen D9

instance Enum Payload where
  toEnum i
    | i < 0 = error $ "Deka.Quad.Payload.toEnum: argument "
      ++ "out of range; is negative"
    | length r > payloadLen = error $ "Deka.Quad.Payload."
        ++ "toEnum: argument too large"
    | otherwise = Payload r
    where
      r = integralToDigits i

  fromEnum i
    | r > (fromIntegral (maxBound :: Int)) =
        error $ "Deka.Quad.Payload.fromEnum:"
          ++ " argument too large to fit into Int"
    | otherwise = fromIntegral r
    where
      r = digitsToInteger . unPayload $ i

-- | Creates a 'Payload'.  Checks to ensure it is not null, not
-- longer than 'payloadLen' and that it does not have leading zeroes
-- (if it is 0, a single 'D0' is allowed).
payload :: [Digit] -> Maybe Payload
payload ds
  | null ds = Nothing
  | length ds > 1 && head ds == D0 = Nothing
  | length ds > payloadLen = Nothing
  | otherwise = Just . Payload $ ds

-- | Payload of [D0]
zeroPayload :: Payload
zeroPayload = Payload [D0]


-- | Maximum number of digits in a coefficient.
coefficientLen :: Int
coefficientLen = c'DECQUAD_Pmax

-- | Maximum number of digits in a payload.
payloadLen :: Int
payloadLen = c'DECQUAD_Pmax - 1

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
minMaxExp :: (Int, Int)
minMaxExp = (l, h)
  where
    l = c'DECQUAD_Emin - c'DECQUAD_Pmax + 1
    h = c'DECQUAD_Emax - c'DECQUAD_Pmax + 1

-- | An adjusted exponent is the value of an exponent of a number
-- when that number is expressed as though in scientific notation
-- with one digit before any decimal point.  This is the finite
-- exponent + (number of significant digits - 1).
newtype AdjustedExp = AdjustedExp { unAdjustedExp :: Int }
  deriving (Eq, Show, Ord)

instance Bounded AdjustedExp where
  minBound = AdjustedExp $ fst minMaxExp
  maxBound = AdjustedExp $ snd minMaxExp + coefficientLen - 1

instance Enum AdjustedExp where
  toEnum i
    | r < minBound = error e
    | r > maxBound = error e
    | otherwise = r
    where
      r = AdjustedExp i
      e = "Deka.AdjustedExp.toEnum: integer out of range"

  fromEnum (AdjustedExp i) = i

adjustedExp :: Coefficient -> Exponent -> AdjustedExp
adjustedExp ds e = AdjustedExp $ unExponent e
  + dDigits ds - 1

-- | The number of significant digits. Zero returns 1.
dDigits :: Coefficient -> Int
dDigits (Coefficient ds) = case dropWhile (== D0) ds of
  [] -> 1
  rs -> length rs

adjustedToExponent :: Coefficient -> AdjustedExp -> Exponent
adjustedToExponent ds e = Exponent $ unAdjustedExp e -
  dDigits ds + 1

-- | The smallest possible adjusted exponent that is still normal.
-- Adjusted exponents smaller than this are subnormal.
minNormalAdj :: AdjustedExp
minNormalAdj = AdjustedExp c'DECQUAD_Emin

-- | Like 'minNormalAdj', but returns the size of the regular exponent
-- rather than the adjusted exponent.
minNormalExp :: Coefficient -> Exponent
minNormalExp c = adjustedToExponent c $ minNormalAdj

-- | The signed integer which indicates the power of ten by which
-- the coefficient is multiplied.
newtype Exponent = Exponent { unExponent :: Int }
  deriving (Eq, Ord, Show)

instance Bounded Exponent where
  minBound = Exponent . fst $ minMaxExp
  maxBound = Exponent . snd $ minMaxExp

instance Enum Exponent where
  toEnum i
    | r < minBound = error e
    | r > maxBound = error e
    | otherwise = r
    where
      r = Exponent i
      e = "Deka.Exponent.toEnum: integer out of range"

  fromEnum (Exponent i) = i

-- | Ensures that the exponent is within the range allowed by
-- 'minMaxExp'.
exponent :: Int -> Maybe Exponent
exponent i
  | i < l = Nothing
  | i > h = Nothing
  | otherwise = Just . Exponent $ i
  where
    (l, h) = minMaxExp

-- | An Exponent whose value is 0.
zeroExponent :: Exponent
zeroExponent = Exponent 0


toDecNumberBCD :: Decoded -> (Int32, [Word8], Int32)
toDecNumberBCD (Decoded s v) = (e, ds, sgn)
  where
    sgn = case s of { NonNeg -> 0; Neg -> c'DECFLOAT_Sign }
    (e, ds) = case v of
      Infinite -> (c'DECFLOAT_Inf, replicate c'DECQUAD_Pmax 0)
      NaN n (Payload ps) -> (ns, np)
        where
          ns = case n of
            Quiet -> c'DECFLOAT_qNaN
            Signaling -> c'DECFLOAT_sNaN
          np = pad ++ map digitToInt ps
          pad = replicate (c'DECQUAD_Pmax - length ps) 0
      Finite (Coefficient digs) (Exponent ex) ->
        ( fromIntegral ex, pad ++ map digitToInt digs )
        where
          pad = replicate (c'DECQUAD_Pmax - length digs) 0

-- | Encodes a new 'Quad'.
fromBCD :: Decoded -> IO Quad
fromBCD dcd =
  newQuad >>= \d ->
  withForeignPtr (unQuad d) $ \pD ->
  let (expn, digs, sgn) = toDecNumberBCD dcd in
  withArray digs $ \pArr ->
  unsafe'c'decQuadFromBCD pD expn pArr sgn >>
  return d

-- | A Quad with coefficient 'D1', exponent 0, and sign 'NonNeg'.
one :: IO Quad
one = fromBCD
  $ Decoded NonNeg (Finite (Coefficient [D1]) (Exponent 0))

-- | Decodes a 'Quad' to a pure Haskell type which holds identical
-- information.
toBCD :: Quad -> IO Decoded
toBCD d =
  withForeignPtr (unQuad d) $ \pD ->
  allocaBytes c'DECQUAD_Pmax $ \pArr ->
  alloca $ \pExp ->
  unsafe'c'decQuadToBCD pD pExp pArr >>= \sgn ->
  peek pExp >>= \ex ->
  peekArray c'DECQUAD_Pmax pArr >>= \coef ->
  return (getDecoded sgn ex coef)


getDecoded
  :: Int32
  -- ^ Sign. Zero if sign is zero; non-zero if sign is not zero
  -- (that is, is negavite.)
  -> Int32
  -- ^ Exponent
  -> [Word8]
  -- ^ Coefficient
  -> Decoded
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

scientific :: Decoded -> String
scientific d = sign ++ rest
  where
    sign = case dSign d of
      NonNeg -> ""
      Neg -> "-"
    rest = case dValue d of
      Infinite -> "Infinity"
      Finite c e -> sciFinite c e
      NaN n p -> sciNaN n p

sciFinite :: Coefficient -> Exponent -> String
sciFinite c e = sCoe ++ 'E':sExp
  where
    sCoe = case unCoefficient c of
      x:xs -> digitToChar x : case xs of
        [] -> []
        _ -> '.' : map digitToChar xs
      [] -> error "sciFinite: empty coefficient"
    sExp = show . unAdjustedExp . adjustedExp c $ e

sciNaN :: NaN -> Payload -> String
sciNaN n p = nStr ++ pStr
  where
    nStr = case n of { Quiet -> "NaN"; Signaling -> "sNaN" }
    pStr = case unPayload p of
      [D0] -> ""
      xs -> map digitToChar xs

-- | Converts Decoded to ordinary decimal notation.  For NaNs and
-- infinities, the notation is identical to that of 'scientific'.
-- Unlike 'scientific', though the result can always be converted back
-- to a 'Quad' using 'fromByteString', the number of significant
-- digits might change.  For example, though @1.2E3@ has two
-- significant digits, using @ordinary@ on this value and then
-- reading it back in with @fromByteString@ will give you @1200E0@,
-- which has four significant digits.

ordinary :: Decoded -> String
ordinary d = sign ++ rest
  where
    sign = case dSign d of
      NonNeg -> ""
      Neg -> "-"
    rest = case dValue d of
      Infinite -> "Infinity"
      Finite c e -> onyFinite c e
      NaN n p -> sciNaN n p

onyFinite :: Coefficient -> Exponent -> String
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

-- | Converts a Decoded to a Rational.  Returns Nothing if the
-- Decoded is not finite.
decodedToRational :: Decoded -> Maybe Rational
decodedToRational d = case dValue d of
  (Finite c e) ->
    let int = digitsToInteger . unCoefficient $ c
        ex = unExponent e
        mkSgn = if dSign d == NonNeg then id else negate
        mult = if ex < 0 then 1 % (10 ^ Prelude.abs ex) else 10 ^ ex
    in Just . mkSgn $ fromIntegral int * mult
  _ -> Nothing

-- # Decoded predicates

dIsFinite :: Decoded -> Bool
dIsFinite (Decoded _ v) = case v of
  Finite _ _ -> True
  _ -> False

dIsInfinite :: Decoded -> Bool
dIsInfinite (Decoded _ v) = case v of
  Infinite -> True
  _ -> False

dIsInteger :: Decoded -> Bool
dIsInteger (Decoded _ v) = case v of
  Finite _ e -> unExponent e == 0
  _ -> False

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
-- The sign must be NonNeg (that is, you cannot have a negative
-- zero.)
dIsLogical :: Decoded -> Bool
dIsLogical (Decoded s v) = fromMaybe False $ do
  guard $ s == NonNeg
  (d, e) <- case v of
    Finite ds ex -> return (ds, ex)
    _ -> Nothing
  guard $ e == zeroExponent
  return
    . all (\x -> x == D0 || x == D1)
    . unCoefficient $ d

dIsNaN :: Decoded -> Bool
dIsNaN (Decoded _ v) = case v of
  NaN _ _ -> True
  _ -> False

-- | True only if @x@ is less than zero and is not an NaN.  It's not
-- enough for the sign to be Neg; the coefficient (if finite) must
-- be greater than zero.
dIsNegative :: Decoded -> Bool
dIsNegative (Decoded s v) = fromMaybe False $ do
  guard $ s == Neg
  return $ case v of
    Finite d _ -> any (/= D0) . unCoefficient $ d
    Infinite -> True
    _ -> False

dIsNormal :: Decoded -> Bool
dIsNormal (Decoded _ v) = case v of
  Finite d e
    | adjustedExp d e < minNormalAdj -> False
    | otherwise -> any (/= D0) . unCoefficient $ d
  _ -> False

dIsPositive :: Decoded -> Bool
dIsPositive (Decoded s v)
  | s == Neg = False
  | otherwise = case v of
      Finite d _ -> any (/= D0) . unCoefficient $ d
      Infinite -> True
      _ -> False

dIsSignaling :: Decoded -> Bool
dIsSignaling (Decoded _ v) = case v of
  NaN Signaling _ -> True
  _ -> False


dIsSigned :: Decoded -> Bool
dIsSigned (Decoded s _) = s == Neg

dIsSubnormal :: Decoded -> Bool
dIsSubnormal (Decoded _ v) = case v of
  Finite d e -> adjustedExp d e < minNormalAdj
  _ -> False

-- | True for any zero (negative or positive zero).
dIsZero :: Decoded -> Bool
dIsZero (Decoded _ v) = case v of
  Finite d _ -> all (== D0) . unCoefficient $ d
  _ -> False

-- # DecClass-like Decoded predicates

dIsSNaN :: Decoded -> Bool
dIsSNaN d = case dValue d of
  NaN n _ -> n == Signaling
  _ -> False

dIsQNaN :: Decoded -> Bool
dIsQNaN d = case dValue d of
  NaN n _ -> n == Quiet
  _ -> False

dIsNegInf :: Decoded -> Bool
dIsNegInf d
  | dSign d == NonNeg = False
  | otherwise = dValue d == Infinite

dIsNegNormal :: Decoded -> Bool
dIsNegNormal d
  | dSign d == NonNeg = False
  | otherwise = case dValue d of
      Finite c e -> e >= minNormalExp c
      _ -> False

dIsNegSubnormal :: Decoded -> Bool
dIsNegSubnormal d
  | dSign d == NonNeg = False
  | otherwise = case dValue d of
      Finite c e -> e < minNormalExp c
      _ -> False

dIsNegZero :: Decoded -> Bool
dIsNegZero d
  | dSign d == NonNeg = False
  | otherwise = case dValue d of
      Finite c _ -> unCoefficient c == [D0]
      _ -> False

dIsPosZero :: Decoded -> Bool
dIsPosZero d
  | dSign d == Neg = False
  | otherwise = case dValue d of
      Finite c _ -> unCoefficient c == [D0]
      _ -> False

dIsPosSubnormal :: Decoded -> Bool
dIsPosSubnormal d
  | dSign d == Neg = False
  | otherwise = case dValue d of
      Finite c e -> e < minNormalExp c
      _ -> False

dIsPosNormal :: Decoded -> Bool
dIsPosNormal d
  | dSign d == Neg = False
  | otherwise = case dValue d of
      Finite c e -> e >= minNormalExp c
      _ -> False

dIsPosInf :: Decoded -> Bool
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
