{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Deka.Native.Abstract where

import Deka.Dec
import Prelude hiding (exponent)
import Control.Monad
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS8

-- # Types

-- | A digit from one to nine.  Useful to represent a most
-- significant digit, or MSD, as an MSD cannot be the digit zero.
data Novem =  D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Ord, Show, Enum, Bounded)

novemToChar :: Novem -> Char
novemToChar n = case n of
  { D1 -> '1'; D2 -> '2'; D3 -> '3'; D4 -> '4'; D5 -> '5';
    D6 -> '6'; D7 -> '7'; D8 -> '8'; D9 -> '9' }

charToNovem :: Char -> Maybe Novem
charToNovem c = case c of
  { '1' -> Just D1; '2' -> Just D2; '3' -> Just D3;
    '4' -> Just D4; '5' -> Just D5; '6' -> Just D6; '7' -> Just D7;
    '8' -> Just D8; '9' -> Just D9; _ -> Nothing }

novemToInt :: Integral a => Novem -> a
novemToInt d = case d of
  { D1 -> 1; D2 -> 2; D3 -> 3; D4 -> 4; D5 -> 5; D6 -> 6;
    D7 -> 7; D8 -> 8; D9 -> 9 }

intToNovem :: Integral a => a -> Maybe Novem
intToNovem a = case a of
  { 1 -> Just D1; 2 -> Just D2; 3 -> Just D3; 4 -> Just D4;
    5 -> Just D5; 6 -> Just D6;
    7 -> Just D7; 8 -> Just D8; 9 -> Just D9; _ -> Nothing }

-- | A digit from zero to nine.
data Decem
  = D0
  | Nonem Novem
  deriving (Eq, Ord, Show)

decemToChar :: Decem -> Char
decemToChar d = case d of
  { D0 -> '0'; Nonem n -> novemToChar n }

charToDecem :: Char -> Maybe Decem
charToDecem c = case c of
  { '0' -> Just D0; _ -> fmap Nonem (charToNovem c) }

decemToInt :: Integral a => Decem -> a
decemToInt d = case d of
  { D0 -> 0;  Nonem n -> novemToInt n }

decemToNovem :: Decem -> Maybe Novem
decemToNovem d = case d of
  Nonem n -> Just n
  _ -> Nothing

intToDecem :: Integral a => a -> Maybe Decem
intToDecem i = case i of
  { 0 -> Just D0; _ -> fmap Nonem $ intToNovem i }

intToDecemList :: Integral a => a -> (Sign, [Decem])
intToDecemList x = (sgn, ls)
  where
    sgn | x < 0 = Sign1
        | otherwise = Sign0
    ls = reverse . go . Prelude.abs $ x
    go !i =
      let (d, m) = i `divMod` 10
          r = maybe (error "intToDecemList: error") id
            . intToDecem $ m
      in if i == 0
          then []
          else r : go d

decemListToInt :: Integral a => [Decem] -> a
decemListToInt ds = foldl' f 0 . indices $ ds
  where
    indices = zip (iterate pred (length ds - 1))
    f acc (ix, d) = acc + decemToInt d * 10 ^ ix

-- | A non-empty set of digits.  The MSD must be from 1 to 9.

data Decuple = Decuple Novem [Decem]
  deriving (Eq, Ord, Show)

decupleToString :: Decuple -> String
decupleToString (Decuple msd rest) =
  novemToChar msd : map decemToChar rest

stringToDecuple :: String -> Maybe Decuple
stringToDecuple str = case str of
  [] -> Nothing
  x:xs -> liftM2 Decuple (charToNovem x) (mapM charToDecem xs)

decupleToInt :: Integral a => Decuple -> a
decupleToInt (Decuple n ds) =
  let len = length ds
      go !soFar !i digs = case digs of
        [] -> soFar
        x:xs ->
          let nxt = i - 1
              thisSum = soFar + decemToInt x * 10 ^ nxt
          in go thisSum nxt xs
  in novemToInt n * (10 ^ len) + go 0 len ds

uncons :: [a] -> Maybe (a, [a])
uncons a = case a of
  [] -> Nothing
  x:xs -> Just (x, xs)

intToDecuple :: Integral a => a -> Maybe (Sign, Decuple)
intToDecuple x = do
  let (sgn, ds) = intToDecemList x
  (d1, dr) <- uncons ds
  let nv = maybe (error "intToDecuple: MSD is not zero") id
        . decemToNovem $ d1
  return (sgn, Decuple nv dr)

decemListToDecuple :: [Decem] -> Maybe Decuple
decemListToDecuple ds = case dropWhile (== D0) ds of
  [] -> Nothing
  x:xs -> Just $ Decuple d1 xs
    where
      d1 = maybe (error "decemListToDecuple: bad MSD") id
        . decemToNovem $ x


-- | Either a set of digits, or zero.  Unsigned.

data Aut
  = Nil
  -- ^ Zero
  | Plenus Decuple
  -- ^ Non-zero
  deriving (Eq, Ord, Show)

autToString :: Aut -> String
autToString a = case a of
  Nil -> "0"
  Plenus ds -> decupleToString ds

stringToAut :: String -> Maybe Aut
stringToAut s = case s of
  "0" -> Just Nil
  _ -> fmap Plenus $ stringToDecuple s

autToInt :: Integral a => Aut -> a
autToInt a = case a of
  Nil -> 0
  Plenus d -> decupleToInt d

-- | Fails if the argument is less than zero.
intToAut :: Integral a => a -> Maybe Aut
intToAut a = case intToDecuple a of
  Nothing -> Just Nil
  Just (s, d) -> case s of
    Sign1 -> Nothing
    _ -> return . Plenus $ d

decemListToAut :: [Decem] -> Aut
decemListToAut ds = case dropWhile (== D0) ds of
  [] -> Nil
  x:xs -> Plenus $ Decuple d1 xs
    where
      d1 = maybe (error "decemListToAut: bad MSD") id
        . decemToNovem $ x

-- | Either a set of digits, or zero.  Signed.

data Firmado
  = Cero
  -- ^ Zero
  | Completo PosNeg Decuple
  -- ^ Non-zero
  deriving (Eq, Ord, Show)

firmadoToString :: Firmado -> String
firmadoToString x = case x of
  Cero -> "0"
  Completo p d -> sgn : decupleToString d
    where
      sgn = case p of { Pos -> '+'; Neg -> '-' }

stringToFirmado :: String -> Maybe Firmado
stringToFirmado s
  | s == "0" = Just Cero
  | otherwise = do
      (sgn, rst) <- case s of
        "" -> Nothing
        x:xs -> case x of
          '+' -> return (Pos, xs)
          '-' -> return (Neg, xs)
          _ -> Nothing
      dec <- stringToDecuple rst
      return $ Completo sgn dec

firmadoToInt :: Integral a => Firmado -> a
firmadoToInt x = case x of
  Cero -> 0
  Completo p d -> apply . decupleToInt $ d
    where
      apply = case p of { Pos -> id; Neg -> negate }

intToFirmado :: Integral a => a -> Firmado
intToFirmado i = case intToDecuple i of
  Nothing -> Cero
  Just (sgn, d) -> Completo p d
    where
      p = case sgn of { Sign0 -> Pos; Sign1 -> Neg }


--
-- Types in Abstract
--

-- | The coefficient in a number; not used in infinities or NaNs.
newtype Coefficient = Coefficient { unCoefficient :: Aut }
  deriving (Eq, Ord, Show)

-- | The exponent in a number.
newtype Exponent = Exponent { unExponent :: Firmado }
  deriving (Eq, Ord, Show)

-- | The diagnostic information in an NaN.
newtype Diagnostic = Diagnostic { unDiagnostic :: Decuple }
  deriving (Eq, Ord, Show)

-- | Whether an NaN is quiet or signaling.
data Noisy = Quiet | Signaling
  deriving (Eq, Ord, Show)

-- | Not a Number.
data NonNum = NonNum
  { noisy :: Noisy
  , diagnostic :: Maybe Diagnostic
  } deriving (Eq, Ord, Show)

-- | All data in an abstract number except for the sign.
data Value
  = Finite Coefficient Exponent
  | Infinite
  | NotANumber NonNum
  deriving (Eq, Ord, Show)

-- | Abstract representation of all numbers covered by the General
-- Decimal Arithmetic Specification.
data Abstract = Abstract
  { sign :: Sign
  , value :: Value
  } deriving (Eq, Ord, Show)

signToString :: Sign -> String
signToString s = case s of
  Sign0 -> ""
  Sign1 -> "-"

-- | Adjusted exponent.  Roughly speaking this represents the
-- coefficient and exponent of an abstract decimal, adjusted so
-- there is a decimal point between the most significant digit of
-- the coefficient and the remaning digits.
newtype AdjustedExp = AdjustedExp { unAdjustedExp :: Integer }
  deriving (Eq, Ord, Show)

-- | Computes an adjusted exponent.  The length of a zero
-- coefficient is one.
adjustedExp :: Coefficient -> Exponent -> AdjustedExp
adjustedExp coe ex = AdjustedExp $ e + (c - 1)
  where
    e = firmadoToInt . unExponent $ ex
    c = fromIntegral $ case unCoefficient coe of
      Nil -> 1
      Plenus (Decuple _ ds) -> length ds + 1

fmtAdjustedExp :: AdjustedExp -> String
fmtAdjustedExp (AdjustedExp i) = 'E' : sgn : digs
  where
    sgn | i < 0 = '-'
        | otherwise = '+'
    digs = show . Prelude.abs $ i

finiteToString :: Coefficient -> Exponent -> String
finiteToString c e = coe ++ ae
  where
    coe = case unCoefficient c of
      Nil -> "0"
      Plenus (Decuple n ds)
        | null ds -> [novemToChar n]
        | otherwise -> novemToChar n : '.' : map decemToChar ds
    ae = fmtAdjustedExp $ adjustedExp c e

nanToString :: NonNum -> String
nanToString (NonNum n d) = pfx ++ "NaN" ++ dia
  where
    pfx = case n of { Quiet -> ""; Signaling -> "s" }
    dia = maybe "" (decupleToString . unDiagnostic) d

fmtValue :: Value -> String
fmtValue v = case v of
  Finite c e -> finiteToString c e
  Infinite -> "Infinity"
  NotANumber n -> nanToString n

-- | Transform an 'Abstract' to a 'String'.  This conforms to the
-- @to-scientific-string@ transformation given in the General
-- Decimal Arithmetic Specification at
--
-- <http://speleotrove.com/decimal/daconvs.html#reftostr>
--
-- with one exception: the specification provides that some finite
-- numbers are represented without exponential notation.
-- 'abstractToString' /always/ uses exponential notation on finite
-- numbers.
abstractToString :: Abstract -> String
abstractToString (Abstract s v) = sgn ++ fmtValue v
  where
    sgn = case s of { Sign0 -> ""; Sign1 -> "-" }

-- | Transforms an 'Abstract' to a 'Dec'. Result is computed in a
-- context using the 'Pedantic' initializer.  Result is returned
-- along with any status flags arising from the computation.
abstractToDec :: Abstract -> (Dec, Flags)
abstractToDec = runCtxStatus . fromByteString
  . BS8.pack . abstractToString

