{-# LANGUAGE Safe #-}
-- | Components of decoded numbers that are common to DecNum and
-- Quad.  Some components (such as Coefficient) are not here because
-- the invariants for these components differ between DecNum and
-- Quad.
module Deka.Decoded where

-- | A single decimal digit.
data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Ord, Show, Enum, Bounded)

digitToInt :: Integral a => Digit -> a
digitToInt d = case d of
  { D0 -> 0; D1 -> 1; D2 -> 2; D3 -> 3; D4 -> 4; D5 -> 5;
    D6 -> 6; D7 -> 7; D8 -> 8; D9 -> 9 }

intToDigit :: Integral a => a -> Digit
intToDigit i = case i of
  { 0 -> D0; 1 -> D1; 2 -> D2; 3 -> D3; 4 -> D4;
    5 -> D5; 6 -> D6; 7 -> D7; 8 -> D8; 9 -> D9;
    _ -> error "intToDigit: integer out of range" }

digitToChar :: Digit -> Char
digitToChar d = case d of
  { D0 -> '0'; D1 -> '1'; D2 -> '2'; D3 -> '3'; D4 -> '4';
    D5 -> '5'; D6 -> '6'; D7 -> '7'; D8 -> '8'; D9 -> '9' }

-- | The most significant digit is at the head of the list.
digitsToInteger :: [Digit] -> Integer
digitsToInteger ls = go (length ls - 1) 0 ls
  where
    go c t ds = case ds of
      [] -> t
      x:xs -> let m = digitToInt x * 10 ^ c
                  t' = m + t
                  c' = c - 1
                  _types = c :: Int
              in go c' t' xs

-- | The most significant digit is at
-- the head of the list.  Sign of number is not relevant.
integralToDigits :: Integral a => a -> [Digit]
integralToDigits = reverse . go . abs
  where
    go i
      | i == 0 = []
      | otherwise =
          let (d, m) = i `divMod` 10
          in intToDigit m : go d

