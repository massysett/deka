module Util where

switch :: a -> [(Bool, a)] -> a
switch dflt [] = dflt
switch dflt ((bl, a):xs)
  | bl = a
  | otherwise = switch dflt xs

parseBool :: Int -> Maybe Bool
parseBool i
  | i == 0 = Just False
  | i == 1 = Just True
  | otherwise = Nothing

safeRead :: Read a => String -> Maybe a
safeRead a = case reads a of
  (x, ""):[] -> Just x
  _ -> Nothing

readNumber :: Read a => String -> Maybe a
readNumber a = case a of
  [] -> Nothing
  x:xs -> if x == '+' then safeRead xs else safeRead (x:xs)
