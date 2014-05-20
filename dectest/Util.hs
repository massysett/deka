module Util where

import qualified Data.ByteString.Char8 as BS8

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

readNumberBS :: Read a => BS8.ByteString -> Maybe a
readNumberBS bs = readNumber (BS8.unpack bs)
