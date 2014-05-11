module Util where

switch :: a -> [(Bool, a)] -> a
switch dflt [] = dflt
switch dflt ((bl, a):xs)
  | bl = a
  | otherwise = switch dflt xs
