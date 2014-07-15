module Deka.Tests.Util where

import Test.QuickCheck

varInt :: Int -> Gen b -> Gen b
varInt = variant

coarbitraryList :: (a -> Gen b -> Gen b) -> [a] -> Gen b -> Gen b
coarbitraryList f ls = case ls of
  [] -> varInt 0
  x:xs -> varInt 1 . f x . coarbitraryList f xs
