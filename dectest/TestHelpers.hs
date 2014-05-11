{-# LANGUAGE OverloadedStrings #-}
module TestHelpers where

import TestLog
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Dec as D
import qualified Deka.Context as C
import Data.Monoid
import Conditions
import Data.List (sort)

testConditions
  :: [BS8.ByteString]
  -- ^ List of conditions
  -> [C.Flag]
  -- ^ Actual conditions from test
  -> TestLog ()
testConditions bs fs = do
  cs <- parseConditions bs
  if sort fs == cs
    then tell "conditions are as expected"
    else flunk $ "conditions not as expected: " <>
          (BS8.pack . show . sort $ fs)

testDec :: D.Dec -> Maybe D.Dec -> TestLog ()
testDec x y = case y of
  Nothing -> tell "target result is undefined" >> return ()
  Just r ->
    let sx = D.toByteString x
        sr = D.toByteString r
        res | sx == sr = tell "result is as expected"
            | otherwise =
                flunk $ "output: " <> sx <> " expected result: " <> sr
    in res
