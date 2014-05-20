{-# LANGUAGE OverloadedStrings #-}
-- | Handling special cases: toSci, toEng, apply, class.

module Specials where

import qualified Data.ByteString.Char8 as BS8
import TestLog
import qualified Deka.Context as C
import qualified Deka.Dec as D
import Types
import Operand
import Directives
import TestHelpers
import Data.Monoid
import Result
import Data.Char (toLower)
import Control.Arrow (first)

toSciOrEng :: (D.Dec -> BS8.ByteString) -> Test
toSciOrEng fn dirs opS rsS cdS = runTestLog $ do
  ic <- parseDirectives dirs
  getOp <- case opS of
    x:[] -> operandSciEngAp ic x
    _ -> flunk $ "expected 1 operand, got " <>
      (BS8.pack . show . length $ opS)
  let (rConv, fl) = C.runCtxStatus getOp
      r = fn rConv
  testConditions cdS fl
  if r == rsS
    then pass $ "string " <> r <> " matches expected result"
    else flunk $ "string " <> r <> " does not match expected "
            <> "result of " <> rsS

toSci :: Test
toSci = toSciOrEng D.toByteString

toEng :: Test
toEng = toSciOrEng D.toEngByteString

apply :: Test
apply dirs opS rsS cdS = runTestLog $ do
  ic <- parseDirectives dirs
  getOp <- case opS of
    x:[] -> operandSciEngAp ic x
    _ -> flunk $ "expected 1 operand, got " <>
      (BS8.pack . show . length $ opS)
  let k = getOp >>= D.plus
      (rConv, fl) = C.runCtxStatus k
      r = D.toByteString rConv
  tgt <- result rsS
  testConditions cdS fl
  case tgt of
    Nothing -> flunk $ "target result of apply is undefined; "
      <> "this makes no sense"
    Just t
      | D.toByteString t == r -> pass "result is as expected"
      | otherwise -> flunk $ "result of " <> r
          <> " does not match expected result of "
          <> D.toByteString t

parseClass :: BS8.ByteString -> TestLog D.Class
parseClass bs = case lookup lwr ls of
  Nothing -> flunk $ "could not parse class: " <> bs
  Just r -> do
    tell $ "parsed class: " <> bs
    return r
  where
    lwr = map toLower . BS8.unpack $ bs
    ls = map (first (map toLower)) D.strToClass

decClass :: Test
decClass dirs opS rsS cdS = runTestLog $ do
  ic <- parseDirectives dirs
  op <- case opS of
    x:[] -> operand ic x
    _ -> flunk $ "expected 1 operand, got " <>
      (BS8.pack . show . length $ opS)
  tgt <- parseClass rsS
  let k = ic >> D.numClass op
      (r, fl) = C.runCtxStatus k
  testConditions cdS fl
  if r == tgt
    then pass "result is as expected"
    else flunk $ "unexpected class: result is " <>
            (BS8.pack . show $ r)
