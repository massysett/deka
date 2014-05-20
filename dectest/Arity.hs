{-# LANGUAGE OverloadedStrings #-}

-- | Use functions in here to test any testcase except toSci,
-- toEng, or apply.  Those have special rules for operand parsing.
module Arity where

import TestLog
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Dec as D
import qualified Deka.Context as C
import Control.Monad
import Data.Monoid
import Operand
import Result
import Types
import Directives
import TestHelpers

type Unary = D.Dec -> C.Ctx D.Dec
type Binary = D.Dec -> D.Dec -> C.Ctx D.Dec
type Ternary = D.Dec -> D.Dec -> D.Dec -> C.Ctx D.Dec

unary :: Unary -> Test
unary f dirs ops rslt conds = runTestLog $ do
  ic <- parseDirectives dirs
  op <- case ops of
    x:[] -> operand ic x
    _ -> flunk $ "one operand expected, got " <>
      (BS8.pack . show . length $ ops)
  mayRslt <- result rslt
  let k = ic >> f op
      (r, fl) = C.runCtxStatus k
  testConditions conds fl
  testDec r mayRslt
  pass "conditions and result match targets"

binary :: Binary -> Test
binary f dirs ops rslt conds = runTestLog $ do
  ic <- parseDirectives dirs
  (opX, opY) <- case ops of
    x:y:[] -> do
      ox <- operand ic x
      oy <- operand ic y
      return (ox, oy)
    _ -> flunk $ "two operands expected, got " <>
      (BS8.pack . show . length $ ops)
  mayRslt <- result rslt
  let k = ic >> f opX opY
      (r, fl) = C.runCtxStatus k
  testConditions conds fl
  testDec r mayRslt
  pass "conditions and result match targets"

comparer :: (D.Dec -> D.Dec -> Ordering) -> Test
comparer f dirs ops rslt conds = runTestLog $ do
  when (not . null $ conds) . flunk $
    "comparer: conditions not null, which makes no sense "
    <> "for this kind of test."
  ic <- parseDirectives dirs
  (opX, opY) <- case ops of
    x:y:[] -> do
      ox <- operand ic x
      oy <- operand ic y
      return (ox, oy)
    _ -> flunk $ "two operands expected, got " <>
      (BS8.pack . show . length $ ops)
  rsltO <- resultOrd rslt
  let r = f opX opY
  if rsltO == r
    then pass "conditions and result match targets"
    else flunk $ "expected: " <> (BS8.pack . show $ rsltO)
          <> " got: " <> (BS8.pack . show $ r)

binaryTest :: (D.Dec -> D.Dec -> Bool) -> Test
binaryTest f dirs ops rslt conds = runTestLog $ do
  when (not . null $ conds) . flunk $
    "binaryTest: conditions not null, which makes no sense "
    <> "for this kind of test."
  ic <- parseDirectives dirs
  (opX, opY) <- case ops of
    x:y:[] -> do
      ox <- operand ic x
      oy <- operand ic y
      return (ox, oy)
    _ -> flunk $ "two operands expected, got " <>
      (BS8.pack . show . length $ ops)
  rsltB <- resultBool rslt
  let r = f opX opY
  if rsltB == r
    then pass "conditions and result match targets"
    else flunk $ "expected: " <> (BS8.pack . show $ rsltB)
          <> " got: " <> (BS8.pack . show $ r)


decAndIntegral
  :: (D.Dec -> D.Signed -> C.Ctx D.Dec) -> Test

decAndIntegral f dirs ops rslt conds = runTestLog $ do
  ic <- parseDirectives dirs
  (opX, opY) <- case ops of
    x:y:[] -> do
      ox <- operand ic x
      oy <- operandIntegral y
      return (ox, oy)
    _ -> flunk $ "two operands expected, got " <>
      (BS8.pack . show . length $ ops)
  let k = ic >> f opX opY
      (r, fl) = C.runCtxStatus k
  mayRslt <- result rslt
  testConditions conds fl
  testDec r mayRslt
  pass "conditions and results match targets"

ternary :: Ternary -> Test
ternary f dirs ops rslt conds = runTestLog $ do
  ic <- parseDirectives dirs
  (opX, opY, opZ) <- case ops of
    x:y:z:[] -> do
      ox <- operand ic x
      oy <- operand ic y
      oz <- operand ic z
      return (ox, oy, oz)
    _ -> flunk $ "three operands expected, got " <>
      (BS8.pack . show . length $ ops)
  mayRslt <- result rslt
  let k = ic >> f opX opY opZ
      (r, fl) = C.runCtxStatus k
  testConditions conds fl
  testDec r mayRslt
  pass "conditions and result match targets"

