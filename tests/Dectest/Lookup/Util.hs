module Dectest.Lookup.Util where

import Dectest.Interp.Octothorpe (WhichPrecision(..))
import qualified Dectest.Interp.Result as R
import qualified Deka.Context as C
import qualified Data.ByteString.Char8 as BS8

testEq
  :: (R.ToByteString r, R.Result r)
  => r
  -> C.Ctx (BS8.ByteString, BS8.ByteString -> Maybe (C.Ctx Bool))
testEq a = return (R.toByteString a, f)
  where
    f bs = case R.result bs of
      Nothing -> Nothing
      Just getR -> Just $ getR a

unary
  :: (R.ToByteString r, R.Result r)
  => WhichPrecision
  -> (a -> C.Ctx r)
  -> [WhichPrecision -> C.Ctx a]
  -> Maybe (C.Ctx (BS8.ByteString, BS8.ByteString -> Maybe (C.Ctx Bool)))
unary wp f ls = case ls of
  x:[] -> Just $ do
    a <- x wp
    r <- f a
    testEq r
  _ -> Nothing


binary
  :: (R.ToByteString r, R.Result r)
  => WhichPrecision
  -> (a -> a -> C.Ctx r)
  -> [WhichPrecision -> C.Ctx a]
  -> Maybe (C.Ctx (BS8.ByteString, BS8.ByteString -> Maybe (C.Ctx Bool)))
binary wp f ls = case ls of
  x:y:[] -> Just $ do
    a <- x wp
    b <- y wp
    r <- f a b
    testEq r
  _ -> Nothing

ternary
  :: (R.ToByteString r, R.Result r)
  => WhichPrecision
  -> (a -> a -> a -> C.Ctx r)
  -> [WhichPrecision -> C.Ctx a]
  -> Maybe (C.Ctx (BS8.ByteString, BS8.ByteString -> Maybe (C.Ctx Bool)))
ternary wp f ls = case ls of
  x:y:z:[] -> Just $ do
    a <- x wp
    b <- y wp
    c <- z wp
    r <- f a b c
    testEq r
  _ -> Nothing

unaryStr
  :: (a -> BS8.ByteString)
  -> [WhichPrecision -> C.Ctx a]
  -> Maybe (C.Ctx (BS8.ByteString, BS8.ByteString -> Maybe (C.Ctx Bool)))
unaryStr f ls = case ls of
  x:[] -> Just $ do
    a <- x FromCtx
    let r = f a
        getBool bs = Just $ return (bs == r)
    return (r, getBool)
  _ -> Nothing

testIntegralValue
  :: (R.ToByteString a, R.Result a)
  => (C.Round -> a -> C.Ctx a)
  -> [WhichPrecision -> C.Ctx a]
  -> Maybe (C.Ctx (BS8.ByteString, BS8.ByteString -> Maybe (C.Ctx Bool)))
testIntegralValue f ls = case ls of
  x:[] -> Just $ do
    a <- x FromCtx
    rnd <- C.getRound
    r <- f rnd a
    let shw = R.toByteString r
        getBool bs = case R.result bs of
          Nothing -> Nothing
          Just fn -> Just $ fn r
    return (shw, getBool)
  _ -> Nothing
