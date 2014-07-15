{-# LANGUAGE OverloadedStrings #-}
module Result where

import TestLog
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Dec as D
import qualified Deka.Context as C
import Data.Monoid

-- | Parses a result token.  Returns Nothing if the result token was
-- a question mark, as this indicates that the result is undefined.
-- Bypasses if the token contains an octothorpe.  Otherwise, returns
-- the Dec.
--
-- Probably the easiest way to do the comparison is going to be to
-- take the output of the test function, apply @toByteString@ to it,
-- and then test that for equality with the result of applying
-- @toByteString@ to the result returned from here.

result
  :: BS8.ByteString
  -> TestLog (Maybe D.Dec)
result bs
  | '#' `BS8.elem` bs =
      bypass $ "result contains an octothorpe: " <> bs

  | '?' `BS8.elem` bs = do
      tell "result token contains a question mark"
      return Nothing

  | otherwise = do
      tell $ "parsing result token: " <> bs
      let r = C.runCtx (D.fromByteString bs)
      tell $ "result parse result: " <> D.toByteString r
      return . Just $ r

-- | Parses result token where the function is expecting an
-- Ordering.

resultOrd
  :: BS8.ByteString
  -> TestLog Ordering
resultOrd bs
  | '#' `BS8.elem` bs =
      bypass $ "result contains an octothorpe: " <> bs

  | '?' `BS8.elem` bs =
      flunk "result token contains a question mark"

  | otherwise = do
      tell $ "parsing result token: " <> bs
      case bs of
        "-1" -> return LT
        "0" -> return EQ
        "1" -> return GT
        _ -> flunk $ "unrecognized Ord result: " <> bs

resultBool
  :: BS8.ByteString
  -> TestLog Bool
resultBool bs
  | '#' `BS8.elem` bs =
      bypass $ "result contains an octothorpe: " <> bs

  | '?' `BS8.elem` bs =
      flunk "result token contains a question mark"

  | otherwise = do
      tell $ "parsing result token: " <> bs
      case bs of
        "1" -> return True
        "0" -> return False
        _ -> flunk $ "unrecognized Bool result: " <> bs

