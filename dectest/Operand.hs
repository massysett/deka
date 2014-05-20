{-# LANGUAGE OverloadedStrings #-}
module Operand where

import TestLog
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Dec as D
import qualified Deka.Context as C
import Data.Monoid
import Util

-- | Parses an operand.  Do not use this function for @toSci@,
-- @toEng@, or @apply@ as those have special rules for handling the
-- context.
--
-- Bypasses if the operand contains any octothorpe.

operand
  :: C.Ctx ()
  -- ^ Initial context to use
  -> BS8.ByteString
  -- ^ Parse this token
  -> TestLog D.Dec
operand ic bs
  | '#' `BS8.elem` bs =
      bypass $ "operand contains an octothorpe: " <> bs
  | otherwise = do
      tell $ "parsing operand token: " <> bs
      let k = do
            ic
            p <- fmap C.unPrecision C.setMaxPrecision
            s <- D.fromByteString bs
            return (p, s)
      let (p, r) = C.runCtx k
      tell $ "operand parse result: " <> D.toByteString r
        <> " parsed at precision: " <> BS8.pack (show p)
      return r

-- | Parses an operand into a context.  Use this function for
-- @toSci@, @toEng@, and @apply@.  Bypasses if the operand contains
-- any octothorpe.
operandSciEngAp
  :: C.Ctx ()
  -- ^ Initial context to use
  -> BS8.ByteString
  -- ^ Parse this token
  -> TestLog (C.Ctx D.Dec)
operandSciEngAp ic bs
  | '#' `BS8.elem` bs =
      bypass $ "operand contains an octothorpe: " <> bs
  | otherwise = do
      tell $ "parsing operand token: " <> bs
      let k = ic >> D.fromByteString bs
      return k

-- | Parses a signed integral operand.
operandIntegral
  :: BS8.ByteString
  -> TestLog D.Signed
operandIntegral bs
  | '#' `BS8.elem` bs =
      bypass $ "operand contains an octothorpe: " <> bs

  | otherwise = case readNumberBS bs of
      Nothing -> flunk $ "could not parse integral operand: " <> bs
      Just r -> do
        tell $ "parsed integral operand: " <> bs
        return r
