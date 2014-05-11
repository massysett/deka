{-# LANGUAGE OverloadedStrings #-}
module Operand where

import TestLog
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Dec as D
import qualified Deka.Context as C
import Data.Monoid

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
      let len = fromIntegral $ BS8.length bs
          strlen = BS8.pack . show $ len
      pc <- case C.precision len of
        Nothing -> flunk $ "could not set precision: " <> strlen
        Just x -> return x
      tell $ "set precision for operand parse to: " <> strlen
      let k = ic >> C.setPrecision pc >> D.fromByteString bs
          r = C.runCtx C.initQuad k
      tell $ "operand parse result: " <> D.toByteString r
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
