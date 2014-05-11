{-# LANGUAGE OverloadedStrings #-}

module Conditions where

import qualified Data.ByteString.Char8 as BS8
import TestLog
import Data.String
import qualified Deka.Context as C
import Data.Monoid
import Data.Char (toLower)
import Data.List (sort)

-- | Parses and sorts a list of conditions.

parseConditions :: [BS8.ByteString] -> TestLog [C.Flag]
parseConditions = fmap sort . mapM parseCondition

parseCondition :: BS8.ByteString -> TestLog C.Flag
parseCondition str = case lookup s allConditions of
  Nothing -> flunk $ "could not parse condition: " <> str
  Just f -> tell ("parsed condition: " <> str) >> return f
  where
    s = map toLower . BS8.unpack $ str

allConditions :: IsString a => [(a, C.Flag)]
allConditions =
  [ ("clamped", C.clamped)
  , ("conversion_syntax", C.conversionSyntax)
  , ("division_by_zero", C.divisionByZero)
  , ("division_impossible", C.divisionImpossible)
  , ("division_undefined", C.divisionUndefined)
  , ("inexact", C.inexact)
  , ("insufficient_storage", C.insufficientStorage)
  , ("invalid_context", C.invalidContext)
  , ("invalid_operation", C.invalidOperation)
  , ("lost_digits", C.lostDigits)
  , ("overflow", C.overflow)
  , ("rounded", C.rounded)
  , ("subnormal", C.subnormal)
  , ("underflow", C.underflow)
  ]
 
