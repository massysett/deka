{-# LANGUAGE OverloadedStrings #-}
module Runner where

import qualified Types as T
import qualified Parse as P
import qualified Deka.Context as C
import Data.Maybe
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid
import TestLog
import Util
import Data.String
import Control.Arrow (first, second)
import Queue


addDirective
  :: P.Keyword
  -> P.Value
  -> TestLog (C.Ctx ())
addDirective (P.Keyword kw) (P.Value val)
  | k == "precision" = case readNumber v >>= C.precision of
      Nothing -> flunk $ "could not set precision: " <> val
      Just p -> do
        tell $ "setting precision to " <> val
        return . C.setPrecision $ p

  | k == "rounding" = do
      let dflt = flunk $ "could not set rounding: " <> val
          mayRnd = lookup val allRounds
      rnd <- maybe dflt return mayRnd
      tell $ "setting rounding to " <> val
      return . C.setRound $ rnd

  | k == "maxexponent" = case readNumber v >>= C.emax of
      Nothing -> flunk $ "could not set Emax: " <> val
      Just p -> do
        tell $ "setting Emax to " <> val
        return . C.setEmax $ p

  | k == "minexponent" = case readNumber v >>= C.emin of
      Nothing -> flunk $ "could not set Emin: " <> val
      Just p -> do
        tell $ "setting Emin to " <> val
        return . C.setEmin $ p

  | k == "extended" = case readNumber v >>= parseBool of
      Nothing -> flunk $ "could not set extended: " <> val
      Just p -> do
        tell $ "setting extended to " <> BS8.pack (show p)
        return . C.setExtended $ p

  | k == "clamp" = case readNumber v >>= parseBool of
      Nothing -> flunk $ "could not set clamp: " <> val
      Just p -> do
        tell $ "setting clamp to " <> BS8.pack (show p)
        return . C.setClamp $ p

  | k == "version" = do
      tell $ "testcases version: " <> val
      return (return ())

  | otherwise = flunk $ "unrecognized directive: " <> val

  where
    k = map toLower . BS8.unpack $ kw
    v = map toLower . BS8.unpack $ val

parseBool :: Int -> Maybe Bool
parseBool i
  | i == 0 = Just False
  | i == 1 = Just True
  | otherwise = Nothing


allRounds :: IsString a => [(a, C.Round)]
allRounds =
  [ ("ceiling", C.roundCeiling)
  , ("down", C.roundDown)
  , ("floor", C.roundFloor)
  , ("half_down", C.roundHalfDown)
  , ("half_even", C.roundHalfEven)
  , ("up", C.roundUp)
  , ("05up", C.round05Up)
  ]

safeRead :: Read a => String -> Maybe a
safeRead a = case reads a of
  (x, ""):[] -> Just x
  _ -> Nothing

readNumber :: Read a => String -> Maybe a
readNumber a = case a of
  [] -> Nothing
  x:xs -> if x == '+' then safeRead xs else safeRead (x:xs)
