{-# LANGUAGE OverloadedStrings #-}
module Directives where

import qualified Parse as P
import qualified Deka.Context as C
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid
import TestLog
import Util
import Data.String
import qualified Data.Traversable as Tr
import qualified Data.Sequence as S


parseDirectives
  :: S.Seq (P.Keyword, P.Value)
  -> TestLog (C.Ctx ())
parseDirectives sq = do
  seqCtx <- Tr.mapM (uncurry addDirective) sq
  let ctxOfSeq = Tr.sequence seqCtx
      r = fmap (const ()) ctxOfSeq
      _types = ( seqCtx :: S.Seq (C.Ctx ()),
                 ctxOfSeq :: C.Ctx (S.Seq ()) )
  return r

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

