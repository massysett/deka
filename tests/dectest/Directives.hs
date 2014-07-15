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
import qualified Data.Sequence as S
import Data.Sequence(ViewR(..))

data Directives = Directives
  { dsPrecision :: Maybe P.Value
  , dsRounding :: Maybe P.Value
  , dsEmax :: Maybe P.Value
  , dsEmin :: Maybe P.Value
  , dsExtended :: Maybe P.Value
  , dsClamp :: Maybe P.Value
  , dsVersion :: Maybe P.Value
  } deriving Show

type Getter = Directives -> Maybe P.Value
type Setter = P.Value -> Directives -> Directives

dirFields
  :: IsString a
  => [(a, (Getter, Setter))]
dirFields =
  [ ("precision", (dsPrecision, \v d -> d { dsPrecision = Just v }))
  , ("rounding", (dsRounding, \v d -> d { dsRounding = Just v }))
  , ("maxexponent", (dsEmax, \v d -> d { dsEmax = Just v }))
  , ("minexponent", (dsEmin, \v d -> d { dsEmin = Just v }))
  , ("extended", (dsExtended, \v d -> d { dsExtended = Just v }))
  , ("clamp", (dsClamp, \v d -> d { dsClamp = Just v }))
  , ("version", (dsVersion, \v d -> d { dsVersion = Just v }))
  ]

emptyDirectives :: Directives
emptyDirectives = Directives Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing

plusDirective
  :: (P.Keyword, P.Value)
  -> Directives
  -> TestLog Directives
plusDirective (kw, vl) d = case lookup k dirFields of
  Nothing -> flunk $ "could not find directive: " <> BS8.pack k
  Just p -> add p
  where
    (k, v) = (map toLower . BS8.unpack . P.unKeyword $ kw,
              P.unValue vl)
    add (get, set) = case get d of
      Nothing -> do
        tell $ "using directive " <> BS8.pack k <> " with value " <> v
        return $ set vl d
      Just _ -> do
        tell $ "ignoring old directive " <> BS8.pack k
          <> " with value " <> v
        return d

plusDirectives
  :: S.Seq (P.Keyword, P.Value)
  -> TestLog Directives
plusDirectives = go emptyDirectives
  where
    go ds sq = case S.viewr sq of
      EmptyR -> return ds
      sq' :> p -> do
        ds' <- plusDirective p ds
        go ds' sq'

directivesToCtx :: Directives -> TestLog (C.Ctx ())
directivesToCtx ds = do

  rnd <- case dsRounding ds of
    Nothing -> return (return ())
    Just (P.Value r) -> do
      let dflt = flunk $ "could not set rounding: " <> r
          mayRnd = lookup r allRounds
      rnd <- maybe dflt return mayRnd
      tell $ "setting rounding to " <> r
      return (C.setRound rnd)

  clmp <- case dsClamp ds of
    Nothing -> return (return ())
    Just (P.Value r) -> case readNumberBS r >>= parseBool of
      Nothing -> flunk $ "could not set clamp: " <> r
      Just p -> do
        tell $ "setting clamp to " <> BS8.pack (show p)
        return (C.setClamp p)

  tri <- do
    pcsn <- case dsPrecision ds of
      Nothing -> flunk "precision not present in directives"
      Just (P.Value r) -> case readNumberBS r >>= C.precision of
        Nothing -> flunk $ "could not set precision: " <> r
        Just p -> do
          tell $ "using for precision: " <> r
          return p

    emx <- case dsEmax ds of
      Nothing -> flunk "emax not present in directives"
      Just (P.Value r) -> case readNumberBS r >>= C.emax of
        Nothing -> flunk $ "could not set emax: " <> r
        Just x -> do
          tell $ "using for Emax: " <> r
          return x

    emn <- case dsEmin ds of
      Nothing -> flunk "emin not present in directives"
      Just (P.Value r) -> case readNumberBS r >>= C.emin of
        Nothing -> flunk $ "could not set emin: " <> r
        Just x -> do
          tell $ "using for Emin: " <> r
          return x

    case C.trio pcsn emx emn of
      Nothing -> flunk $ "failed to set trio"
      Just t -> return $ C.setTrio t

  return $ rnd >> clmp >> tri

parseDirectives
  :: S.Seq (P.Keyword, P.Value)
  -> TestLog (C.Ctx ())
parseDirectives sq = plusDirectives sq >>= directivesToCtx

allRounds :: IsString a => [(a, C.Round)]
allRounds =
  [ ("ceiling", C.roundCeiling)
  , ("up", C.roundUp)
  , ("half_up", C.roundHalfUp)
  , ("half_even", C.roundHalfEven)
  , ("half_down", C.roundHalfDown)
  , ("down", C.roundDown)
  , ("floor", C.roundFloor)
  , ("05up", C.round05Up)
  ]

