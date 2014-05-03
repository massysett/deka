module Dectest.Interp.Directive where

import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Context as C
import Data.Char
import Dectest.Parse
import Data.Maybe

-- | The first four are mandatory
data Directive
  = Precision C.Precision
  | Round C.Round
  | Emax C.Emax
  | Emin C.Emin
  | Extended Bool
  | Clamp Bool
  | Dectest BS8.ByteString
  | Other BS8.ByteString BS8.ByteString
  deriving Show

interpDirective :: Keyword -> Value -> Directive
interpDirective (Keyword key) (Value val)
  | k == "precision" = Precision
    . fromMaybe (error $ "precision out of range: " ++ v)
    . C.precision
    . fromMaybe (error $ "failed to read precision: " ++ v)
    . safeRead $ v

  | k == "rounding" = case lookup v rounds of
      Nothing -> error $ "unrecognized rounding: " ++ v
      Just r -> Round r

  | k == "maxexponent" = Emax
      . fromMaybe (error $ "emax out of range: " ++ v)
      . C.emax
      . fromMaybe (error $ "failed to read emax: " ++ v)
      . safeRead $ v

  | k == "minexponent" = Emin
      . fromMaybe (error $ "emin out of range: " ++ v)
      . C.emin
      . fromMaybe (error $ "failed to read emin: " ++ v)
      . safeRead $ v

  | k == "extended" = case v of
      "0" -> Extended False
      "1" -> Extended True
      _ -> error $ "extended not recognized: " ++ v

  | k == "clamp" = case v of
      "0" -> Clamp False
      "1" -> Clamp True
      _ -> error $ "clamp not recognized: " ++ v

  -- For Dectest and Other store original value, not lowercase one
  | k == "dectest" = Dectest val 
  | otherwise = Other key val


  where
    k = map toLower . BS8.unpack $ key
    v = map toLower . BS8.unpack $ val

data Applied
  = ModCtx (C.Ctx ())
  | ReadDectest BS8.ByteString
  | Unrecognized BS8.ByteString BS8.ByteString

applyDirective :: Directive -> Applied
applyDirective d = case d of
  Precision p -> ModCtx $ C.setPrecision p
  Round r -> ModCtx $ C.setRound r
  Emax e -> ModCtx $ C.setEmax e
  Emin e -> ModCtx $ C.setEmin e
  Extended b -> ModCtx $ C.setExtended b
  Clamp b -> ModCtx $ C.setClamp b
  Dectest s -> ReadDectest s
  Other a b -> Unrecognized a b

rounds :: [(String, C.Round)]
rounds =
  [ ("ceiling", C.roundCeiling)
  , ("down", C.roundDown)
  , ("floor", C.roundFloor)
  , ("half_down", C.roundHalfDown)
  , ("half_even", C.roundHalfEven)
  , ("half_up", C.roundHalfUp)
  , ("up", C.roundUp)
  , ("05up", C.round05Up)
  ]



safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  (a, ""):[] -> Just a
  _ -> Nothing
