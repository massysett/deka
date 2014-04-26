module Dectest.Lookup.Quad where

import Deka.Quad
import qualified Data.ByteString.Char8 as BS8

data Function
  = StrToQuad (BS8.ByteString -> Ctx Quad)
  | QuadToStr (Quad -> BS8.ByteString)
  | FromInt (C'int32_t -> Quad)
  | FromUInt (C'uint32_t -> Quad)
  | Rounder (Round -> Quad -> Ctx C'int32_t)
  | URounder (Round -> Quad -> Ctx C'uint32_t)
  | Unary (Quad -> Ctx Quad)
  | Binary (Quad -> Quad -> Ctx Quad)
  | Ternary (Quad -> Quad -> Quad -> Ctx Quad)
  | MaybeOrd (Quad -> Quad -> Maybe Ordering)
  | Comparer (Quad -> Quad -> Ordering)
  | BinaryBool (Quad -> Quad -> Bool)
  | Classifier (Quad -> Class)
  | Predicate (Quad -> Bool)
  | BinaryCF (Quad -> Quad -> Quad)
  | UnaryInt (Quad -> Int)
  | RoundQuad (Round -> Quad -> Ctx Quad)

data Record = Record
  { recName :: String
  -- ^ The function is known by this name in Deka.Quad

  , recTestName :: Maybe String
  -- ^ The name of the corresponding Decnumber test keyword, if
  -- there is one

  , recFn :: Function
  }

functions :: [Record]
functions =
  [ Record "fromByteString" Nothing (StrToQuad fromByteString)
  , Record "toByteString" Nothing (QuadToStr toByteString)
  , Record "toEngByteString" Nothing (QuadToStr toEngByteString)
  , Record "fromInt32" Nothing (FromInt fromInt32)
  , Record "fromUInt32" Nothing (FromUInt fromUInt32)
  , Record "toInt32" Nothing (Rounder toInt32)
  , Record "toInt32Exact" Nothing (Rounder toInt32Exact)
  , Record "toUInt32" Nothing (URounder toUInt32)
  , Record "toUInt32Exact" Nothing (URounder toUInt32Exact)
  , Record "add" (Just "add") (Binary add)
  ]
