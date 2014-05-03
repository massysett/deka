module Dectest.Lookup.Types where

import qualified Data.ByteString.Char8 as BS8
import Deka.Context
import Data.Int
import Data.Word
import Deka.Class

data Function a
  = StrToType (BS8.ByteString -> Ctx a)
  | TypeToStr (a -> BS8.ByteString)
  | FromInt (Int32 -> a)
  | FromUInt (Word32 -> a)
  | Rounder (Round -> a -> Ctx Int32)
  | URounder (Round -> a -> Ctx Word32)
  | Unary (a -> Ctx a)
  | Binary (a -> a -> Ctx a)
  | Ternary (a -> a -> a -> Ctx a)
  | MaybeOrd (a -> a -> Maybe Ordering)
  | Comparer (a -> a -> Ordering)
  | BinaryBool (a -> a -> Bool)
  | Classifier (a -> Class)
  | Predicate (a -> Bool)
  | BinaryCF (a -> a -> a)
  | UnaryInt (a -> Int)
  | RoundSameType (Round -> a -> Ctx a)

data Record a = Record
  { recName :: String
  -- ^ The function is known by this name in the module
  -- corresponding to the data type (e.g.
  -- Data.Fixed.Quad.fromByteString)

  , recTestName :: Maybe String
  -- ^ The name of the corresponding Decnumber test keyword, if
  -- there is one

  , recFn :: Function a
  }

unary :: String -> (a -> Ctx a) -> Record a
unary s f = Record s (Just s) (Unary f)

binary :: String -> (a -> a -> Ctx a) -> Record a
binary s f = Record s (Just s) (Binary f)

ternary :: String -> (a -> a -> a -> Ctx a) -> Record a
ternary s f = Record s (Just s) (Ternary f)

pdct :: String -> (a -> Bool) -> Record a
pdct s f = Record s Nothing (Predicate f)

