module Dectest.Lookup.Util where

import Dectest.Interp.Octothorpe (WhichPrecision)
import qualified Deka.Context as C
import Control.Applicative
import Control.Monad (join)

unary
  :: WhichPrecision
  -> (a -> C.Ctx a)
  -> [WhichPrecision -> C.Ctx a]
  -> Maybe (C.Ctx a)
unary wp f ls = case ls of
  x:[] -> Just . join $ f <$> (x wp)
  _ -> Nothing

binary
  :: WhichPrecision
  -> (a -> a -> C.Ctx a)
  -> [WhichPrecision -> C.Ctx a]
  -> Maybe (C.Ctx a)
binary wp f ls = case ls of
  x:y:[] -> Just $ do
    a <- x wp
    b <- y wp
    f a b
  _ -> Nothing

ternary
  :: WhichPrecision
  -> (a -> a -> a -> C.Ctx a)
  -> [WhichPrecision -> C.Ctx a]
  -> Maybe (C.Ctx a)
ternary wp f ls = case ls of
  x:y:z:[] -> Just $ do
    a <- x wp
    b <- y wp
    c <- z wp
    f a b c
  _ -> Nothing
