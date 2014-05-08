{-# LANGUAGE OverloadedStrings #-}
module Dectest.Lookup.Dec where

import Deka.DecNum
import Prelude (Functor(..), Monad(..))
import qualified Dectest.Lookup.Util as U
import Dectest.Interp.Octothorpe (WhichPrecision(..))
import qualified Data.ByteString as BS8
import qualified Dectest.Apply.Types as Y

-- use DoNotRound for everything except toSci, toEng, or apply; for
-- those three, use FromCtx.
testLookups :: [(BS8.ByteString, Y.ApplyTest DecNum)]
testLookups =
  [ ("abs", U.unary DoNotRound abs)
  , ("add", U.binary DoNotRound add)
  , ("and", U.binary DoNotRound and)
  , ("apply", U.unary FromCtx plus)
  -- skip: canonical
  , ("class", U.unary DoNotRound numClass)
  , ("compare", U.binary DoNotRound compare)
  , ("comparesig", U.binary DoNotRound compareSignal)
  , ("comparetotal", U.binary DoNotRound compareTotal)
  , ("comparetotalmag", U.binary DoNotRound compareTotalMag)
  -- skip: copy, copyabs, copynegate
  , ("copysign", U.binary DoNotRound (fmap (fmap return) copySign))
  , ("divide", U.binary DoNotRound divide)
  , ("divideint", U.binary DoNotRound divideInteger)
  , ("exp", U.unary DoNotRound exp)
  , ("fma", U.ternary DoNotRound fma)
  , ("invert", U.unary DoNotRound invert)
  , ("ln", U.unary DoNotRound ln)
  , ("log10", U.unary DoNotRound log10)
  , ("logb", U.unary DoNotRound logB)
  , ("max", U.binary DoNotRound max)
  , ("min", U.binary DoNotRound min)
  , ("maxmag", U.binary DoNotRound maxMag)
  , ("minmag", U.binary DoNotRound minMag)
  , ("minus", U.unary DoNotRound minus)
  , ("multiply", U.binary DoNotRound multiply)
  , ("nextminus", U.unary DoNotRound nextMinus)
  , ("nextplus", U.unary DoNotRound nextPlus)
  , ("nexttoward", U.binary DoNotRound nextToward)
  , ("or", U.binary DoNotRound or)
  , ("plus", U.unary DoNotRound plus)
  , ("power", U.binary DoNotRound power)
  , ("quantize", U.binary DoNotRound quantize)
  , ("reduce", U.unary DoNotRound reduce)
  , ("remainder", U.binary DoNotRound remainder)
  , ("remaindernear", U.binary DoNotRound remainderNear)
  , ("rescale", U.binary DoNotRound rescale)
  , ("rotate", U.binary DoNotRound rotate)
  , ("samequantum", U.binary DoNotRound (fmap (fmap return) sameQuantum))
  , ("scaleb", U.binary DoNotRound scaleB)
  , ("shift", U.binary DoNotRound shift)
  , ("squareroot", U.unary DoNotRound squareRoot)
  , ("subtract", U.binary DoNotRound subtract)
  , ("toEng", U.unaryStr toEngByteString)
  , ("tointegral", U.unary DoNotRound toIntegralValue)
  , ("toIntegralx", U.unary DoNotRound toIntegralExact)
  , ("toSci", U.unaryStr toByteString)
  , ("trim", U.unary DoNotRound (fmap return trim))
  , ("xor", U.binary DoNotRound xor)
  ]
