{-# LANGUAGE OverloadedStrings #-}

module NumTests where

import Arity
import Types
import Specials
import Deka.Dec
import qualified Data.ByteString.Char8 as BS8
import Prelude(Monad(..), Functor(..))

testLookups :: [(BS8.ByteString, Test)]
testLookups =
  [ ("abs", unary abs)
  , ("add", binary add)
  , ("and", binary and)
  , ("apply", apply)
  -- skip: canonical
  , ("class", decClass)
  , ("compare", binary compare)
  , ("comparesig", binary compareSignal)
  , ("comparetotal", binary compareTotal)
  , ("comparetotalmag", binary compareTotalMag)
  -- skip: copy, copyabs, copynegate
  , ("copysign", binary (fmap (fmap return) copySign))
  , ("divide", binary divide)
  , ("divideint", binary divideInteger)
  , ("exp", unary exp)
  , ("fma", ternary fma)
  , ("invert", unary invert)
  , ("ln", unary ln)
  , ("log10", unary log10)
  , ("logb", unary logB)
  , ("max", binary max)
  , ("min", binary min)
  , ("maxmag", binary maxMag)
  , ("minmag", binary minMag)
  , ("minus", unary minus)
  , ("multiply", binary multiply)
  , ("nextminus", unary nextMinus)
  , ("nextplus", unary nextPlus)
  , ("nexttoward", binary nextToward)
  , ("or", binary or)
  , ("plus", unary plus)
  , ("power", binary power)
  , ("quantize", binary quantize)
  , ("reduce", unary reduce)
  , ("remainder", binary remainder)
  , ("remaindernear", binary remainderNear)
  , ("rescale", binary rescale)
  , ("rotate", binary rotate)
  , ("samequantum", binary (fmap (fmap return) sameQuantum))
  , ("scaleb", binary scaleB)
  , ("shift", binary shift)
  , ("squareroot", unary squareRoot)
  , ("subtract", binary subtract)
  , ("toEng", toEng)
  , ("tointegral", unary toIntegralValue)
  , ("toIntegralx", unary toIntegralExact)
  , ("toSci", toSci)
  , ("trim", unary (fmap return trim))
  , ("xor", binary xor)
  ]
