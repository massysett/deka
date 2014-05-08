{-# LANGUAGE OverloadedStrings #-}
module Dectest.Lookup.Quad where

import Deka.Fixed.Quad
import Prelude (Maybe(..), Functor(..), Monad(..))
import Dectest.Lookup.Types
import qualified Dectest.Lookup.Util as U
import Dectest.Interp.Octothorpe (WhichPrecision(..))
import qualified Data.ByteString as BS8
import qualified Dectest.Apply.Types as Y

functions :: [Record Quad]
functions =
  [ Record "fromByteString" Nothing (StrToType fromByteString)
  , Record "toByteString" Nothing (TypeToStr toByteString)
  , Record "toEngByteString" Nothing (TypeToStr toEngByteString)
  , Record "fromInt32" Nothing (FromInt fromInt32)
  , Record "fromUInt32" Nothing (FromUInt fromUInt32)
  , Record "toInt32" Nothing (Rounder toInt32)
  , Record "toInt32Exact" Nothing (Rounder toInt32Exact)
  , Record "toUInt32" Nothing (URounder toUInt32)
  , Record "toUInt32Exact" Nothing (URounder toUInt32Exact)
  , binary "add" add
  , binary "subtract" subtract
  , binary "multiply" multiply
  , ternary "fma" fma
  , binary "divide" divide
  , Record "divideInteger" (Just "divideint") (Binary divideInteger)
  , Record "remainderNear" (Just "remaindernear") (Binary remainderNear)
  , binary "quantize" quantize
  , unary "reduce" reduce
  , binary "compare" compare
  , Record "compareOrd" Nothing (MaybeOrd compareOrd)
  , Record "compareSignal" (Just "comparesig") (Binary compareSignal)
  , Record "compareTotal" Nothing (Comparer compareTotal)
  , Record "compareTotalMag" Nothing (Comparer compareTotalMag)
  , binary "max" max
  , binary "maxMag" maxMag
  , binary "min" min
  , binary "minMag" minMag
  , Record "sameQuantum" (Just "samequantum") (BinaryBool sameQuantum)
  , Record "decClass" (Just "class") (Classifier decClass)
  , pdct "isFinite" isFinite
  , pdct "isInfinite" isInfinite
  , pdct "isInteger" isInteger
  , pdct "isLogical" isLogical
  , pdct "isNaN" isNaN
  , pdct "isNegative" isNegative
  , pdct "isNormal" isNormal
  , pdct "isPositive" isPositive
  , pdct "isSignaling" isSignaling
  , pdct "isSigned" isSigned
  , pdct "isSubnormal" isSubnormal
  , pdct "isZero" isZero
  , unary "plus" plus
  , unary "minus" minus
  , unary "abs" abs
  , Record "copySign" (Just "copysign") (BinaryCF copySign)
  , unary "nextMinus" nextMinus
  , unary "nextPlus" nextPlus
  , binary "nextToward" nextToward
  , binary "and" and
  , binary "or" or
  , binary "xor" xor
  , unary "invert" invert
  , binary "shift" shift
  , binary "rotate" rotate
  , unary "logB" logB
  , binary "scaleB" scaleB
  , Record "digits" (Just "digits") (UnaryInt digits)
  , Record "toIntegralExact" (Just "tointegralx") (Unary toIntegralExact)
  , Record "toIntegralValue" (Just "tointegral")
      (RoundSameType toIntegralValue)
  ]

-- use DoNotRound for everything except toSci, toEng, or apply; for
-- those three, use FromCtx.
testLookups :: [(BS8.ByteString, Y.ApplyTest Quad)]
testLookups =
  [ ("abs", U.unary DoNotRound abs)
  , ("add", U.binary DoNotRound add)
  , ("and", U.binary DoNotRound and)
  , ("apply", U.unary FromCtx plus)
  -- skip: canonical
  , ("class", U.unary DoNotRound (fmap return decClass))
  , ("compare", U.binary DoNotRound compare)
  , ("comparesig", U.binary DoNotRound compareSignal)
  , ("comparetotal", U.binary DoNotRound
      (fmap (fmap return) compareTotal))
  , ("comparetotalmag", U.binary DoNotRound
      (fmap (fmap return) compareTotalMag))
  -- skip: copy, copyabs, copynegate
  , ("copysign", U.binary DoNotRound (fmap (fmap return) copySign))
  , ("divide", U.binary DoNotRound divide)
  , ("divideint", U.binary DoNotRound divideInteger)
  -- skip: exp
  , ("fma", U.ternary DoNotRound fma)
  , ("invert", U.unary DoNotRound invert)
  -- skipped: ln, log10
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
  -- skipped: power
  , ("quantize", U.binary DoNotRound quantize)
  , ("reduce", U.unary DoNotRound reduce)
  , ("remainder", U.binary DoNotRound remainder)
  , ("remaindernear", U.binary DoNotRound remainderNear)
  -- skipped: rescale
  , ("rotate", U.binary DoNotRound rotate)
  , ("samequantum", U.binary DoNotRound (fmap (fmap return) sameQuantum))
  , ("scaleb", U.binary DoNotRound scaleB)
  , ("shift", U.binary DoNotRound shift)
  -- skipped: squareroot
  , ("subtract", U.binary DoNotRound subtract)
  , ("toEng", U.unaryStr toEngByteString)
  , ("tointegral", U.testIntegralValue toIntegralValue)
  , ("toIntegralx", U.unary DoNotRound toIntegralExact)
  , ("toSci", U.unaryStr toByteString)
  -- skipped: trim
  , ("xor", U.binary DoNotRound xor)
  ]
