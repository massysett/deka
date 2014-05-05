module Dectest.Lookup.Double where

import Deka.Fixed.Double
import Prelude (Maybe(..))
import Dectest.Lookup.Types

functions :: [Record Double]
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