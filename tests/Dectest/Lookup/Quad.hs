module Dectest.Lookup.Quad where

import Deka.Quad
import qualified Data.ByteString.Char8 as BS8
import Prelude (Maybe(..), String, Ordering, Bool(..), Int)

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

unary :: String -> (Quad -> Ctx Quad) -> Record
unary s f = Record s (Just s) (Unary f)

binary :: String -> (Quad -> Quad -> Ctx Quad) -> Record
binary s f = Record s (Just s) (Binary f)

ternary :: String -> (Quad -> Quad -> Quad -> Ctx Quad) -> Record
ternary s f = Record s (Just s) (Ternary f)

pdct :: String -> (Quad -> Bool) -> Record
pdct s f = Record s Nothing (Predicate f)

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
  , Record "toIntegralValue" (Just "tointegral") (RoundQuad toIntegralValue)
  ]
