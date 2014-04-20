module Deka.Class.Internal where

import Deka.Decnumber.Context

newtype Class = Class { _unClass :: C'decClass }
  deriving (Eq, Ord)

-- | Signaling NaN
sNaN :: Class
sNaN = Class c'DEC_CLASS_SNAN

-- | Quiet NaN
qNaN :: Class
qNaN = Class c'DEC_CLASS_QNAN

-- | Negative infinity
negInf :: Class
negInf = Class c'DEC_CLASS_NEG_INF

-- | Negative normal number
negNormal :: Class
negNormal = Class c'DEC_CLASS_NEG_NORMAL

-- | Negative subnormal number
negSubnormal :: Class
negSubnormal = Class c'DEC_CLASS_NEG_SUBNORMAL

-- | The negative zero
negZero :: Class
negZero = Class c'DEC_CLASS_NEG_ZERO

-- | The positive zero
posZero :: Class
posZero = Class c'DEC_CLASS_POS_ZERO

-- | A positive subnormal number
posSubnormal :: Class
posSubnormal = Class c'DEC_CLASS_POS_SUBNORMAL

-- | A positive normal number
posNormal :: Class
posNormal = Class c'DEC_CLASS_POS_NORMAL

-- | Positive infinity
posInf :: Class
posInf = Class c'DEC_CLASS_POS_INF

instance Show Class where
  show c
    | c == sNaN = "sNaN"
    | c == qNaN = "qNaN"
    | c == negInf = "negInf"
    | c == negNormal = "negNormal"
    | c == negSubnormal = "negSubnormal"
    | c == negZero = "negZero"
    | c == posZero = "posZero"
    | c == posSubnormal = "posSubnormal"
    | c == posNormal = "posNormal"
    | c == posInf = "posInf"
    | otherwise = error "show: class: unknown class"
