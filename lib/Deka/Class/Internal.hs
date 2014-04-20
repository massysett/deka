module Deka.Class.Internal where

import Deka.Decnumber.Context

newtype Class = Class { _unClass :: C'decClass }
  deriving (Eq, Ord)

sNaN :: Class
sNaN = Class c'DEC_CLASS_SNAN

qNaN :: Class
qNaN = Class c'DEC_CLASS_QNAN

negInf :: Class
negInf = Class c'DEC_CLASS_NEG_INF

negNormal :: Class
negNormal = Class c'DEC_CLASS_NEG_NORMAL

negSubnormal :: Class
negSubnormal = Class c'DEC_CLASS_NEG_SUBNORMAL

negZero :: Class
negZero = Class c'DEC_CLASS_NEG_ZERO

posZero :: Class
posZero = Class c'DEC_CLASS_POS_ZERO

posSubnormal :: Class
posSubnormal = Class c'DEC_CLASS_POS_SUBNORMAL

posNormal :: Class
posNormal = Class c'DEC_CLASS_POS_NORMAL

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
