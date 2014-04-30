module Deka.Internal.Fixed.Basic where

import Foreign.Safe
import Foreign.C.Types
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.DecNumber
import Prelude (IO)

class Basic b where
  baseFromString
    :: Ptr b -> Ptr CChar -> Ptr C'decContext -> IO (Ptr b)
  stringSize :: b -> Int
  baseToString
    :: Ptr b -> Ptr CChar -> IO (Ptr CChar)
  baseToEngString
    :: Ptr b -> Ptr CChar -> IO (Ptr CChar)
  baseFromNumber
    :: Ptr b -> Ptr C'decNumber -> Ptr C'decContext -> IO (Ptr b)

  baseToNumber
    :: Ptr b -> Ptr C'decNumber -> IO (Ptr C'decNumber)

  baseCanonical
    :: Ptr b -> Ptr b -> IO (Ptr b)

  baseIsCanonical
    :: Ptr b -> IO Int32

