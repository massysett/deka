module Deka.Internal.Decnumber.DecNumberLocal where

-- decNumberLocal.h requires decNumber.h to be included first,
-- or compile time errors result
#include <decNumber.h>
#include <decNumberLocal.h>

littleEndian :: Bool
littleEndian
  | litend == (1 :: Int) = True
  | litend == 0 = False
  | otherwise = error "littleEndian: unknown value"
  where
    litend = #const DECLITEND
