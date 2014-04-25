module Deka.Internal.Context.Generators where

import Test.QuickCheck
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.Types
import Foreign.Safe

nine9 :: C'int32_t
nine9 = 999999999

context :: Gen (Ptr C'decContext -> IO ())
context = do
  digits <- choose (1, nine9)
  emax <- choose (0, nine9)
  emin <- choose (negate nine9, 0)
  rnd <- elements [ c'DEC_ROUND_CEILING, c'DEC_ROUND_UP,
    c'DEC_ROUND_HALF_UP, c'DEC_ROUND_HALF_DOWN,
    c'DEC_ROUND_DOWN, c'DEC_ROUND_FLOOR, c'DEC_ROUND_05UP,
    c'DEC_ROUND_MAX ]
  trps <-
