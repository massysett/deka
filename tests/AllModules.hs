{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Lists all modules in the tests directory.  A hack to make it
-- easy to compile all test modules.  By importing this file in the
-- main immutability.hs, all modules get compiled.
module AllModules where

import Deka.Decoded.Generators
import Deka.Internal.Context.Generators
--import Deka.Internal.Dec.Decoding.Generators
import Util
