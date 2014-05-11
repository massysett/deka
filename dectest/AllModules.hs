{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Lists all modules in the tests directory.  A hack to make it
-- easy to compile all test modules.  By importing this file in the
-- main immutability.hs, all modules get compiled.
module AllModules where

import Parse
import Parse.Tokenizer
import Parse.Tokens
import Types
import Runner
import Queue
import TestLog
import Util
