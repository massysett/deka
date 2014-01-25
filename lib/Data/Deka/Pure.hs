{-# LANGUAGE Trustworthy #-}

module Data.Deka.Pure
  ( runEnvPure
  , module Data.Deka.Env
  ) where

import Data.Deka.Env
import System.IO.Unsafe

runEnvPure :: Env a -> (a, Flags)
runEnvPure e = unsafePerformIO $ runEnv e
