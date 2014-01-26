{-# LANGUAGE Trustworthy #-}

module Data.Deka.Pure
  ( runEnvPure
  , evalEnvPure
  , module Data.Deka.Env
  ) where

import Data.Deka.Env
import System.IO.Unsafe

runEnvPure :: Env a -> (a, Flags)
runEnvPure e = unsafePerformIO $ runEnv e

evalEnvPure :: Env a -> a
evalEnvPure = fst . runEnvPure
