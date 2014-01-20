{-# LANGUAGE Trustworthy #-}

module Data.Deka.Pure where

import Data.Deka.Safe
import System.IO.Unsafe

runEnvPure :: Initializer -> Env a -> (a, Context)
runEnvPure i e = unsafePerformIO $ runEnv i e

evalEnvPure :: Initializer -> Env a -> a
evalEnvPure i e = unsafePerformIO $ evalEnv i e
