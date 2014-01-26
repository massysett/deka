{-# LANGUAGE Trustworthy #-}

module Data.Deka.Pure
  ( runEnv
  , evalEnv
  , module Data.Deka.IO
  ) where

import Data.Deka.IO
import System.IO.Unsafe

runEnv :: Env a -> (a, Flags)
runEnv e = unsafePerformIO $ runEnvIO e

evalEnv :: Env a -> a
evalEnv = fst . runEnv
