{-# LANGUAGE Trustworthy #-}

-- | Pure interface to floating-point decimals.
--
-- Floating point operations are, in fact, performed in the IO monad
-- because they are carried out through the FFI.  However, the
-- functions have no observable side effects, so this module uses
-- 'unsafePerformIO' to allow you to perform the computations in a
-- pure function.  This module is Trustworthy for Safe Haskell
-- purposes.  If you do not Trust me, use "Data.Deka.IO", which is
-- Safe for Safe Haskell purposes.
--
-- This module also re-exports everything in "Data.Deka.IO".
--
-- Many functions in this module clash with Prelude names, so you
-- might want to do
--
-- > import qualified Data.Deka.Pure as D
module Data.Deka.Pure
  ( runCtx
  , evalCtx
  , module Data.Deka.IO
  ) where

import Data.Deka.IO
import System.IO.Unsafe

runCtx :: Ctx a -> (a, Flags)
runCtx e = unsafePerformIO $ runCtxIO e

evalCtx :: Ctx a -> a
evalCtx = fst . runCtx
