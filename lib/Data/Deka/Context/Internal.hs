module Data.Deka.Context.Internal where

import Data.Deka.Decnumber.Context

import Foreign.Safe
import Control.Applicative
import Control.Monad

-- | The Ctx monad
--
-- The General Decimal Arithmetic specification states that most
-- computations occur within a @context@, which affects the manner
-- in which computations are done (for instance, the context
-- determines the rounding algorithm).  The context also carries
-- the flags that computations can set (for instance, a computation might
-- set a flag to indicate that the result is rounded or inexact or
-- was a division by zero.) The Ctx monad carries this context.
newtype Ctx a = Ctx { unCtx :: Ptr C'decContext -> IO a }

instance Functor Ctx where
  fmap = liftM

instance Applicative Ctx where
  pure = return
  (<*>) = ap

instance Monad Ctx where
  return a = Ctx $ \_ -> return a
  Ctx a >>= f = Ctx $ \p -> do
    r1 <- a p
    let b = unCtx $ f r1
    b p
  fail s = Ctx $ \_ -> fail s

