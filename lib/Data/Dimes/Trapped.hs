{-# LANGUAGE Safe #-}
module Data.Dimes.Trapped
  ( Error(..)
  , TrapFn
  , Trap
  , unTrap
  , getSignals
  , setSignals
  , modifySignals
  , mapTrap
  , getContext
  , setContext
  , getPrecision
  , setPrecision
  ) where

import Control.Monad
import Control.Applicative
import qualified Data.Dimes.Safe as S
import Bindings.Mpdecimal
import Foreign.Safe

data Error
  = EString String
  | EGetContext
  | ESetContext S.Context
    -- ^ Setting new context triggered failure
  | EGetPrecision
  | ESetPrecision S.Precision

type TrapFn a
  = Ptr C'mpd_context_t
  -> S.Signals
  -> IO (S.Signals, Either Error a)

newtype Trap a = Trap { unTrap :: TrapFn a }

instance Functor Trap where
  fmap = liftM

instance Applicative Trap where
  pure = return
  (<*>) = ap

instance Monad Trap where
  return a = Trap $ \_ s -> return (s, return a)
  Trap f1 >>= f2 = Trap $ \c s -> do
    e1 <- f1 c s
    case snd e1 of
      Left err -> return (fst e1, Left err)
      Right g -> ($ (fst e1)) . ($ c) . unTrap . f2 $ g
  fail e = Trap $ \_ s -> return (s, Left (EString e))

getSignals :: Trap S.Signals
getSignals = Trap $ \_ s -> return (s, return s)

setSignals :: S.Signals ->  Trap ()
setSignals s = Trap $ \_ _ -> return (s, return ())

modifySignals :: (S.Signals -> S.Signals) -> Trap ()
modifySignals f = Trap $ \_ s -> return (f s, return ())

liftToTrap :: Error -> S.Env a -> Trap a
liftToTrap err e = Trap $ \c s -> do
  r <- ($ c) . S.unEnv $ e
  stat <- S.unEnv S.getStatus c
  let st = S.unStatus stat
  if st `S.hasSignal` s
    then return (s, Left err)
    else return (s, Right r)

mapTrap
  :: ((S.Signals, Either Error a) -> (S.Signals, Either Error b))
  -> Trap a
  -> Trap b
mapTrap f t = Trap $ \p s -> do
  r <- unTrap t p s
  return $ f r

-- # Context
getContext :: Trap S.Context
getContext = liftToTrap EGetContext S.getContext 

setContext :: S.Context -> Trap ()
setContext c = liftToTrap (ESetContext c) (S.setContext c)

getPrecision :: Trap S.Precision
getPrecision = liftToTrap EGetPrecision S.getPrecision

setPrecision :: S.Precision -> Trap ()
setPrecision p = liftToTrap (ESetPrecision p) (S.setPrecision p)
