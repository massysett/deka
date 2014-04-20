module Deka.Context.Internal where

import Deka.Decnumber.Context

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

-- # Rounding

newtype Round = Round { _unRound :: C'rounding }
  deriving (Eq, Ord)

-- | Round toward positive infinity.
roundCeiling :: Round
roundCeiling = Round c'DEC_ROUND_CEILING

-- | Round away from zero.
roundUp :: Round
roundUp = Round c'DEC_ROUND_UP

-- | @0.5@ rounds up
roundHalfUp :: Round
roundHalfUp = Round c'DEC_ROUND_HALF_UP

-- | @0.5@ rounds to nearest even
roundHalfEven :: Round
roundHalfEven = Round c'DEC_ROUND_HALF_EVEN

-- | @0.5@ rounds down
roundHalfDown :: Round
roundHalfDown = Round c'DEC_ROUND_HALF_DOWN

-- | Round toward zero - truncate
roundDown :: Round
roundDown = Round c'DEC_ROUND_DOWN

-- | Round toward negative infinity.
roundFloor :: Round
roundFloor = Round c'DEC_ROUND_FLOOR

-- | Round for reround
round05Up :: Round
round05Up = Round c'DEC_ROUND_05UP

instance Show Round where
  show r
    | r == roundCeiling = "ceiling"
    | r == roundUp = "up"
    | r == roundHalfUp = "half up"
    | r == roundHalfEven = "half even"
    | r == roundHalfDown = "half down"
    | r == roundDown = "down"
    | r == roundFloor = "floor"
    | r == round05Up = "05up"
    | otherwise = error "show: unknown rounding value"

getRound :: Ctx Round
getRound = Ctx $ fmap Round . peek . p'decContext'round

setRound :: Round -> Ctx ()
setRound (Round r) = Ctx $ \ptr -> poke (p'decContext'round ptr) r

