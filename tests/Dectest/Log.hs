module Dectest.Log where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad
import qualified Data.Sequence as Seq
import Data.Monoid
import Control.Applicative
import Control.Monad.Trans.Class

newtype Log m a = Log { runLog :: m (a, Seq.Seq BS8.ByteString) }

tell :: Monad m => BS8.ByteString -> Log m ()
tell b = Log (return ((), Seq.singleton b))

instance Monad m => Monad (Log m) where
  return a = Log (return (a, Seq.empty))
  Log k >>= f = Log $ do
    (a, bs1) <- k
    let Log k' = f a
    (a', bs2) <- k'
    return (a', bs1 <> bs2)

instance Monad m => Functor (Log m) where
  fmap = liftM

instance Monad m => Applicative (Log m) where
  pure = return
  (<*>) = ap

instance MonadTrans Log where
  lift k = Log (liftM (\a -> (a, Seq.empty)) k)
