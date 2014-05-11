module Queue
  ( Queue
  , newQueue
  , enqueue
  , dequeue
  ) where

import Data.Sequence

newtype Queue a = Queue (Seq a) deriving Show

newQueue :: Queue a
newQueue = Queue empty

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue q) = Queue $ a <| q

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue q) = case viewr q of
  EmptyR -> (Nothing, Queue empty)
  r :> a -> (Just a, Queue r)

sequence :: Monad m => 
