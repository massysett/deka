module TestLog
  ( TestLog
  , tell
  , bypass
  , flunk
  , Done
  , pass
  , runTestLog
  ) where

import qualified Data.Sequence as S
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid

data State a
  = Good a
  | Failed
  | Bypass
  deriving Show

newtype TestLog a = TestLog
  { unTestLog :: (S.Seq BS8.ByteString, State a) }
  deriving Show

instance Monad TestLog where
  return a = TestLog (S.empty, Good a)
  (TestLog (ss, st)) >>= f = TestLog $ case st of
    Failed -> (ss, Failed)
    Bypass -> (ss, Bypass)
    Good a -> let (ss', st') = unTestLog $ f a in
      (ss <> ss', st')

instance Applicative TestLog where
  pure = return
  (<*>) = ap

instance Functor TestLog where
  fmap = liftM

tell :: BS8.ByteString -> TestLog ()
tell bs = TestLog (S.singleton bs, Good ())

bypass :: BS8.ByteString -> TestLog a
bypass bs = TestLog (S.singleton bs, Bypass)

flunk :: BS8.ByteString -> TestLog a
flunk bs = TestLog (S.singleton bs, Failed)

data Done = Done
  deriving Show

pass :: BS8.ByteString -> TestLog Done
pass bs = TestLog (S.singleton bs, Good Done)

runTestLog :: TestLog Done -> (Maybe Bool, S.Seq BS8.ByteString)
runTestLog (TestLog (ss, st)) = (r, ss)
  where
    r = case st of
      Good Done -> Just True
      Failed -> Just False
      Bypass -> Nothing

