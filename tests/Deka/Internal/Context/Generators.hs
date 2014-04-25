module Deka.Internal.Context.Generators where

import Test.QuickCheck
import Test.QuickCheck.Random
import Test.QuickCheck.Gen
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.Types
import Foreign.Safe
import System.Random.Shuffle
import Deka.Internal.Context

nine9 :: C'int32_t
nine9 = 999999999

getRand :: Gen QCGen
getRand = MkGen $ \g _ -> g

randList :: [a] -> Gen [a]
randList ls = sized $ \s -> do
  n <- choose (0, max (length ls) s)
  rnd <- getRand
  let ls' = shuffle' ls (length ls) rnd
  return $ take n ls'

round :: Gen Round
round = fmap Round $ elements allRounds

context :: Gen (IO (ForeignPtr C'decContext))
context = do
  digits <- choose (1, nine9)
  emax <- choose (0, nine9)
  emin <- choose (negate nine9, 0)
  rnd <- elements allRounds
  trps <- oneof [ return [], randList extFlags ]
  stat <- oneof [ return [], randList extFlags ]
  clmp <- elements [0, 1]
  ext <- elements [0, 1]
  let trps' = foldl (.|.) 0 trps
      stat' = foldl (.|.) 0 stat
  return $ do
    fp <- mallocForeignPtrBytes c'decContext'sizeOf
    withForeignPtr fp $ \ptr -> do
      poke (p'decContext'digits ptr) digits
      poke (p'decContext'emax ptr) emax
      poke (p'decContext'emin ptr) emin
      poke (p'decContext'round ptr) rnd
      poke (p'decContext'traps ptr) trps'
      poke (p'decContext'status ptr) stat'
      poke (p'decContext'clamp ptr) clmp
      poke (p'decContext'extended ptr) ext
    return fp

