{-# LANGUAGE NoImplicitPrelude #-}
-- | Provides facilities to map test string names to functions.

module Dectest.Lookup where

import qualified Data.ByteString.Char8 as BS8
import qualified Dectest.Apply.Types as Y
import Deka.DecNum
import Deka.Fixed.Single
import Deka.Fixed.Double
import Deka.Fixed.Quad
import qualified Dectest.Lookup.Dec as N
import qualified Dectest.Lookup.Single as S
import qualified Dectest.Lookup.Double as D
import qualified Dectest.Lookup.Quad as Q

class Lookups a where
  lookups :: [(BS8.ByteString, Y.ApplyTest a)]

instance Lookups DecNum where
  lookups = N.testLookups

instance Lookups Single where
  lookups = S.testLookups

instance Lookups Double where
  lookups = D.testLookups

instance Lookups Quad where
  lookups = Q.testLookups
