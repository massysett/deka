module Util where

import Test.QuickCheck hiding (maxSize)

maxSize :: Int -> Gen a -> Gen a
maxSize s g = sized $ \o -> resize (min o s) g

