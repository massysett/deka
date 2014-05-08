{-# LANGUAGE OverloadedStrings #-}
module Dectest.Lookup.Single where

import Deka.Fixed.Single
import qualified Dectest.Lookup.Util as U
import qualified Data.ByteString as BS8
import qualified Dectest.Apply.Types as Y

-- use DoNotRound for everything except toSci, toEng, or apply; for
-- those three, use FromCtx.
testLookups :: [(BS8.ByteString, Y.ApplyTest Single)]
testLookups =
  [ ("toEng", U.unaryStr toEngByteString)
  , ("toSci", U.unaryStr toByteString)
  ]
