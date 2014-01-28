-- | Examples for the Deka library.
--
-- The examples are not visible from the Haddock documentation.
-- Instead, you want to view the source code.  You can do this
-- straight from Haddock docs themselves if you build the docs with
--
-- > cabal haddock --hyperlink-source
--
-- By putting the examples in the source code for this module, the
-- compiler will complain if I make a mistake in the examples.

module Data.Deka.Examples where

-- For very simple arithmetic, just import Data.Deka.  It contains a
-- Deka type, which is an instance of Num.

-- For work that goes beyond very simple arithmetic, you will
-- typically import Data.Deka.Pure.  This allows you to run all the
-- code in pure functions.  "Under the covers" things happen in the
-- IO monad; however, it is safe to do this work in pure code
-- because there are no visible side effects.  However, all the Safe
-- code (that is, Safe in a Safe Haskell sense) is included in
-- Data.Deka.IO, so you can import that if you wish.
--
-- Usually you will want to perform a qualified import, because
-- Data.Deka.Pure exports a lot of functions that clash with Prelude
-- names.

import Data.Deka
import Data.Deka.Pure as D

-- | A Quad is the number type. 
