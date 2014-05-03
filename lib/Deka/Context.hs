{-# LANGUAGE Trustworthy #-}
module Deka.Context
  ( 
    -- * Ctx
    Ctx

    -- * Flags
  , Flag
  , allFlags
  , conversionSyntax
  , divisionByZero
  , divisionImpossible
  , divisionUndefined
  , insufficientStorage
  , inexact
  , invalidContext
  , invalidOperation
  , lostDigits
  , overflow
  , clamped
  , rounded
  , subnormal
  , underflow

  -- * Traps
  -- ** Set
  , setTraps
  , setTrap
  , clearTraps

  -- ** Query
  , isTrapSet
  , getTraps

  -- * Status
  -- ** Set
  , clearStatus
  
  -- ** Query
  , isStatusSet
  , getStatus

  -- * Digits
  , Precision
  , precision
  , unPrecision
  , setPrecision
  , getPrecision

  -- * Rounding
  -- ** Rounding types
  , Round
  , roundCeiling
  , roundUp
  , roundHalfUp
  , roundHalfEven
  , roundHalfDown
  , roundDown
  , roundFloor
  , round05Up

  -- ** Getting and setting
  , getRound
  , setRound

  -- * Emax and Emin
  , getEmax
  , setEmax
  , getEmin
  , setEmin

  -- * Clamp and extended
  , getClamp
  , setClamp
  , getExtended
  , setExtended

  -- * Initializers
  , Initializer
  , initBase
  , initSingle
  , initDouble
  , initQuad

  -- * Running a Ctx
  , runCtx
  , runCtxStatus
  , local

  ) where

import Deka.Internal.Context hiding (runCtx, runCtxStatus)
import qualified Deka.Internal.Context as I
import Deka.Internal.Unsafe

runCtx :: Initializer -> Ctx a -> a
runCtx = unsafe2 I.runCtx

runCtxStatus :: Initializer -> Ctx a -> (a, [Flag])
runCtxStatus = unsafe2 I.runCtxStatus
