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
  , setAllTraps
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
  , C'int32_t
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
  , initDecimal32
  , initDecimal64
  , initDecimal128

  -- * Running a Ctx
  , runCtx
  , runCtxStatus

  ) where

import Deka.Context.Internal hiding (runCtx, runCtxStatus)
import qualified Deka.Context.Internal as I
import Deka.Decnumber.Types
import Deka.Unsafe

runCtx :: Initializer -> Ctx a -> a
runCtx = unsafe2 I.runCtx

runCtxStatus :: Initializer -> Ctx a -> (a, [Flag])
runCtxStatus = unsafe2 I.runCtxStatus
