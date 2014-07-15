{-# LANGUAGE Safe #-}
module Deka.Context
  ( 

    -- * Integer type
    Signed

    -- * Ctx
  , Ctx

    -- * Flags
  , Flag
  , Flags
  , allFlag
  , fullFlags
  , emptyFlags
  , packFlags
  , unpackFlags

  -- ** Individual flags
  , clamped
  , conversionSyntax
  , divisionByZero
  , divisionImpossible
  , divisionUndefined
  , fpuError
  , inexact
  , invalidContext
  , invalidOperation
  , mallocError
  , notImplemented
  , overflow
  , rounded
  , subnormal
  , underflow

  -- * Traps
  , getTraps
  , setTraps

  -- * Status
  , getStatus
  , setStatus
  
  -- * Digits
  , Precision
  , precision
  , unPrecision
  , getPrecision
  , setMaxPrecision

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
  , roundTruncate

  -- ** Getting and setting
  , getRound
  , setRound

  -- * Emax and Emin
  -- ** Emax
  , Emax
  , unEmax
  , emax
  , getEmax

  -- ** Emin
  , Emin
  , unEmin
  , emin
  , getEmin

  -- * Trio
  , Trio
  , trioPrecision
  , trioEmax
  , trioEmin
  , trio
  , setTrio
  , getTrio

  -- * Clamp
  , getClamp
  , setClamp

  -- * Correct rounding
  , getAllCorrectRound
  , setAllCorrectRound

  -- * Initializers
  , Initializer(..)
  , initCtx

  -- * Running a Ctx
  , runCtxInit
  , runCtx
  , runCtxStatus
  , local

  ) where

import Deka.Internal.Context
import Deka.Internal.Mpdec (Signed)
