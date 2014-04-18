module Data.Deka.Context
  ( 

    -- * Types
    C'int32_t

    -- * Ctx
  , Ctx

   -- * Flags
  , Flag
  , allFlags
  , conversionSyntax
  , divisionByZero
  , divisionImpossible
  , divisionUndefined
  , insufficientStorage
  , inexact
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
  , setDigits
  , getDigits
  ) where

import Foreign.Safe
import Data.Deka.Context.Internal
import Data.Deka.Decnumber.Context
import Data.Deka.Decnumber.Types

newtype Flag = Flag { unFlag :: C'uint32_t }
  deriving (Eq, Ord)

instance Show Flag where
  show f
    | f == conversionSyntax = "Conversion syntax"
    | f == divisionByZero = "Division by zero"
    | f == divisionImpossible = "Division impossible"
    | f == insufficientStorage = "Insufficient storage"
    | f == inexact = "Inexact"
    | f == lostDigits = "Lost digits"
    | f == overflow = "Overflow"
    | f == clamped = "Clamped"
    | f == rounded = "Rounded"
    | f == subnormal = "Subnormal"
    | f == underflow = "Underflow"
    | otherwise = error "show flag: unrecognized flag"

allFlags :: [Flag]
allFlags =
  [ conversionSyntax, divisionByZero, divisionImpossible,
    divisionUndefined, insufficientStorage, inexact,
    lostDigits, overflow, clamped, rounded, subnormal,
    underflow ]

whichFlags :: C'uint32_t -> [Flag]
whichFlags i =
  map Flag
  . filter ((/= 0) . (.&. i))
  . map unFlag
  $ allFlags

combineFlags :: [Flag] -> C'uint32_t
combineFlags = foldl f 0
  where
    f a (Flag b) = a .|. b

conversionSyntax :: Flag
conversionSyntax = Flag c'DEC_Conversion_syntax

divisionByZero :: Flag
divisionByZero = Flag c'DEC_Division_by_zero

divisionImpossible :: Flag
divisionImpossible = Flag c'DEC_Division_impossible

divisionUndefined :: Flag
divisionUndefined = Flag c'DEC_Division_undefined

insufficientStorage :: Flag
insufficientStorage = Flag c'DEC_Insufficient_storage

inexact :: Flag
inexact = Flag c'DEC_Inexact

lostDigits :: Flag
lostDigits = Flag c'DEC_Lost_digits

overflow :: Flag
overflow = Flag c'DEC_Overflow

clamped :: Flag
clamped = Flag c'DEC_Overflow

rounded :: Flag
rounded = Flag c'DEC_Clamped

subnormal :: Flag
subnormal = Flag c'DEC_Subnormal

underflow :: Flag
underflow = Flag c'DEC_Underflow

-- # Traps

-- ## Set
setAllTraps :: [Flag] -> Ctx ()
setAllTraps fs = Ctx $ \ptr -> do
  let pTr = p'decContext'traps ptr
  poke pTr (combineFlags fs) 

setTrap :: Flag -> Ctx ()
setTrap f = Ctx $ \ptr -> do
  let pTr = p'decContext'traps ptr
  ts <- peek pTr
  poke pTr (ts .|. unFlag f)

clearTraps :: [Flag] -> Ctx ()
clearTraps fs = Ctx $ \ptr -> do
  let pTr = p'decContext'traps ptr
  ts <- peek pTr
  poke pTr (ts .&. complement (combineFlags fs))

-- ## Query

isTrapSet :: Flag -> Ctx Bool
isTrapSet (Flag f) = Ctx $ \ptr -> do
  let pTr = p'decContext'traps ptr
  ts <- peek pTr
  return $ ts .&. f /= 0

getTraps :: Ctx [Flag]
getTraps = Ctx $ \ptr -> do
  ts <- peek (p'decContext'traps ptr)
  return $ whichFlags ts

-- # Status

-- ## Set

-- No set functions provided - you're not supposed to set traps

clearStatus :: [Flag] -> Ctx ()
clearStatus fs = Ctx $ \ptr -> do
  _ <- c'decContextClearStatus ptr (combineFlags fs) 
  return ()

-- ## Query

isStatusSet :: Flag -> Ctx Bool
isStatusSet (Flag f) = Ctx $ \ptr -> do
  let pSt = p'decContext'status ptr
  ts <- peek pSt
  return $ ts .&. f /= 0

getStatus :: Ctx [Flag]
getStatus = Ctx $ \ptr -> do
  let pSt = p'decContext'status ptr
  ts <- peek pSt
  return $ whichFlags ts

-- # Digits

setDigits :: C'int32_t -> Ctx Bool
setDigits d = Ctx f
  where
    f p | d < c'DEC_MIN_DIGITS = return False
        | d > c'DEC_MAX_DIGITS = return False
        | otherwise = poke (p'decContext'digits p) d
              >> return True

getDigits :: Ctx C'int32_t
getDigits = Ctx $ peek . p'decContext'digits
