{-# LANGUAGE Trustworthy, DeriveDataTypeable #-}
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

  ) where

import Foreign.Safe
import System.IO.Unsafe (unsafePerformIO)
import Deka.Context.Internal
import Deka.Decnumber.Context
import Deka.Decnumber.Types
import Data.Typeable

newtype Flag = Flag { unFlag :: C'uint32_t }
  deriving (Eq, Ord, Typeable)

instance Show Flag where
  show f
    | f == conversionSyntax = "Conversion syntax"
    | f == divisionByZero = "Division by zero"
    | f == divisionImpossible = "Division impossible"
    | f == insufficientStorage = "Insufficient storage"
    | f == inexact = "Inexact"
    | f == invalidContext = "Invalid context"
    | f == invalidOperation = "Invalid operation"
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

-- | A source string (for instance, in 'fromByteString') contained
-- errors.
conversionSyntax :: Flag
conversionSyntax = Flag c'DEC_Conversion_syntax

-- | A non-zero dividend is divided by zero.  Unlike @0/0@, it has a
-- defined result (a signed Infinity).
divisionByZero :: Flag
divisionByZero = Flag c'DEC_Division_by_zero

-- | Sometimes raised by 'divideInteger' and 'remainder'.
divisionImpossible :: Flag
divisionImpossible = Flag c'DEC_Division_impossible

-- | @0/0@ is undefined.  It sets this flag and returns a quiet NaN.
divisionUndefined :: Flag
divisionUndefined = Flag c'DEC_Division_undefined

insufficientStorage :: Flag
insufficientStorage = Flag c'DEC_Insufficient_storage

-- | One or more non-zero coefficient digits were discarded during
-- rounding.
inexact :: Flag
inexact = Flag c'DEC_Inexact

invalidContext :: Flag
invalidContext = Flag c'DEC_Invalid_context

-- | Raised on a variety of invalid operations, such as an attempt
-- to use 'compareSignal' on an operand that is an NaN.
invalidOperation :: Flag
invalidOperation = Flag c'DEC_Invalid_operation

lostDigits :: Flag
lostDigits = Flag c'DEC_Lost_digits

-- | The exponent of a result is too large to be represented.
overflow :: Flag
overflow = Flag c'DEC_Overflow

clamped :: Flag
clamped = Flag c'DEC_Overflow

rounded :: Flag
rounded = Flag c'DEC_Clamped

subnormal :: Flag
subnormal = Flag c'DEC_Subnormal

-- | A result is both subnormal and inexact.
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

newtype Precision = Precision { unPrecision :: C'int32_t }
  deriving (Eq, Ord, Show)

precision :: C'int32_t -> Maybe Precision
precision i
  | i < c'DEC_MIN_DIGITS = Nothing
  | i > c'DEC_MAX_DIGITS = Nothing
  | otherwise = Just . Precision $ i

setPrecision :: Precision -> Ctx ()
setPrecision (Precision d) = Ctx $ \ptr ->
  poke (p'decContext'digits ptr) d

getPrecision :: Ctx Precision
getPrecision = Ctx $ fmap Precision . peek . p'decContext'digits

-- # Emax, Emin

getEmax :: Ctx Int
getEmax = Ctx $ fmap fromIntegral . peek . p'decContext'emax

setEmax :: Int -> Ctx Bool
setEmax i = Ctx f
  where
    f ptr
      | i < c'DEC_MIN_EMAX = return False
      | i > c'DEC_MAX_EMAX = return False
      | otherwise = poke (p'decContext'emax ptr) (fromIntegral i)
          >> return True

getEmin :: Ctx Int
getEmin = Ctx $ fmap fromIntegral . peek . p'decContext'emin

setEmin :: Int -> Ctx Bool
setEmin i = Ctx f
  where
    f ptr
      | i < c'DEC_MIN_EMIN = return False
      | i > c'DEC_MAX_EMIN = return False
      | otherwise = poke (p'decContext'emin ptr) (fromIntegral i)
          >> return True

-- # Clamp and extended

getClamp :: Ctx Bool
getClamp = Ctx $ fmap (/= 0) . peek . p'decContext'clamp

setClamp :: Bool -> Ctx ()
setClamp b = Ctx f
  where
    f ptr = poke (p'decContext'clamp ptr) v
    v = if b then 1 else 0

getExtended :: Ctx Bool
getExtended = Ctx $ fmap (/= 0) . peek . p'decContext'extended

setExtended :: Bool -> Ctx ()
setExtended b = Ctx f
  where
    f ptr = poke (p'decContext'extended ptr) v
    v = if b then 1 else 0

-- # Initializers

newtype Initializer = Initializer { _unInitializer :: C'int32_t }
  deriving (Eq, Ord)

initBase :: Initializer
initBase = Initializer c'DEC_INIT_BASE

initDecimal32 :: Initializer
initDecimal32 = Initializer c'DEC_INIT_DECIMAL32

initDecimal64 :: Initializer
initDecimal64 = Initializer c'DEC_INIT_DECIMAL64

initDecimal128 :: Initializer
initDecimal128 = Initializer c'DEC_INIT_DECIMAL128

instance Show Initializer where
  show i
    | i == initBase = "base"
    | i == initDecimal32 = "decimal32"
    | i == initDecimal64 = "decimal64"
    | i == initDecimal128 = "decimal128"
    | otherwise = error "show initializer: unknown value"

-- # Run

runCtx :: Initializer -> Ctx a -> a
runCtx (Initializer i) (Ctx f) = unsafePerformIO $ do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \ptr -> do
    _ <- c'decContextDefault ptr i
    f ptr

