{-# LANGUAGE Trustworthy #-}
module Data.Deka.Context
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
  , NumDigits
  , numDigits
  , setNumDigits
  , getNumDigits

  -- * Rounding
  -- ** Rounding types
  , Round
  , roundCeiling
  , roundUp
  , roundHalfUp
  , roundHalfEven
  , roundHalfDown
  , roundFloor
  , round05Up
  , roundMax

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

newtype NumDigits = NumDigits { _unNumDigits :: C'int32_t }
  deriving (Eq, Ord, Show)

numDigits :: Int -> Maybe NumDigits
numDigits i
  | i < c'DEC_MIN_DIGITS = Nothing
  | i > c'DEC_MAX_DIGITS = Nothing
  | otherwise = Just . NumDigits . fromIntegral $ i

setNumDigits :: NumDigits -> Ctx ()
setNumDigits (NumDigits d) = Ctx $ \ptr ->
  poke (p'decContext'digits ptr) d

getNumDigits :: Ctx NumDigits
getNumDigits = Ctx $ fmap NumDigits . peek . p'decContext'digits

-- # Rounding

newtype Round = Round { _unRound :: C'rounding }
  deriving (Eq, Ord)

roundCeiling :: Round
roundCeiling = Round c'DEC_ROUND_CEILING

roundUp :: Round
roundUp = Round c'DEC_ROUND_UP

roundHalfUp :: Round
roundHalfUp = Round c'DEC_ROUND_HALF_UP

roundHalfEven :: Round
roundHalfEven = Round c'DEC_ROUND_HALF_EVEN

roundHalfDown :: Round
roundHalfDown = Round c'DEC_ROUND_HALF_DOWN

roundFloor :: Round
roundFloor = Round c'DEC_ROUND_FLOOR

round05Up :: Round
round05Up = Round c'DEC_ROUND_05UP

roundMax :: Round
roundMax = Round c'DEC_ROUND_MAX

instance Show Round where
  show r
    | r == roundCeiling = "ceiling"
    | r == roundUp = "up"
    | r == roundHalfUp = "half up"
    | r == roundHalfEven = "half even"
    | r == roundHalfDown = "half down"
    | r == roundFloor = "floor"
    | r == round05Up = "05up"
    | r == roundMax = "max"
    | otherwise = error "show: unknown rounding value"

getRound :: Ctx Round
getRound = Ctx $ fmap Round . peek . p'decContext'round

setRound :: Round -> Ctx ()
setRound (Round r) = Ctx $ \ptr -> poke (p'decContext'round ptr) r

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
