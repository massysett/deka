{-# LANGUAGE Safe, DeriveDataTypeable #-}
module Deka.Internal.Context where

import Deka.Internal.Decnumber.Context

import Foreign.Safe
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Typeable

-- | The Ctx monad
--
-- The General Decimal Arithmetic specification states that most
-- computations occur within a @context@, which affects the manner
-- in which computations are done (for instance, the context
-- determines the rounding algorithm).  The context also carries
-- the flags that computations can set (for instance, a computation might
-- set a flag to indicate that the result is rounded or inexact or
-- was a division by zero.) The Ctx monad carries this context.
--
-- A 'Ctx' has many fields. Computations in "Deka.DecNum" make use
-- of all these fields.  Computations in the "Deka.Fixed" modules
-- ignore the settings in all fields except for rounding and the
-- status flags.  For example, any traps you set with 'setTraps'
-- has no effect when using any of the computations in 'Deka.Fixed'.
newtype Ctx a = Ctx { unCtx :: Ptr C'decContext -> IO a }

instance Functor Ctx where
  fmap = liftM

instance Applicative Ctx where
  pure = return
  (<*>) = ap

instance Monad Ctx where
  return a = Ctx $ \_ -> return a
  Ctx a >>= f = Ctx $ \p -> do
    r1 <- a p
    let b = unCtx $ f r1
    b p
  fail s = Ctx $ \_ -> fail s

-- # Rounding

newtype Round = Round { _unRound :: C'rounding }
  deriving (Eq, Ord)

-- | Round toward positive infinity.
roundCeiling :: Round
roundCeiling = Round c'DEC_ROUND_CEILING

-- | Round away from zero.
roundUp :: Round
roundUp = Round c'DEC_ROUND_UP

-- | @0.5@ rounds up
roundHalfUp :: Round
roundHalfUp = Round c'DEC_ROUND_HALF_UP

-- | @0.5@ rounds to nearest even
roundHalfEven :: Round
roundHalfEven = Round c'DEC_ROUND_HALF_EVEN

-- | @0.5@ rounds down
roundHalfDown :: Round
roundHalfDown = Round c'DEC_ROUND_HALF_DOWN

-- | Round toward zero - truncate
roundDown :: Round
roundDown = Round c'DEC_ROUND_DOWN

-- | Round toward negative infinity.
roundFloor :: Round
roundFloor = Round c'DEC_ROUND_FLOOR

-- | Round for reround
round05Up :: Round
round05Up = Round c'DEC_ROUND_05UP

instance Show Round where
  show r
    | r == roundCeiling = "ceiling"
    | r == roundUp = "up"
    | r == roundHalfUp = "half up"
    | r == roundHalfEven = "half even"
    | r == roundHalfDown = "half down"
    | r == roundDown = "down"
    | r == roundFloor = "floor"
    | r == round05Up = "05up"
    | otherwise = error "show: unknown rounding value"

getRound :: Ctx Round
getRound = Ctx $ fmap Round . peek . p'decContext'round

setRound :: Round -> Ctx ()
setRound (Round r) = Ctx $ \ptr -> poke (p'decContext'round ptr) r

-- # Precision

-- | Sets the precision to be used for all operations.  The result
-- of an operation is rounded to this length if necessary.  Must
-- have a vlue in the range @1@ through @999,999,999@.
newtype Precision = Precision { unPrecision :: Int32 }
  deriving (Eq, Ord, Show)

instance Bounded Precision where
  minBound = Precision c'DEC_MIN_DIGITS
  maxBound = Precision c'DEC_MAX_DIGITS

-- # Initializers

-- | Before running computations in a context. the context must be
-- initialized with certain settings, such as the rounding mode,
-- precision, and maximum adjusted exponent.  An 'Initializer'
-- contains all these settings.
newtype Initializer = Initializer { _unInitializer :: Int32 }
  deriving (Eq, Ord)

-- | This sets:
--
-- * 'Precision' to @9@
-- * 'Emax' to @999,999,999@
-- * 'Emin' to @-999,999,999@
-- * Rounding to 'roundHalfUp'
-- * No status flags are set
-- * Traps are set to 'divisionByZero', 'invalidOperation',
--   'overflow', and 'underflow'
-- * 'setClamp' is True
-- * 'setExtended' is True
initBase :: Initializer
initBase = Initializer c'DEC_INIT_BASE

-- | This sets:
--
-- * 'Precision' to @7@
-- * 'Emax' to @96@
-- * 'Emin' to @-95@
-- * Rounding to 'roundHalfEven'
-- * No status flags are set
-- * No traps are enabled
-- * 'setClamp' is True
-- * 'setExtended' is True
initSingle :: Initializer
initSingle = Initializer c'DEC_INIT_DECSINGLE

-- | This sets the fields the same as for 'initSingle', except:
--
-- * 'Precision' is @16@
-- * 'Emax' is @384@
-- * 'Emin' is @-383@
initDouble :: Initializer
initDouble = Initializer c'DEC_INIT_DECDOUBLE

-- | This sets the fields the same as for 'initSingle', except:
--
-- * 'Precision' is @34@
-- * 'Emax' is @6144@
-- * 'Emin' is @-6143@
initQuad :: Initializer
initQuad = Initializer c'DEC_INIT_DECQUAD

instance Show Initializer where
  show i
    | i == initBase = "base"
    | i == initSingle = "single"
    | i == initDouble = "double"
    | i == initQuad = "quad"
    | otherwise = error "show initializer: unknown value"

-- # Run

-- | Runs a Ctx computation; begins with the given Initializer to
-- set up the context.
runCtx :: Initializer -> Ctx a -> IO a
runCtx (Initializer i) (Ctx f) = do
  fp <- mallocForeignPtrBytes c'decContext'sizeOf
  withForeignPtr fp $ \ptr -> do
    _ <- c'decContextDefault ptr i
    f ptr

-- | Like 'runCtx' but also returns any status flags resulting from
-- the computation.
runCtxStatus :: Initializer -> Ctx a -> IO (a, [Flag])
runCtxStatus i c = runCtx i $ do
  r <- c
  f <- getStatus
  return (r, f)

-- # Local

-- | Runs a Ctx computation within the existing Ctx.  The existing
-- Ctx is copied to form a new Ctx; then the child computation is
-- run without affecting the parent Ctx.

local
  :: Ctx a
  -- ^ Run this computation.  It is initialized with the current
  -- Ctx, but does not affect the current Ctx.
  -> Ctx a
  -- ^ Returns the result of the child computation.
local (Ctx l) = Ctx $ \parent ->
  allocaBytes (c'decContext'sizeOf) $ \child ->
  copyBytes child parent c'decContext'sizeOf >>
  l child

-- # Flags

-- | Indicates error conditions.  This type serves two purposes:
-- computations set flags to indicate errors; these can be queried
-- with 'isStatusSet' and 'getStatus' and cleared with
-- 'clearStatus'.  In addition, you can set traps; see 'setTraps',
-- 'setTrap', and 'clearTraps'.
--
-- 'Flag' is an instance of 'Exception' so that you can throw it if
-- you want; however, none of the functions in the @deka@ package
-- throw.
newtype Flag = Flag { unFlag :: Word32 }
  deriving (Eq, Ord, Typeable)

instance Exception Flag

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

-- | A list of all possible 'Flag'.
allFlags :: [Flag]
allFlags =
  [ conversionSyntax, divisionByZero, divisionImpossible,
    divisionUndefined, insufficientStorage, inexact,
    lostDigits, overflow, clamped, rounded, subnormal,
    underflow ]

whichFlags :: Word32 -> [Flag]
whichFlags i =
  map Flag
  . filter ((/= 0) . (.&. i))
  . map unFlag
  $ allFlags

combineFlags :: [Flag] -> Word32
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

-- | The Context for computations was invalid; this error should
-- never occur because @deka@ keeps you from setting an invalid
-- context.
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

-- | If you set a trap, a computation will immediately raise
-- @SIGFPE@ if the corresponding error arises.  (This behavior is
-- set in the decNumber C library and cannot be configured.)
-- 'setTraps' causes a trap to be set for all the 'Flag' you list.
-- This is not a toggling mechanism; if you list a 'Flag' here, it
-- will be set as a trap.  This function never clears any traps that
-- have already been set.
--
-- By setting a flag here, SIGFPE is raised if any subsequent
-- computations raise the corresponding error condition.  Setting a
-- flag with this function or with 'setTrap' never, by itself,
-- causes SIGFPE to be raised; it is raised only by a subsequent
-- computation.  So, if you set a flag using this function or
-- 'setTrap' and the corresponding status flag is already set,
-- SIGFPE will be raised only if a subsequent computation raises
-- that error condition.
setTraps :: [Flag] -> Ctx ()
setTraps fs = Ctx $ \ptr -> do
  let pTr = p'decContext'traps ptr
  poke pTr (combineFlags fs) 

-- | Sets a single trap.  Does not affect any other traps.  This
-- always sets the given trap; this is not a toggling mechanism.
-- See notes under 'setTraps'.
setTrap :: Flag -> Ctx ()
setTrap f = Ctx $ \ptr -> do
  let pTr = p'decContext'traps ptr
  ts <- peek pTr
  poke pTr (ts .|. unFlag f)

-- | Clears the given traps; does not affect any traps not listed.
clearTraps :: [Flag] -> Ctx ()
clearTraps fs = Ctx $ \ptr -> do
  let pTr = p'decContext'traps ptr
  ts <- peek pTr
  poke pTr (ts .&. complement (combineFlags fs))

-- ## Query

-- | Is this particular trap set?
isTrapSet :: Flag -> Ctx Bool
isTrapSet (Flag f) = Ctx $ \ptr -> do
  let pTr = p'decContext'traps ptr
  ts <- peek pTr
  return $ ts .&. f /= 0

-- | Gets a list of all currently set traps.
getTraps :: Ctx [Flag]
getTraps = Ctx $ \ptr -> do
  ts <- peek (p'decContext'traps ptr)
  return $ whichFlags ts

-- # Status

-- ## Set

-- No set functions provided - you're not supposed to set traps

-- | Clears status flags.  Status flags indicate whether previous
-- computations have caused particular error conditions.
--
-- No function is provided to set status flags; only computations
-- may set status flags.
clearStatus :: [Flag] -> Ctx ()
clearStatus fs = Ctx $ \ptr -> do
  _ <- c'decContextClearStatus ptr (combineFlags fs) 
  return ()

-- ## Query

-- | Is this particular status flag set?
isStatusSet :: Flag -> Ctx Bool
isStatusSet (Flag f) = Ctx $ \ptr -> do
  let pSt = p'decContext'status ptr
  ts <- peek pSt
  return $ ts .&. f /= 0

-- | Returns a list of all currently set status flags.
getStatus :: Ctx [Flag]
getStatus = Ctx $ \ptr -> do
  let pSt = p'decContext'status ptr
  ts <- peek pSt
  return $ whichFlags ts

-- # Digits

-- | Creates a 'Precision' that you can then set with
-- 'setPrecision'.  Returns 'Nothing' if the argument is less than
-- @1@ or greater than @999,999,999@.
precision :: Int32 -> Maybe Precision
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

-- | Maximum adjusted exponent.  The adjusted exponent is calculated
-- as though the number were expressed in scientific notation.  If
-- the adjusted exponent would be larger than 'Emax' then an
-- overflow results.  This value must be in the range @0@ through
-- @999,999,999@.
newtype Emax = Emax { unEmax :: Int32 }
  deriving (Eq, Ord, Show)

instance Bounded Emax where
  minBound = Emax c'DEC_MIN_EMAX
  maxBound = Emax c'DEC_MAX_EMAX

-- | Minimum adjusted exponent.  The adjusted exponent is calculated
-- as though the number were expressed in scientific notation.  If
-- the adjusted exponent would be smaller than 'Emin' then the
-- result is subnormal.  If the result is also inexact, an underflow
-- results.  If subnormal results are allowed (see 'setClamp') the
-- smallest possible exponent is 'Emin' minus 'Precision' plus @1@.
--
-- 'Emin' is usually set to @-Emax@ or to @-(Emax - 1)@.  It must be
-- in the range @-999,999,999@ through @0@.
newtype Emin = Emin { unEmin :: Int32 }
  deriving (Eq, Ord, Show)

instance Bounded Emin where
  minBound = Emin c'DEC_MIN_EMIN
  maxBound = Emin c'DEC_MAX_EMIN

-- | Returns an 'Emax' for use in 'setEmax', but only if the given
-- argument is in the range @0@ through @999,999,999@.

emax :: Int32 -> Maybe Emax
emax i
  | i < c'DEC_MIN_EMAX = Nothing
  | i > c'DEC_MAX_EMAX = Nothing
  | otherwise = Just . Emax $ i

-- | Returns an 'Emin' for use in 'setEmin', but only if the given
-- argument is in the range @-999,999,999@ through @0@.

emin :: Int32 -> Maybe Emin
emin i
  | i < c'DEC_MIN_EMIN = Nothing
  | i > c'DEC_MAX_EMIN = Nothing
  | otherwise = Just . Emin $ i

getEmax :: Ctx Emax
getEmax = Ctx $ fmap Emax . peek . p'decContext'emax

setEmax :: Emax -> Ctx ()
setEmax (Emax i) = Ctx $ \ptr -> poke (p'decContext'emax ptr) i

getEmin :: Ctx Emin
getEmin = Ctx $ fmap Emin . peek . p'decContext'emin

setEmin :: Emin -> Ctx ()
setEmin (Emin i) = Ctx $ \ptr -> poke (p'decContext'emin ptr) i

-- # Clamp and extended

getClamp :: Ctx Bool
getClamp = Ctx $ fmap (/= 0) . peek . p'decContext'clamp

-- | Controls explicit exponent clamping.  When False, a result
-- exponent is limited to a maximum of emax and a minimum of emin
-- (for example, the exponent of a zero result will be clamped to be
-- in this range). When True, a result exponent has the same minimum
-- but is limited to a maximum of emax-(digits-1). As well as
-- clamping zeros, this may cause the coefficient of a result to be
-- padded with zeros on the right in order to bring the exponent
-- within range.
--
-- Also when True, this limits the length of NaN payloads to
-- 'Precision' - 1 when constructing a NaN by conversion from a
-- string.

setClamp :: Bool -> Ctx ()
setClamp b = Ctx f
  where
    f ptr = poke (p'decContext'clamp ptr) v
    v = if b then 1 else 0

getExtended :: Ctx Bool
getExtended = Ctx $ fmap (/= 0) . peek . p'decContext'extended

-- | When True, special values are possible and subnormal numbers
-- can result from operations.  When False, no negative zeroes are
-- possible; operands are rounded, and the exponent range is
-- balanced.
setExtended :: Bool -> Ctx ()
setExtended b = Ctx f
  where
    f ptr = poke (p'decContext'extended ptr) v
    v = if b then 1 else 0

