{-# LANGUAGE Trustworthy, DeriveDataTypeable #-}
module Deka.Internal.Context where

import Foreign.C
import Foreign.Safe
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Typeable
import Deka.Internal.Mpdec
import System.IO.Unsafe (unsafePerformIO)

-- | The Ctx monad
--
-- The General Decimal Arithmetic specification states that most
-- computations occur within a @context@, which affects the manner
-- in which computations are done (for instance, the context
-- determines the rounding algorithm).  The context also carries
-- the flags that computations can set (for instance, a computation might
-- set a flag to indicate that the result is rounded or inexact or
-- was a division by zero.) The Ctx monad carries this context.

newtype Ctx a = Ctx { unCtx :: Ptr C'mpd_context_t -> IO a }

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

newtype Round = Round { _unRound :: CInt }
  deriving (Eq, Ord)

-- | Round toward positive infinity.
roundCeiling :: Round
roundCeiling = Round c'MPD_ROUND_CEILING

-- | Round away from zero.
roundUp :: Round
roundUp = Round c'MPD_ROUND_UP

-- | @0.5@ rounds up
roundHalfUp :: Round
roundHalfUp = Round c'MPD_ROUND_HALF_UP

-- | @0.5@ rounds to nearest even
roundHalfEven :: Round
roundHalfEven = Round c'MPD_ROUND_HALF_EVEN

-- | @0.5@ rounds down
roundHalfDown :: Round
roundHalfDown = Round c'MPD_ROUND_HALF_DOWN

-- | Round toward zero - truncate
roundDown :: Round
roundDown = Round c'MPD_ROUND_DOWN

-- | Round toward negative infinity.
roundFloor :: Round
roundFloor = Round c'MPD_ROUND_FLOOR

-- | Round for reround
round05Up :: Round
round05Up = Round c'MPD_ROUND_05UP

-- | Truncate, but set infinities.
roundTruncate :: Round
roundTruncate = Round c'MPD_ROUND_TRUNC

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
    | r == roundTruncate = "truncate"
    | otherwise = error "show: unknown rounding value"

getRound :: Ctx Round
getRound = Ctx $ fmap Round . peek . p'mpd_context_t'round

setRound :: Round -> Ctx ()
setRound (Round r) = Ctx $ \ptr -> poke (p'mpd_context_t'round ptr) r

-- # Precision

-- | Sets the precision to be used for all operations.  The result
-- of an operation is rounded to this length if necessary.
newtype Precision = Precision { unPrecision :: Signed }
  deriving (Eq, Ord, Show)

instance Bounded Precision where
  minBound = Precision 1
  maxBound = Precision c'MPD_MAX_PREC

-- | Creates a 'Precision' that you can then set with
-- 'setTrio'.  Returns 'Nothing' if the argument is out of
-- range.  The minimum possible value is always 1; the maximum
-- possible value is platform dependent and is revealed by
-- 'maxBound'.
precision :: Signed -> Maybe Precision
precision i
  | i < 1 = Nothing
  | i > c'MPD_MAX_PREC = Nothing
  | otherwise = Just . Precision $ i

-- | Sets the precision to the maximum possible, respecting that
-- @'Emax' > 5 * 'Precision'@.  Returns the new 'Precision'.

setMaxPrecision :: Ctx Precision
setMaxPrecision = do
  Emax x <- getEmax
  let p' = Precision $ x `div` 5
  setPrecision p'
  return p'

setPrecision :: Precision -> Ctx ()
setPrecision (Precision d) = Ctx $ \ptr ->
  poke (p'mpd_context_t'prec ptr) d

getPrecision :: Ctx Precision
getPrecision = Ctx $ fmap Precision . peek . p'mpd_context_t'prec

-- # Emax, Emin

-- | Maximum adjusted exponent.  The adjusted exponent is calculated
-- as though the number were expressed in scientific notation.  If
-- the adjusted exponent would be larger than 'Emax' then an
-- overflow results.
--
-- The minimum possible value is always 0; the
-- maximum possible value is platform dependent and is revealed by
-- 'maxBound'.
newtype Emax = Emax { unEmax :: Signed }
  deriving (Eq, Ord, Show)

instance Bounded Emax where
  minBound = Emax 0
  maxBound = Emax c'MPD_MAX_EMAX

-- | Minimum adjusted exponent.  The adjusted exponent is calculated
-- as though the number were expressed in scientific notation.  If
-- the adjusted exponent would be smaller than 'Emin' then the
-- result is subnormal.  If the result is also inexact, an underflow
-- results.  If subnormal results are allowed (see 'setClamp') the
-- smallest possible exponent is 'Emin' minus 'Precision' plus @1@.
--
-- The minimum possible value is platform dependent
-- and is revealed by 'minBound'; the maximum possible value is
-- always 0.
newtype Emin = Emin { unEmin :: Signed }
  deriving (Eq, Ord, Show)

instance Bounded Emin where
  minBound = Emin c'MPD_MIN_EMIN
  maxBound = Emin 0

-- | Returns an 'Emax' for use in 'setTrio'.  Fails if argument is
-- out of range.
emax :: Signed -> Maybe Emax
emax i
  | r < minBound = Nothing
  | r > maxBound = Nothing
  | otherwise = Just r
  where
    r = Emax i

-- | Returns an 'Emin' for use in 'setTrio'.  Fails if argument is
-- out of range.
emin :: Signed -> Maybe Emin
emin i
  | r < minBound = Nothing
  | r > maxBound = Nothing
  | otherwise = Just r
  where
    r = Emin i

getEmax :: Ctx Emax
getEmax = Ctx $ fmap Emax . peek . p'mpd_context_t'emax

setEmax :: Emax -> Ctx ()
setEmax (Emax i) = Ctx $ \ptr -> poke (p'mpd_context_t'emax ptr) i

getEmin :: Ctx Emin
getEmin = Ctx $ fmap Emin . peek . p'mpd_context_t'emin

setEmin :: Emin -> Ctx ()
setEmin (Emin i) = Ctx $ \ptr -> poke (p'mpd_context_t'emin ptr) i

-- # Trio

-- | In addition to the limits on 'Precision', 'Emax', and 'Emin',
-- there are also requirements on the relationship between these
-- three variables:
--
-- * @'Emax' > 5 * 'Precision'@ 
--
-- * either @'Emin' == 1 - 'Emax'@ or @'Emin' == -'Emax'@
--
-- The 'Trio' enforces this relationship.
--
-- It is also recommended that @'Emax' > 10 * 'Precision'@, but
-- since this is not required the 'Trio' does not enforce it.

data Trio = Trio
  { trioPrecision :: Precision
  , trioEmax :: Emax
  , trioEmin :: Emin
  } deriving Show

-- | Make a new 'Trio'.  Fails if the values are out of range.

trio :: Precision -> Emax -> Emin -> Maybe Trio
trio pp@(Precision p) px@(Emax x) pn@(Emin n)
  | not $ x > 5 * p = Nothing
  | not $ n == 1 - x || n == negate x = Nothing
  | otherwise = Just $ Trio pp px pn

setTrio :: Trio -> Ctx ()
setTrio (Trio p x n) =
  setPrecision p >> setEmax x >> setEmin n

getTrio :: Ctx Trio
getTrio = liftM3 Trio getPrecision getEmax getEmin

-- # Clamp

getClamp :: Ctx Bool
getClamp = Ctx $ fmap (/= 0) . peek . p'mpd_context_t'clamp

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
    f ptr = poke (p'mpd_context_t'clamp ptr) v
    v = if b then 1 else 0


-- # Flags

-- | Indicates error conditions.  This type serves two purposes:
-- computations set flags to indicate errors, and flags indicate
-- which errors you want to have raise a signal.  See 'getStatus',
-- 'setStatus', 'getTraps', and 'setTraps'.
--
-- 'Flag' is an instance of 'Exception' so that you can throw it if
-- you want; however, none of the functions in the @deka@ package
-- throw.
newtype Flag = Flag { unFlag :: Word32 }
  deriving (Eq, Ord, Typeable)

instance Exception Flag

instance Show Flag where
  show f
    | f == conversionSyntax     = "Conversion syntax"
    | f == divisionByZero       = "Division by zero"
    | f == divisionImpossible   = "Division impossible"
    | f == divisionUndefined    = "Division undefined"
    | f == inexact              = "Inexact"
    | f == invalidContext       = "Invalid context"
    | f == invalidOperation     = "Invalid operation"
    | f == mallocError          = "malloc error"
    | f == fpuError             = "FPU error"
    | f == notImplemented       = "Not implemented"
    | f == overflow             = "Overflow"
    | f == clamped              = "Clamped"
    | f == rounded              = "Rounded"
    | f == subnormal            = "Subnormal"
    | f == underflow            = "Underflow"
    | otherwise = error "show flag: unrecognized flag"

-- | A container of 'Flag'.
newtype Flags = Flags { unFlags :: Word32 }
  deriving (Eq, Typeable)

instance Show Flags where
  show = show . unpackFlags

instance Exception Flags

-- | A list of all possible 'Flag', in order.
allFlag :: [Flag]
allFlag =
  [ clamped
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
  ]

-- | All possible 'Flag' are set.
fullFlags :: Flags
fullFlags = packFlags allFlag

-- | No 'Flag' are set.
emptyFlags :: Flags
emptyFlags = Flags 0

-- | Flags will always be unpacked in order.
unpackFlags :: Flags -> [Flag]
unpackFlags (Flags i) = f allFlag
  where
    f [] = []
    f (Flag x:xs)
      | x .&. i /= 0 = Flag x : f xs
      | otherwise = f xs

packFlags :: [Flag] -> Flags
packFlags = Flags . foldl (.|.) 0 . map unFlag

-- | A source string (for instance, in 'fromByteString') contained
-- errors.
conversionSyntax :: Flag
conversionSyntax = Flag c'MPD_Conversion_syntax

-- | A non-zero dividend is divided by zero.  Unlike @0/0@, it has a
-- defined result (a signed Infinity).
divisionByZero :: Flag
divisionByZero = Flag c'MPD_Division_by_zero

-- | Sometimes raised by 'divideInteger' and 'remainder'.
divisionImpossible :: Flag
divisionImpossible = Flag c'MPD_Division_impossible

-- | @0/0@ is undefined.  It sets this flag and returns a quiet NaN.
divisionUndefined :: Flag
divisionUndefined = Flag c'MPD_Division_undefined

-- | One or more non-zero coefficient digits were discarded during
-- rounding.
inexact :: Flag
inexact = Flag c'MPD_Inexact

-- | The Context for computations was invalid; this error should
-- never occur because @deka@ keeps you from setting an invalid
-- context.
invalidContext :: Flag
invalidContext = Flag c'MPD_Invalid_context

-- | Raised on a variety of invalid operations, such as an attempt
-- to use 'compareSignal' on an operand that is an NaN.
invalidOperation :: Flag
invalidOperation = Flag c'MPD_Invalid_operation

mallocError :: Flag
mallocError = Flag c'MPD_Malloc_error

fpuError :: Flag
fpuError = Flag c'MPD_Fpu_error

notImplemented :: Flag
notImplemented = Flag c'MPD_Not_implemented

-- | The exponent of a result is too large to be represented.
overflow :: Flag
overflow = Flag c'MPD_Overflow

clamped :: Flag
clamped = Flag c'MPD_Clamped

rounded :: Flag
rounded = Flag c'MPD_Rounded

subnormal :: Flag
subnormal = Flag c'MPD_Subnormal

-- | A result is both subnormal and inexact.
underflow :: Flag
underflow = Flag c'MPD_Underflow

-- # Traps

-- ## Set

-- | If you set a trap, a computation will immediately raise
-- @SIGFPE@ if the corresponding error arises.  (Currently this
-- behavior cannot be configured to do something else.)
-- 'setTraps' clears all existing traps and sets them to the new
-- ones you specify.
--
-- By setting a flag here, SIGFPE is raised if any subsequent
-- computations raise the corresponding error condition.  Setting a
-- flag with this function or with 'setTrap' never, by itself,
-- causes SIGFPE to be raised; it is raised only by a subsequent
-- computation.  So, if you set a flag using this function or
-- 'setTrap' and the corresponding status flag is already set,
-- SIGFPE will be raised only if a subsequent computation raises
-- that error condition.
setTraps :: Flags -> Ctx ()
setTraps fs = Ctx $ \ptr -> do
  let pTr = p'mpd_context_t'traps ptr
  poke pTr (unFlags fs) 

-- | Gets all currently set traps.
getTraps :: Ctx Flags
getTraps = Ctx $ \ptr -> do
  ts <- peek (p'mpd_context_t'traps ptr)
  return $ Flags ts

-- # Status

-- ## Set

-- | Sets status flags.  All existing status flags are cleared and
-- replaced with the ones you indicate here.
setStatus :: Flags -> Ctx ()
setStatus fs = Ctx $ \ptr ->
  poke (p'mpd_context_t'status ptr) (unFlags fs)

-- | All currently set status flags.
getStatus :: Ctx Flags
getStatus = Ctx $ \ptr -> do
  let pSt = p'mpd_context_t'status ptr
  ts <- peek pSt
  return $ Flags ts

-- # Initializers

-- | Before running computations in a context. the context must be
-- initialized with certain settings, such as the rounding mode,
-- precision, and maximum adjusted exponent.  An 'Initializer'
-- contains all these settings.
--
-- On 64-bit platforms, the maximums are:
--
-- * 'Precision' of ((1 * 10 ^ 18) - 1)
-- * 'Emax' of ((1 * 10 ^ 18) - 1)
-- * 'Emin' of -((1 * 10 ^ 18) - 1)
--
-- On 32-bit platforms, the maximums are:
--
-- * 'Precision' of 4.25 * 10 ^ 8
-- * 'Emax' of 4.25 * 10 ^ 8
-- * 'Emin' of -4.25 * 10 ^ 8

data Initializer
  = Max
  -- ^ Sets:
  --
  -- * 'Precision' to the maximum available
  -- * 'Emax' to the maximum available
  -- * 'Emin' to the minimum available
  -- * 'Round' to 'roundHalfEven'
  -- * Traps to 'invalidOperation', 'divisionByZero', 'overflow',
  --   'underflow'
  -- * No status flags are set
  -- * No newtrap is set
  -- * 'setClamp' is False
  -- * 'setAllCorrectRound' is True
  --
  -- As noted in the documentation for 'Trio', the specification
  -- requires that @'Emax' > 5 * 'Precision'@; 'Max' does /not/
  -- respect this.

  | Default
  -- ^ Same as 'Max', except:
  --
  -- * Precision is @2 * MPD_RDIGITS@

  | Basic
  -- ^ Same as 'Max', except:
  --
  -- * 'Precision' is 9
  -- * Traps to 'invalidOperation', 'divisionByZero', 'overflow',
  --   'underflow', and 'clamped'

  | Pedantic
  -- ^ Sets the maximum allowable figures, while respecting the
  -- restriction that stated in the specification and the @mpdecimal@
  -- documentation, which is that @'Emax' > 5 * 'Precision'@.  Also,
  -- sets no traps.  This sets:
  --
  -- * 'Emax' to the maximum available
  -- * 'Emin' to the minimum available
  --
  -- * 'Precision' is set to @'Emax' `div` 5@.  On 64-bit platforms,
  -- this is ((2 * 10 ^ 17) - 1); on 32-bit platforms, this is 8.5 *
  -- 10 ^ 8.
  --
  -- * 'Round' to 'roundHalfEven'
  -- * No traps are set
  -- * No status flags are set
  -- * No newtrap is set
  -- * 'setClamp' is False
  -- * 'setAllCorrectRound' is True

  | Decimal32
  -- ^ Sets:
  --
  -- * 'Precision' to @7@
  -- * 'Emax' to @96@
  -- * 'Emin' to @-95@
  -- * Rounding to 'roundHalfEven'
  -- * No traps are enabled
  -- * No status flags are set
  -- 'newTrap' is clear
  -- * 'setClamp' is True
  -- * 'setAllCorrectRound' is True

  | Decimal64
  -- ^ Same as 'Decimal32', except:
  --
  -- * 'Precision' is @16@
  -- * 'Emax' is @384@
  -- * 'Emin' is @-383@

  | Decimal128
  -- ^ Same as 'Decimal32', except:
  --
  -- * 'Precision' is @34@
  -- * 'Emax' is @6144@
  -- * 'Emin' is @-6143@

-- | Re-initialize a 'Ctx' using the given Initializer.
initCtx :: Initializer -> Ctx ()
initCtx i = Ctx $ \p ->
  case i of
    Max -> c'mpd_maxcontext p
    Default -> c'mpd_defaultcontext p
    Basic -> c'mpd_basiccontext p
    Pedantic -> unCtx pedantic p
    Decimal32 -> c'mpd_ieee_context p 32 >> return ()
    Decimal64 -> c'mpd_ieee_context p 64 >> return ()
    Decimal128 -> c'mpd_ieee_context p 128 >> return ()

clearStatus :: Ctx ()
clearStatus = Ctx $ \p -> poke (p'mpd_context_t'status p) 0

clearNewtrap :: Ctx ()
clearNewtrap = Ctx $ \p -> poke (p'mpd_context_t'newtrap p) 0

pedantic :: Ctx ()
pedantic = do
  setEmax maxBound
  setEmin minBound
  let pc = Precision $ ((unEmax maxBound) `div` 5)
  setPrecision pc
  setRound roundHalfEven
  setTraps emptyFlags
  clearStatus
  clearNewtrap
  setClamp False
  setAllCorrectRound True

-- # allCorrectRound

-- | By default, most functions are correctly rounded.  By setting
-- allCorrectRound, correct rounding is additionally enabled for
-- exp, ln, and log10.  In this case, all functions except pow and
-- invroot return correctly rounded results.
getAllCorrectRound :: Ctx Bool
getAllCorrectRound = Ctx $ fmap (/= 0) . peek . p'mpd_context_t'allcr

setAllCorrectRound :: Bool -> Ctx ()
setAllCorrectRound b = Ctx f
  where
    f ptr = poke (p'mpd_context_t'allcr ptr) v
    v = if b then 1 else 0

-- # Runners

-- | Runs a Ctx computation; begins with the given Initializer to
-- set up the context.
runCtxInit :: Initializer -> Ctx a -> a
runCtxInit i (Ctx f) = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes c'mpd_context_t'sizeOf
  withForeignPtr fp $ \ptr -> do
    _ <- unCtx (initCtx i) ptr
    f ptr

-- | Runs a Ctx computation using the 'Pedantic' Initializer.
runCtx :: Ctx a -> a
runCtx = runCtxInit Pedantic

-- | Like 'runCtx' but also returns any status flags resulting from
-- the computation.
runCtxStatus :: Ctx a -> (a, Flags)
runCtxStatus c = runCtx $ do
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
  allocaBytes (c'mpd_context_t'sizeOf) $ \child ->
  copyBytes child parent c'mpd_context_t'sizeOf >>
  l child

