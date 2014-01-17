{-# LANGUAGE Safe #-}
module Data.Dimes.Safe
  (
    -- * Env monad
    Env
  , runEnv
  , evalEnv
  
   -- * Context
  , Context(..)
  , getContext
  , setContext

    -- ** Initializing an Env
  , Initializer
  , maxContext
  , basicContext
  , defaultContext
  , initFromContext
  
  -- *** IEEE interchange formats
  , Interchange
  , interchange
  , unInterchange
  , interchangeInitializer

  -- *** Common interchange formats
  , interchange32
  , interchange64
  , interchange128

  -- ** Precision
  , Precision
  , precision
  , unPrecision
  , getPrecision
  , setPrecision
  , PosExpMax
  , posExpMax
  , unPosExpMax
  , getPosExpMax
  , setPosExpMax
  , NegExpMin
  , negExpMin
  , unNegExpMin
  , getNegExpMin
  , setNegExpMin

  -- ** Traps and signals
  -- *** Individual signals
  , Signal
  , ieeeInvalidOperation
  , clamped
  , divisionByZero
  , fpuError
  , inexact
  , notImplemented
  , overflow
  , rounded
  , subnormal
  , underflow

  -- *** Groups of signals
  , Signals
  , emptySignals
  , addSignal
  , concatSignals
  , isSet

  -- *** Traps
  , Traps(..)
  , getTraps
  , setTraps

  -- *** Status flags
  , Status(..)
  , getStatus
  , setStatus

  -- *** Newtrap
  , NewTrap(..)
  , getNewTrap
  , setNewTrap

  -- ** Rounding
  , Round
  , roundUp
  , roundDown
  , roundCeiling
  , roundFloor
  , roundHalfUp
  , roundHalfDown
  , roundHalfEven
  , round05up
  , roundTrunc
  , getRound
  , setRound

  -- ** Clamp
  , Clamp
  , clampOn
  , clampOff
  , getClamp
  , setClamp

  -- ** Correct Rounding
  , RoundAll
  , roundAllOn
  , roundAllOff
  , getRoundAll
  , setRoundAll

  -- ** Exponent limits
  , getExpTiny
  , getExpTop

  -- * Mpd
  , Mpd

  -- * Assignment, conversions, I/O
  , setString
  , setIntegral

  -- ** Set special values
  , Sign
  , positive
  , negative
  , NonFinite
  , infinite
  , nan
  , sNan
  , setSpecial

  -- ** Convert Decimal
  , ExpCase
  , expLower
  , expUpper
  , mpdToSci
  , mpdToEng
  , mpdFormat

  -- * Attributes of a Decimal
  , adjExp
  , isFinite
  , isInfinite
  , isNan
  , isNegative
  , isPositive
  , isQnan
  , isSigned
  , isSnan
  , isSpecial
  , isZero
  , isNormal
  , isSubnormal
  , isInteger
  , OddEven(..)
  , isOddEven
  , sign
  , trailZeros
  , isZeroCoeff
  , isOddCoeff

  -- * Arithmetic
  , add
  , sub
  , mul
  , div
  , fma
  , divInt
  , rem
  , remNear
  , divMod
  , exp
  , ln
  , log10
  , pow
  , sqrt
  , invroot
  , minus
  , plus
  , abs
  , cmp
  , cmpTotal
  , cmpTotalMagnitude
  , max
  , maxMag
  , min
  , minMag
  , nextMinus
  , nextPlus
  , nextToward
  , quantize
  , rescale
  , sameQuantum
  , reduce
  , roundToIntegralExact
  , roundToIntegralValue
  , floor
  , ceiling
  , truncate
  , logb
  , scaleb
  , powmod

  -- * Shifting and rounding
  , shift
  , shiftn
  , shiftl
  , shiftr
  , rotate

  -- * Logical
  , and
  , or
  , xor
  , invert

  ) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Bindings.Mpdecimal
import Foreign.C.Types
import Foreign.Safe hiding
  (isSigned, shift, rotate, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Prelude hiding
  (isInfinite, div, rem, divMod, exp, sqrt, abs, min, max,
   floor, ceiling, truncate, and, or)
import qualified Prelude as P

-- # Context

data Context = Context
  { ctxPrecision :: Precision
  , ctxPosExpMax :: PosExpMax
  , ctxNegExpMin :: NegExpMin
  , ctxTraps :: Traps
  , ctxStatus :: Status
  , ctxNewTrap :: NewTrap
  , ctxRound :: Round
  , ctxClamp :: Clamp
  , ctxRoundAll :: RoundAll
  }

getContext :: Env Context
getContext
  = Context
  <$> getPrecision
  <*> getPosExpMax
  <*> getNegExpMin
  <*> getTraps
  <*> getStatus
  <*> getNewTrap
  <*> getRound
  <*> getClamp
  <*> getRoundAll

setContext :: Context -> Env ()
setContext c = do
  setPrecision . ctxPrecision $ c
  setPosExpMax . ctxPosExpMax $ c
  setNegExpMin . ctxNegExpMin $ c
  setTraps . ctxTraps $ c
  setStatus . ctxStatus $ c
  setNewTrap . ctxNewTrap $ c
  setRound . ctxRound $ c
  setClamp . ctxClamp $ c
  setRoundAll . ctxRoundAll $ c


-- ## Initializing a context

newtype Initializer = Initializer
  { unInitializer :: Ptr C'mpd_context_t -> IO () }

maxContext :: Initializer
maxContext = Initializer c'mpd_maxcontext

basicContext :: Initializer
basicContext = Initializer c'mpd_basiccontext

defaultContext :: Initializer
defaultContext = Initializer c'mpd_defaultcontext

initFromContext :: Context -> Initializer
initFromContext = Initializer . unEnv . setContext

-- * IEEE interchange formats

newtype Interchange = Interchange { unInterchange :: CInt }
  deriving (Eq, Ord, Show)

-- | Creates an Interchange.  The argument must be a multiple of 32
-- and less than IEEE_CONTEXT_MAX_BITS; fails if this is not true.
interchange :: Integral a => a -> Maybe Interchange
interchange a
  | a <= 0 = Nothing
  | a `P.rem` 32 /= 0 = Nothing
  | a >= c'MPD_IEEE_CONTEXT_MAX_BITS = Nothing
  | otherwise = Just . Interchange . fromIntegral $ a

interchange32 :: Interchange
interchange32 = Interchange c'MPD_DECIMAL32

interchange64 :: Interchange
interchange64 = Interchange c'MPD_DECIMAL64

interchange128 :: Interchange
interchange128 = Interchange c'MPD_DECIMAL128

interchangeInitializer :: Interchange -> Initializer
interchangeInitializer i = Initializer $ \p -> do
  e <- c'mpd_ieee_context p (unInterchange i)
  if e /= 0 then error "interchangeInitializer failed" else return ()


-- # Env monad

newtype Env a = Env
  { unEnv :: Ptr C'mpd_context_t -> IO a }

instance Monad Env where
  return a = Env $ \_ -> return a
  a >>= f = Env $ \p -> do
    r1 <- unEnv a p
    let b = unEnv $ f r1
    b p

liftIO :: IO a -> Env a
liftIO i = Env $ \_ -> i

instance Functor Env where
  fmap = liftM

instance Applicative Env where
  pure = return
  (<*>) = ap

-- In Control.Monad.Trans.State:
-- run returns (a, s)
-- eval returns a
-- exec returns s

-- | Runs a Env computation.
runEnv :: Initializer -> Env a -> IO (a, Context)
runEnv i f =
  bracket get release $ \hdlr -> do
    poke p'mpd_traphandler hdlr
    p <- mallocForeignPtr
    withForeignPtr p $ \ptr -> do
      unInitializer i ptr
      unEnv f' ptr
  where
    get = mk'funptr_mpd_traphandler (const $ return ())
    release = freeHaskellFunPtr
    f' = (,) <$> f <*> getContext

evalEnv :: Initializer -> Env a -> IO a
evalEnv i f =
  bracket get release $ \hdlr -> do
    poke p'mpd_traphandler hdlr
    p <- mallocForeignPtr
    withForeignPtr p $ \ptr -> do
      unInitializer i ptr
      unEnv f ptr
  where
    get = mk'funptr_mpd_traphandler (const $ return ())
    release = freeHaskellFunPtr


getEnvPtr :: Env (Ptr C'mpd_context_t)
getEnvPtr = Env $ \p -> return p

withEnvPtr :: (Ptr C'mpd_context_t -> IO a) -> Env a
withEnvPtr f = do
  p <- getEnvPtr
  liftIO $ f p

-- # Env helpers
getter
  :: (a -> b)
  -> (Ptr C'mpd_context_t -> IO a)
  -> Env b
getter f g = fmap f $ withEnvPtr g

setter
  :: String
  -- ^ For error message
  -> (a -> b)
  -> (Ptr C'mpd_context_t -> b -> IO CInt)
  -> a
  -> Env ()
setter s f g a = do
  e <- withEnvPtr (\c -> g c (f a))
  if e /= 0 then error (s ++ " setter failed") else return ()


-- # Precision

newtype Precision = Precision { unPrecision :: C'mpd_ssize_t }
  deriving (Eq, Show, Ord)

-- | Precision.  This must be a positive integer.  Fails if the
-- integer is not positive, or if it exceeds MPD_MAX_PREC.

precision :: Integer -> Maybe Precision
precision i
  | i < 1 = Nothing
  | i > c'MPD_MAX_PREC = Nothing
  | otherwise = Just . Precision . fromIntegral $ i

getPrecision :: Env Precision
getPrecision = getter Precision c'mpd_getprec

setPrecision :: Precision -> Env ()
setPrecision = setter "Precision" unPrecision c'mpd_qsetprec

newtype PosExpMax = PosExpMax
  { unPosExpMax :: C'mpd_ssize_t }
  deriving (Eq, Show, Ord)

-- | Maximum positive exponent.  This must be a positive integer.
-- Fails if out of range.
posExpMax :: Integer -> Maybe PosExpMax
posExpMax i
  | i < 1 = Nothing
  | i > c'MPD_MAX_EMAX = Nothing
  | otherwise = Just . PosExpMax . fromIntegral $ i

getPosExpMax :: Env PosExpMax
getPosExpMax = getter PosExpMax c'mpd_getemax

setPosExpMax :: PosExpMax -> Env ()
setPosExpMax = setter "PosExpMax" unPosExpMax c'mpd_qsetemax

newtype NegExpMin = NegExpMin { unNegExpMin :: C'mpd_ssize_t }
  deriving (Eq, Show, Ord)

-- | Minimum negative exponent.  This must be a negative integer.
-- Fails if out of range.
negExpMin :: Integer -> Maybe NegExpMin
negExpMin i
  | i > -1 = Nothing
  | i < c'MPD_MIN_EMIN = Nothing
  | otherwise = Just . NegExpMin . fromIntegral $ i

getNegExpMin :: Env NegExpMin
getNegExpMin = getter NegExpMin c'mpd_getemin

setNegExpMin :: NegExpMin -> Env ()
setNegExpMin = setter "NegExpMin" unNegExpMin c'mpd_qsetemin

-- ### Individual Signals

newtype Signal = Signal C'uint32_t
  deriving (Eq, Show, Ord)

ieeeInvalidOperation :: Signal
ieeeInvalidOperation = Signal $ c'MPD_IEEE_Invalid_operation

clamped :: Signal
clamped = Signal $ c'MPD_Clamped

divisionByZero :: Signal
divisionByZero = Signal $ c'MPD_Division_by_zero

fpuError :: Signal
fpuError = Signal $ c'MPD_Fpu_error

inexact :: Signal
inexact = Signal $ c'MPD_Inexact

notImplemented :: Signal
notImplemented = Signal c'MPD_Not_implemented

overflow :: Signal
overflow = Signal c'MPD_Overflow

rounded :: Signal
rounded = Signal c'MPD_Rounded

subnormal :: Signal
subnormal = Signal c'MPD_Subnormal

underflow :: Signal
underflow = Signal c'MPD_Underflow

-- ### Groups of signals

newtype Signals = Signals { unSignals :: C'uint32_t }
  deriving (Eq, Show, Ord)

emptySignals :: Signals
emptySignals = Signals 0

addSignal :: Signals -> Signal -> Signals
addSignal (Signals ss) (Signal s) = Signals $ ss .|. s

concatSignals :: [Signal] -> Signals
concatSignals = foldl addSignal emptySignals

isSet :: Signals -> Signal -> Bool
isSet (Signals ss) (Signal s) = ss .&. s /= 0

-- ### Traps

newtype Traps = Traps { unTraps :: Signals }
  deriving (Eq, Ord, Show)

getTraps :: Env Traps
getTraps = getter (Traps . Signals) c'mpd_gettraps

setTraps :: Traps -> Env ()
setTraps = setter "Traps" (unSignals . unTraps) c'mpd_qsettraps

-- ### Status

newtype Status = Status { unStatus :: Signals }
  deriving (Eq, Ord, Show)

getStatus :: Env Status
getStatus = getter (Status . Signals) c'mpd_getstatus

setStatus :: Status -> Env ()
setStatus = setter "Status" (unSignals . unStatus) c'mpd_qsetstatus

-- ### NewTrap

newtype NewTrap = NewTrap { unNewTrap :: Signals }
  deriving (Eq, Ord, Show)

getNewTrap :: Env NewTrap
getNewTrap = getter (NewTrap . Signals) f
  where
    f = peek . p'mpd_context_t'newtrap

setNewTrap :: NewTrap -> Env ()
setNewTrap = setter "NewTrap" (unSignals . unNewTrap) f
  where
    f ptr val =
      let destPtr = p'mpd_context_t'newtrap ptr
      in poke destPtr val >> return 0

-- # Rounding

newtype Round = Round { unRound :: CInt }
  deriving (Eq, Ord, Show)

-- RoundGuard not implemented; it's not in the docs

roundUp, roundDown, roundCeiling, roundFloor, roundHalfUp,
  roundHalfDown, roundHalfEven, round05up, roundTrunc :: Round

roundUp = Round c'MPD_ROUND_UP
roundDown = Round c'MPD_ROUND_DOWN
roundCeiling = Round c'MPD_ROUND_CEILING
roundFloor = Round c'MPD_ROUND_FLOOR
roundHalfUp = Round c'MPD_ROUND_HALF_UP
roundHalfDown = Round c'MPD_ROUND_HALF_DOWN
roundHalfEven = Round c'MPD_ROUND_HALF_EVEN
round05up = Round c'MPD_ROUND_05UP
roundTrunc = Round c'MPD_ROUND_TRUNC

getRound :: Env Round
getRound = getter Round c'mpd_getround

setRound :: Round -> Env ()
setRound = setter "Round" unRound c'mpd_qsetround

-- * Clamp
newtype Clamp = Clamp { unClamp :: CInt }
  deriving (Eq, Ord, Show)

clampOn :: Clamp
clampOn = Clamp 1

clampOff :: Clamp
clampOff = Clamp 0

getClamp :: Env Clamp
getClamp = getter Clamp c'mpd_getclamp

setClamp :: Clamp -> Env ()
setClamp = setter "Clamp" unClamp c'mpd_qsetclamp

-- * Round all functions correctly

newtype RoundAll = RoundAll { unRoundAll :: CInt }
  deriving (Eq, Ord, Show)

roundAllOn :: RoundAll
roundAllOn = RoundAll 1

roundAllOff :: RoundAll
roundAllOff = RoundAll 0

getRoundAll :: Env RoundAll
getRoundAll = getter RoundAll c'mpd_getcr

setRoundAll :: RoundAll -> Env ()
setRoundAll = setter "RoundAll" unRoundAll c'mpd_qsetcr

-- * Exponent limits

-- | The lowest possible exponent of a subnormal number
getExpTiny :: Env C'mpd_ssize_t
getExpTiny = getter id c'mpd_etiny

-- | The highest possible exponent of a normal number.  Only
-- relevant if Clamp is clampOn.
getExpTop :: Env C'mpd_ssize_t
getExpTop = withEnvPtr (\c -> c'mpd_etop c)

-----------------------------------------------------------
-- # Mpd
-----------------------------------------------------------

newtype Mpd = Mpd { unMpd :: ForeignPtr C'mpd_t }


mkMpd :: IO Mpd
mkMpd = do
  p <- c'mpd_qnew
  when (p == nullPtr) $ error "mkMpd: out of memory"
  fp <- newForeignPtr p'mpd_del p
  return $ Mpd fp

-- | Create new decimal.  Uninitialized.  TODO is this the right way
-- to check for null pointer?
newMpd :: Env Mpd
newMpd = Env $ \_ -> mkMpd

withMpdPtr :: Mpd -> (Ptr C'mpd_t -> IO a) -> Env a
withMpdPtr m f = Env $ \_ ->
  withForeignPtr (unMpd m) f

withMpdAndEnv
  :: Mpd
  -> (Ptr C'mpd_t -> Ptr C'mpd_context_t -> IO a)
  -> Env a
withMpdAndEnv m f = do
  ctx <- getEnvPtr
  withMpdPtr m (\mptr -> f mptr ctx)

-- | Sets the value of an MPD from a string.
setString
  :: BS.ByteString
  -> Env Mpd
setString bs = do
  m <- newMpd
  let f cstr = return $ withMpdAndEnv m
        (\pm pc -> c'mpd_set_string pm cstr pc)
  join $ liftIO $ BS.useAsCString bs f
  return m

-- | Sets the value of an MPD by converting an Integral to a string
-- first, then using setString.  Probably will not win any speed
-- awards.
setIntegral :: Integral a => a -> Env Mpd
setIntegral = setString . BS8.pack . show . f . fromIntegral
  where
    f = id :: Integer -> Integer

-- ## Setting the sign
newtype Sign = Sign { _unSign :: C'uint8_t }
  deriving (Eq, Show, Ord)

positive :: Sign
positive = Sign c'MPD_POS

negative :: Sign
negative = Sign c'MPD_NEG

newtype NonFinite = NonFinite { _unNonFinite :: C'uint8_t }
  deriving (Eq, Show, Ord)

infinite :: NonFinite
infinite = NonFinite c'MPD_INF

nan :: NonFinite
nan = NonFinite c'MPD_NAN

sNan :: NonFinite
sNan = NonFinite c'MPD_SNAN

setSpecial :: Sign -> NonFinite -> Env Mpd
setSpecial (Sign s) (NonFinite nf) = do
  m <- newMpd
  let f p = c'mpd_setspecial p s nf
  withMpdPtr m f
  return m

-- ## Convert Decimal

data ExpCase = ExpCase { unExpCase :: CInt }
  deriving (Eq, Show)

expLower :: ExpCase
expLower = ExpCase 0

expUpper :: ExpCase
expUpper = ExpCase 1

mpdToSci :: ExpCase -> Mpd -> Env BS.ByteString
mpdToSci c m = withMpdPtr m $ \mPtr -> do
  str <- c'mpd_to_sci mPtr (unExpCase c)
  bs <- BS.packCString str
  free str
  return bs


mpdToEng :: ExpCase -> Mpd -> Env BS.ByteString
mpdToEng c m = withMpdPtr m $ \mPtr -> do
  str <- c'mpd_to_eng mPtr (unExpCase c)
  bs <- BS.packCString str
  free str
  return bs


mpdFormat
  :: BS.ByteString
  -- ^ Format. The syntax is the same as in Python PEP 3101.

  -> Mpd
  -> Env BS.ByteString

mpdFormat bs m = withMpdAndEnv m $ \mPtr cPtr -> do
  r <- BS.useAsCString bs $ \strPtr ->
    c'mpd_format mPtr strPtr cPtr
  r' <- BS.packCString r
  free r
  return r'

adjExp :: Mpd -> Env Integer
adjExp m = withMpdPtr m $ \mPtr -> do
  r <- c'mpd_adjexp mPtr
  return . fromIntegral $ r

getAttr :: (Ptr C'mpd_t -> IO CInt) -> Mpd -> Env Bool
getAttr f m = withMpdPtr m $ \mPtr -> do
  r <- f mPtr
  return $ r /= 0

isFinite, isInfinite, isNan, isNegative, isPositive,
  isQnan, isSigned, isSnan, isSpecial, isZero :: Mpd -> Env Bool

isFinite = getAttr c'mpd_isfinite
isInfinite = getAttr c'mpd_isinfinite
isNan = getAttr c'mpd_isnan
isNegative = getAttr c'mpd_isnegative
isPositive = getAttr c'mpd_ispositive
isQnan = getAttr c'mpd_isqnan
isSigned = getAttr c'mpd_issigned
isSnan = getAttr c'mpd_issnan
isSpecial = getAttr c'mpd_isspecial
isZero = getAttr c'mpd_iszero

getCtxAttr
  :: (Ptr C'mpd_t -> Ptr C'mpd_context_t -> IO CInt)
  -> Mpd
  -> Env Bool
getCtxAttr f m = withMpdAndEnv m $ \pM pC -> do
  r <- f pM pC
  return $ r /= 0

isNormal, isSubnormal :: Mpd -> Env Bool
isNormal = getCtxAttr c'mpd_isnormal
isSubnormal = getCtxAttr c'mpd_issubnormal

data OddEven = IsOdd | IsEven

-- | Nothing for non-integral values.
isOddEven :: Mpd -> Env (Maybe OddEven)
isOddEven m = do
  b <- isInteger m
  if b
    then do
      o <- getAttr c'mpd_isodd m
      return . Just $ if o then IsOdd else IsEven
    else return Nothing

isInteger :: Mpd -> Env Bool
isInteger = getAttr c'mpd_isinteger 

sign :: Mpd -> Env Sign
sign m = withMpdPtr m $ \pM -> do
  r <- c'mpd_sign pM
  return $ Sign r

trailZeros :: Mpd -> Env Integer
trailZeros m = withMpdPtr m $ \pM -> fmap fromIntegral
  $ c'mpd_trail_zeros pM

getNonSpecialAttr :: (Ptr C'mpd_t -> IO CInt) -> Mpd -> Env (Maybe Bool)
getNonSpecialAttr f m = do
  s <- isSpecial m
  if s then return Nothing else do
    z <- getAttr f m
    return . Just $ z

-- | Nothing for special numbers
isZeroCoeff :: Mpd -> Env (Maybe Bool)
isZeroCoeff = getNonSpecialAttr c'mpd_iszerocoeff

isOddCoeff :: Mpd -> Env (Maybe Bool)
isOddCoeff = getNonSpecialAttr c'mpd_isoddcoeff

-- # Arithmetic

type Unary
  = Ptr C'mpd_t
  -> Ptr C'mpd_t
  -> Ptr C'mpd_context_t
  -> IO ()

type Binary
  = Ptr C'mpd_t
  -> Ptr C'mpd_t
  -> Ptr C'mpd_t
  -> Ptr C'mpd_context_t
  -> IO ()

type Ternary
  = Ptr C'mpd_t
  -> Ptr C'mpd_t
  -> Ptr C'mpd_t
  -> Ptr C'mpd_t
  -> Ptr C'mpd_context_t
  -> IO ()

unary
  :: Unary
  -> Mpd
  -> Env Mpd
unary f x = Env $ \pCtx -> do
  r <- unEnv newMpd pCtx
  withForeignPtr (unMpd r) $ \rPtr ->
          withForeignPtr (unMpd x) $ \xPtr ->
          f rPtr xPtr pCtx
  return r


binary
  :: Binary
  -> Mpd
  -> Mpd
  -> Env Mpd
binary f x y = Env $ \pCtx -> do
  r <- unEnv newMpd pCtx
  withForeignPtr (unMpd r) $ \rPtr ->
          withForeignPtr (unMpd x) $ \xPtr ->
          withForeignPtr (unMpd y) $ \yPtr ->
          f rPtr xPtr yPtr pCtx
  return r

ternary
  :: Ternary
  -> Mpd
  -> Mpd
  -> Mpd
  -> Env Mpd
ternary f x y z = Env $ \pCtx -> do
  r <- unEnv newMpd pCtx
  withForeignPtr (unMpd r) $ \rPtr ->
    withForeignPtr (unMpd x) $ \xPtr ->
    withForeignPtr (unMpd y) $ \yPtr ->
    withForeignPtr (unMpd z) $ \zPtr ->
    f rPtr xPtr yPtr zPtr pCtx
  return r

add :: Mpd -> Mpd -> Env Mpd
add = binary c'mpd_add

sub :: Mpd -> Mpd -> Env Mpd
sub = binary c'mpd_sub

mul :: Mpd -> Mpd -> Env Mpd
mul = binary c'mpd_mul

div :: Mpd -> Mpd -> Env Mpd
div = binary c'mpd_div

fma :: Mpd -> Mpd -> Mpd -> Env Mpd
fma = ternary c'mpd_fma

divInt :: Mpd -> Mpd -> Env Mpd
divInt = binary c'mpd_divint

rem :: Mpd -> Mpd -> Env Mpd
rem = binary c'mpd_rem

remNear :: Mpd -> Mpd -> Env Mpd
remNear = binary c'mpd_rem_near

divMod :: Mpd -> Mpd -> Env (Mpd, Mpd)
divMod x y = Env $ \cPtr -> do
  q <- unEnv newMpd cPtr
  r <- unEnv newMpd cPtr
  withForeignPtr (unMpd q) $ \qPtr ->
    withForeignPtr (unMpd r) $ \rPtr ->
    withForeignPtr (unMpd x) $ \xPtr ->
    withForeignPtr (unMpd y) $ \yPtr ->
    c'mpd_divmod qPtr rPtr xPtr yPtr cPtr
  return (q, r)

exp :: Mpd -> Env Mpd
exp = unary c'mpd_exp

ln :: Mpd -> Env Mpd
ln = unary c'mpd_ln

log10 :: Mpd -> Env Mpd
log10 = unary c'mpd_log10

pow :: Mpd -> Mpd -> Env Mpd
pow = binary c'mpd_pow

sqrt :: Mpd -> Env Mpd
sqrt = unary c'mpd_sqrt

invroot :: Mpd -> Env Mpd
invroot = unary c'mpd_invroot

minus :: Mpd -> Env Mpd
minus = unary c'mpd_minus

plus :: Mpd -> Env Mpd
plus = unary c'mpd_plus

abs :: Mpd -> Env Mpd
abs = unary c'mpd_abs

type Comparer
  = Ptr C'mpd_t
  -> Ptr C'mpd_t
  -> Ptr C'mpd_context_t
  -> IO CInt

noContextComparer
  :: (Ptr C'mpd_t -> Ptr C'mpd_t -> IO CInt)
  -> Comparer
noContextComparer f = \x y _ -> f x y

cmpCore 
  :: Comparer
  -> Mpd
  -> Mpd
  -> Env Ordering
cmpCore f x y = Env $ \cPtr -> do
  r <- withForeignPtr (unMpd x) $ \xPtr ->
    withForeignPtr (unMpd y) $ \yPtr ->
    f xPtr yPtr cPtr
  let g i | i == (-1) = LT
          | i == 0 = EQ
          | i == 1 = GT
          | otherwise = error "cmpCore: should never happen"
  return $ g r

cmp :: Mpd -> Mpd -> Env Ordering
cmp = cmpCore c'mpd_cmp

cmpTotal :: Mpd -> Mpd -> Env Ordering
cmpTotal = cmpCore (noContextComparer c'mpd_cmp_total)

cmpTotalMagnitude :: Mpd -> Mpd -> Env Ordering
cmpTotalMagnitude = cmpCore (noContextComparer c'mpd_cmp_total_mag)

max :: Mpd -> Mpd -> Env Mpd
max = binary c'mpd_max

maxMag :: Mpd -> Mpd -> Env Mpd
maxMag = binary c'mpd_max_mag

min :: Mpd -> Mpd -> Env Mpd
min = binary c'mpd_min

minMag :: Mpd -> Mpd -> Env Mpd
minMag = binary c'mpd_min_mag

nextMinus :: Mpd -> Env Mpd
nextMinus = unary c'mpd_next_minus

nextPlus :: Mpd -> Env Mpd
nextPlus = unary c'mpd_next_plus

nextToward :: Mpd -> Mpd -> Env Mpd
nextToward = binary c'mpd_next_toward

quantize :: Mpd -> Mpd -> Env Mpd
quantize = binary c'mpd_quantize

rescale :: C'mpd_ssize_t -> Mpd -> Env Mpd
rescale s m = Env $ \cPtr -> do
  r <- unEnv newMpd cPtr
  withForeignPtr (unMpd m) $ \aPtr ->
    withForeignPtr (unMpd r) $ \rPtr ->
    c'mpd_rescale rPtr aPtr s cPtr
  return r

sameQuantum :: Mpd -> Mpd -> Env Bool
sameQuantum x y = Env $ \_ ->
  withForeignPtr (unMpd x) $ \xPtr ->
  withForeignPtr (unMpd y) $ \yPtr -> do
    r <- c'mpd_same_quantum xPtr yPtr
    case () of
      _ | r == 1 -> return True
        | r == 0 -> return False
        | otherwise -> error "sameQuantum: should never happen"

reduce :: Mpd -> Env Mpd
reduce = unary c'mpd_reduce

roundToIntegralExact :: Mpd -> Env Mpd
roundToIntegralExact = unary c'mpd_round_to_intx

roundToIntegralValue :: Mpd -> Env Mpd
roundToIntegralValue = unary c'mpd_round_to_int

floor :: Mpd -> Env Mpd
floor = unary c'mpd_floor

ceiling :: Mpd -> Env Mpd
ceiling = unary c'mpd_ceil

truncate :: Mpd -> Env Mpd
truncate = unary c'mpd_trunc

logb :: Mpd -> Env Mpd
logb = unary c'mpd_logb

scaleb :: Mpd -> Mpd -> Env Mpd
scaleb = binary c'mpd_scaleb

powmod
  :: Mpd
  -- ^ Base
  -> Mpd
  -- ^ Exponent
  -> Mpd
  -- ^ Mod
  -> Env Mpd
powmod (Mpd b) (Mpd e) (Mpd m) = Env $ \pC ->
  withForeignPtr b $ \pB ->
  withForeignPtr e $ \pE ->
  withForeignPtr m $ \pM ->
  mkMpd >>= \r ->
  withForeignPtr (unMpd r) $ \pR ->
  c'mpd_powmod pR pB pE pM pC >>
  return r

shift :: Mpd -> Mpd -> Env Mpd
shift = binary c'mpd_shift

type SsizeArg a
  = Ptr C'mpd_t
  -> Ptr C'mpd_t
  -> C'mpd_ssize_t
  -> Ptr C'mpd_context_t
  -> IO a

ssizeArg
  :: SsizeArg a
  -> Mpd
  -> C'mpd_ssize_t
  -> Env (a, Mpd)
ssizeArg f m s = Env $ \cPtr ->
  withForeignPtr (unMpd m) $ \mPtr ->
  mkMpd >>= \r ->
  withForeignPtr (unMpd r) $ \rPtr ->
  f rPtr mPtr s cPtr >>= \fRes ->
  return (fRes, r)

shiftn :: C'mpd_ssize_t -> Mpd -> Env Mpd
shiftn s m = fmap snd $ ssizeArg c'mpd_shiftn m s

shiftl :: C'mpd_ssize_t -> Mpd -> Env Mpd
shiftl s m = fmap snd $ ssizeArg c'mpd_shiftl m s

shiftr :: C'mpd_ssize_t -> Mpd -> Env (C'mpd_uint_t, Mpd)
shiftr s m = ssizeArg c'mpd_shiftr m s

rotate :: Mpd -> Mpd -> Env Mpd
rotate = binary c'mpd_rotate

and :: Mpd -> Mpd -> Env Mpd
and = binary c'mpd_and

or :: Mpd -> Mpd -> Env Mpd
or = binary c'mpd_or

xor :: Mpd -> Mpd -> Env Mpd
xor = binary c'mpd_xor

invert :: Mpd -> Env Mpd
invert = unary c'mpd_invert
