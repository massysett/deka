{-# LANGUAGE Safe #-}
module Data.Dimes.Context
  ( -- * Context
    -- ** Initializing a context
    Initializer
  , maxContext
  , basicContext
  , defaultContext
  
  -- *** IEEE interchange formats
  , Interchange
  , interchange
  , unInterchange
  , interchangeInitializer

  -- *** Common interchange formats
  , interchange32
  , interchange64
  , interchange128

  -- ** Context monad
  , Context
  , runContext
  
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
  -- *** Signals
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

  -- *** Traps
  , Traps
  , setTraps
  , getTraps
  , trapNone
  , addSignal
  , trapSignals

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

  ) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Bindings.Mpdecimal
import Data.Bits
import Foreign.C.Types
import Foreign.Safe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

-- # Initializing a context

newtype Initializer = Initializer
  { unInitializer :: Ptr C'mpd_context_t -> IO () }

maxContext :: Initializer
maxContext = Initializer c'mpd_maxcontext

basicContext :: Initializer
basicContext = Initializer c'mpd_basiccontext

defaultContext :: Initializer
defaultContext = Initializer c'mpd_defaultcontext

-- * IEEE interchange formats

newtype Interchange = Interchange { unInterchange :: CInt }
  deriving (Eq, Ord, Show)

-- | Creates an Interchange.  The argument must be a multiple of 32
-- and less than IEEE_CONTEXT_MAX_BITS; fails if this is not true.
interchange :: Integral a => a -> Maybe Interchange
interchange a
  | a <= 0 = Nothing
  | a `rem` 32 /= 0 = Nothing
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
-- # Context monad

newtype Context a = Context
  { unContext :: Ptr C'mpd_context_t -> IO a }

instance Monad Context where
  return a = Context $ \_ -> return a
  a >>= f = Context $ \p -> do
    r1 <- unContext a p
    let b = unContext $ f r1
    b p

liftIO :: IO a -> Context a
liftIO i = Context $ \_ -> i

instance Functor Context where
  fmap = liftM

instance Applicative Context where
  pure = return
  (<*>) = ap

-- | Runs a Context computation.
runContext :: Initializer -> Context a -> IO a
runContext i f =
  bracket get release $ \hdlr -> do
    poke p'mpd_traphandler hdlr
    p <- mallocForeignPtr
    withForeignPtr p $ \ptr -> do
      unInitializer i ptr
      unContext f ptr
  where
    get = mk'funptr_mpd_traphandler (const $ return ())
    release = freeHaskellFunPtr

getContextPtr :: Context (Ptr C'mpd_context_t)
getContextPtr = Context $ \p -> return p

withContextPtr :: (Ptr C'mpd_context_t -> IO a) -> Context a
withContextPtr f = do
  p <- getContextPtr
  liftIO $ f p

-- # Context helpers
getter
  :: (a -> b)
  -> (Ptr C'mpd_context_t -> IO a)
  -> Context b
getter f g = fmap f $ withContextPtr g

setter
  :: String
  -- ^ For error message
  -> (a -> b)
  -> (Ptr C'mpd_context_t -> b -> IO CInt)
  -> a
  -> Context ()
setter s f g a = do
  e <- withContextPtr (\c -> g c (f a))
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

getPrecision :: Context Precision
getPrecision = getter Precision c'mpd_getprec

setPrecision :: Precision -> Context ()
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

getPosExpMax :: Context PosExpMax
getPosExpMax = getter PosExpMax c'mpd_getemax

setPosExpMax :: PosExpMax -> Context ()
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

getNegExpMin :: Context NegExpMin
getNegExpMin = getter NegExpMin c'mpd_getemin

setNegExpMin :: NegExpMin -> Context ()
setNegExpMin = setter "NegExpMin" unNegExpMin c'mpd_qsetemin

-- # Traps

newtype Traps = Traps { unTraps :: C'uint32_t }
  deriving (Eq, Show, Ord)

setTraps :: Traps -> Context ()
setTraps = setter "Traps" unTraps c'mpd_qsettraps

getTraps :: Context Traps
getTraps = fmap Traps (withContextPtr c'mpd_gettraps)

trapNone :: Traps
trapNone = Traps 0

addSignal :: Traps -> Signal -> Traps
addSignal (Traps t) (Signal s) = Traps $ t .|. s

trapSignals :: [Signal] -> Traps
trapSignals = foldl addSignal trapNone

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

getRound :: Context Round
getRound = getter Round c'mpd_getround

setRound :: Round -> Context ()
setRound = setter "Round" unRound c'mpd_qsetround

-- * Clamp
newtype Clamp = Clamp { unClamp :: CInt }
  deriving (Eq, Ord, Show)

clampOn :: Clamp
clampOn = Clamp 1

clampOff :: Clamp
clampOff = Clamp 0

getClamp :: Context Clamp
getClamp = getter Clamp c'mpd_getclamp

setClamp :: Clamp -> Context ()
setClamp = setter "Clamp" unClamp c'mpd_qsetclamp

-- * Round all functions correctly

newtype RoundAll = RoundAll { unRoundAll :: CInt }
  deriving (Eq, Ord, Show)

roundAllOn :: RoundAll
roundAllOn = RoundAll 1

roundAllOff :: RoundAll
roundAllOff = RoundAll 0

getRoundAll :: Context RoundAll
getRoundAll = getter RoundAll c'mpd_getcr

setRoundAll :: RoundAll -> Context ()
setRoundAll = setter "RoundAll" unRoundAll c'mpd_qsetcr

-- * Exponent limits

-- | The lowest possible exponent of a subnormal number
getExpTiny :: Context C'mpd_ssize_t
getExpTiny = getter id c'mpd_etiny

-- | The highest possible exponent of a normal number.  Only
-- relevant if Clamp is clampOn.
getExpTop :: Context C'mpd_ssize_t
getExpTop = withContextPtr (\c -> c'mpd_etop c)

-----------------------------------------------------------
-- # Mpd
-----------------------------------------------------------

newtype Mpd = Mpd { unMpd :: ForeignPtr C'mpd_t }

-- | Create new decimal.  Uninitialized.  TODO is this the right way
-- to check for null pointer?
newMpd :: Context Mpd
newMpd = Context $ \_ -> do
  p <- c'mpd_qnew
  when (p == nullPtr) $ error "newMpd: out of memory"
  fp <- newForeignPtr p'mpd_del p
  return $ Mpd fp

withMpdPtr :: Mpd -> (Ptr C'mpd_t -> IO a) -> Context a
withMpdPtr m f = Context $ \_ ->
  withForeignPtr (unMpd m) f

withMpdAndContext
  :: Mpd
  -> (Ptr C'mpd_t -> Ptr C'mpd_context_t -> IO a)
  -> Context a
withMpdAndContext m f = do
  ctx <- getContextPtr
  withMpdPtr m (\mptr -> f mptr ctx)

-- | Sets the value of an MPD from a string.
setString
  :: BS.ByteString
  -> Context Mpd
setString bs = do
  m <- newMpd
  let f cstr = return $ withMpdAndContext m
        (\pm pc -> c'mpd_set_string pm cstr pc)
  join $ liftIO $ BS.useAsCString bs f
  return m

setIntegral :: Integral a => a -> Context Mpd
setIntegral = setString . BS8.pack . show . f . fromIntegral
  where
    f = id :: Integer -> Integer
