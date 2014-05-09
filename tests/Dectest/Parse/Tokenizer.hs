{-# LANGUAGE OverloadedStrings #-}
-- | A finite state machine to break a single line of text into
-- tokens.
--
-- The machine has three main states, some of which have sub-states:
--
-- * inside of a token. Here, the machine can be in a @plain@ (that
-- is, unquoted) word, or it can be inside of a quoted word.  When
-- inside of a quoted word, if a closing quotation mark is seen, the
-- machine transitions to being in a @pending@ state.  The next
-- character determines what happens next: if it's an identical
-- quotation mark, then a single quotation mark is added to the
-- current token, and the machine stays in a quoted word state.  If
-- the character is anything else, the machine outputs the previous
-- token and begins a new one.
--
-- * between tokens.
--
-- * in a comment.  Before a token is output, the tokenizer examines
-- it to determine whether it is both (1) unquoted, and (2) begins
-- with two dashes.  If so, it's a comment token.  The token is not
-- output, and the machine shifts to being in a comment state, which
-- swallows all remaining characters that are input.
--
-- To use the tokenizer:
--
-- * turn it on with 'start'.
--
-- * Feed it characters from the line using 'feed'.  Occasionally
-- this will output a single 'Token'.  It always outputs a new
-- 'Tokenizer' that will accept further characters.
--
-- * when done, be sure to turn it off with 'finish'.  This will
-- eject any characters in the tokenizer that have not been spit out
-- yet.

module Dectest.Parse.Tokenizer
  ( Tokenizer
  , Token(..)
  , start           -- :: Tokenizer
  , feed            -- :: Char -> Tokenizer -> (Tokenizer, Maybe Token)
  , finish          -- :: Tokenizer -> Maybe Token
  ) where

import qualified Data.ByteString.Char8 as BS8

-- | Set to True when a single close quote character has been
-- parsed.  Since double quote characters indicates an enclosed
-- quote, we don't know until the next character whether to close
-- the quote or just include a quote in the token.
type Pending = Bool

data QuoteType = Single | Double
  deriving (Eq, Show)

toQuot :: QuoteType -> Char
toQuot Single = '\''
toQuot Double = '"'

data InTok
  = PlainWord
  | Quoted QuoteType Pending
  deriving Show

data Accepting
  = InTok InTok BS8.ByteString
  | BetweenToks
  deriving Show

-- The Tokenizer holds a Maybe Accepting, which is Nothing if the
-- Tokenizer is no longer accepting new characters because it is in
-- an comment, or is Just if accepting new characters.

data Tokenizer = Tokenizer (Maybe Accepting)
  deriving Show

data Token = Token
  { unToken :: BS8.ByteString
  , quoted :: Bool
  } deriving (Eq, Ord, Show)

-- | Turns the tokenizer on.

start :: Tokenizer
start = Tokenizer . Just $ BetweenToks

-- | Feeds a character to the tokenizer.
feed :: Char -> Tokenizer -> (Tokenizer, Maybe Token)
feed c (Tokenizer t) = case t of
  Nothing -> (Tokenizer Nothing, Nothing)

  Just a -> case a of
    InTok tokType curr -> case tokType of

      PlainWord ->
        let mnt q = mint (InTok (Quoted q False) BS8.empty)
                          curr False
        in case c of
            '"' -> mnt Double
            '\'' -> mnt Single
            ' ' -> mint BetweenToks curr False
            _ -> stayInTok c curr PlainWord

      Quoted qType pend
        | pend -> acceptQuotedWithPending c curr qType
        | otherwise -> acceptQuotedNoPending c curr qType

    BetweenToks -> case c of
      ' ' -> (Tokenizer (Just BetweenToks), Nothing)
      '"' -> newTok (Left Double)
      '\'' -> newTok (Left Single)
      _ -> newTok (Right c)

-- | Creates a new token, if called for.  Checks to see if the token
-- that would be produced would be a comment token.  If so, do not
-- emit a token, and return a Tokenizer that does not accept new
-- characters.  Otherwise, emit the token, and return an accepting
-- Tokenizer.

mint
  :: Accepting
  -- ^ If a token is produced because the Tokenizer did not output a
  -- comment, this becomes the new state of the Tokenizer.
  -> BS8.ByteString
  -- ^ What to output
  -> Bool
  -- ^ True if the token is quoted; False if not
  -> (Tokenizer, Maybe Token)
mint a o q
  | q = nt
  | otherwise =
      if BS8.take 2 o == "--"
      then (Tokenizer Nothing, Nothing)
      else nt
  where
    nt = (Tokenizer (Just a), Just (Token o q))

-- | Accepts a new character without producing a token.

stayInTok
  :: Char
  -- ^ Character to add to the storehouse of characters in the
  -- current token
  -> BS8.ByteString
  -- ^ Storehouse of current characters
  -> InTok
  -> (Tokenizer, Maybe Token)
  -- ^ snd is always False, fst is a Tokenizer that is InTok
stayInTok c s i = (tzr, Nothing)
  where
    tzr = Tokenizer (Just ac)
    ac = InTok i (s `BS8.snoc` c)

-- | Accepts a character while within a quote and there is not a
-- pending character.

acceptQuotedNoPending
  :: Char
  -- ^ Character to accept
  -> BS8.ByteString
  -- ^ Storehouse of current characters
  -> QuoteType
  -- ^ Type of quote we're currently inside of
  -> (Tokenizer, Maybe Token)
  -- ^ snd is always False, fst is a Tokenizer that is InTok
acceptQuotedNoPending c s q = (tzr, Nothing)
  where
    tzr = Tokenizer (Just ac)
    ac = InTok (Quoted q pnd) s'
    (s', pnd)
      | c == toQuot q = (s, True)
      | otherwise = (s `BS8.snoc` c, False)

-- | Accepts a character while within a quote and there is a pending
-- character.
acceptQuotedWithPending
  :: Char
  -- ^ Character to accept
  -> BS8.ByteString
  -- ^ Storehouse of current characters
  -> QuoteType
  -- ^ Type of quote we're currently inside of
  -> (Tokenizer, Maybe Token)
  -- ^ snd is always False, fst is a Tokenizer that is InTok
acceptQuotedWithPending c s q
  | c == toQuot q = stayInTok c s (Quoted q False)
  | otherwise = case c of
      ' ' -> mnt BetweenToks
      '"' -> mnt (InTok (Quoted Double False) BS8.empty)
      '\'' -> mnt (InTok (Quoted Single False) BS8.empty)
      _ -> mnt (InTok PlainWord (BS8.singleton c))
  where
    mnt a = mint a s True

-- | Starts a new token.
newTok
  :: Either QuoteType Char
  -- ^ If Left, start a new quoted token.  If Right, start a new
  -- plain token with the given character.
  -> (Tokenizer, Maybe Token)
  -- ^ snd is always False, fst if an accepting Tokenizer
newTok e = (Tokenizer (Just (InTok i s)), Nothing)
  where
    (i, s) = case e of
      Left q -> (Quoted q False, BS8.empty)
      Right c -> (PlainWord, BS8.singleton c)

-- | Finishes any remaining tokens in the machine.  Applies 'error'
-- if there is a quoted token in the machine that has not been
-- finished yet.
finish :: Tokenizer -> Maybe Token
finish (Tokenizer ma) = case ma of
  Nothing -> Nothing
  Just a -> case a of
    BetweenToks -> Nothing
    InTok i s -> case i of
      PlainWord -> Just (Token s False)
      Quoted _ pnd
        | pnd -> Just (Token s True)
        | otherwise -> error "finish: quoted token still in machine"
