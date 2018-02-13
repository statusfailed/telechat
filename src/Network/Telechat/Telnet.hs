{-# LANGUAGE OverloadedStrings #-}
module Network.Telechat.Telnet
  ( telnet_SET_RAW_MODE
  , telnetData
  , telnetDataParser
  ) where

import Data.Maybe
import Control.Applicative
import Control.Monad

import Data.Attoparsec.ByteString
import Data.ByteString as BS

-- This module contains raw telnet commands (as bytes) which are used to put
-- the client into "raw" mode, with no local echoing of characters.

-- We briefly pretend to be telnet-aware in order to tell the
-- client to use unbuffered input, which lets us build an interactive prompt
-- properly
-- see: https://stackoverflow.com/questions/4532344/
--
-- and https://tools.ietf.org/html/rfc1184
-- and https://tools.ietf.org/html/rfc854
-- and https://tools.ietf.org/html/rfc857

-- Final command to set raw mode.
telnet_SET_RAW_MODE :: ByteString
telnet_SET_RAW_MODE = pack
  -- IAC DO LINEMODE
  -- sender (us) requests the remote side
  -- "Begin subnegotiation of editing status" (P4, RFC1184)
  [ 255, 253, 34

  -- IAC SB LINEMODE MODE 0
  -- the "0" is a bitmask. By turning off everything, we basically tell
  -- the client to be a dumb byte-entering-machine.
  , 255, 250, 34, 1, 0

  -- IAC 240, page 14 https://tools.ietf.org/html/rfc854
  -- "End of parameters"
  , 255, 240

  -- IAC WILL ECHO
  -- "we'll output characters for this terminal, don't you worry!"
  -- more or less instructs the client to stop echoing locally.
  -- P1, RFC857
  , 255, 251, 1
  ]


-- sb is "begin subnegotiation"
-- page 14, RF854: https://tools.ietf.org/html/rfc854
iacSe = word8 255 >> word8 240 -- IAC SB

-- | Read bytes until the IAC SE sequence is encountered
untilIacSe :: Parser ()
untilIacSe = do
  takeWhile1 (/= 255)
  void iacSe <|> void untilIacSe

-- | Parse and ignore a telnet command
telnetCommand :: Parser (Maybe ByteString)
telnetCommand = do
  word8 255 -- IAC
  code <- anyWord8
  case code of
    255 -> return $ Just "\255" -- doubling up means a literal

    -- SB is "begin subnegotiation" - so eat bytes until SE: end subng.
    250 -> untilIacSe >> return Nothing

    -- Other commands are just a single byte (sequence goes IAC, Code, Arg)
    _   -> anyWord8 >> return Nothing

rawData :: Parser (Maybe ByteString)
rawData = Just . BS.singleton <$> satisfy (/= 255)

telnetData :: Parser (Maybe ByteString)
telnetData = telnetCommand <|> rawData

-- | Parse raw data from a telnet connection, by ignoring all telnet commands.
telnetDataParser :: Parser [ByteString]
telnetDataParser = catMaybes <$> many' telnetData
