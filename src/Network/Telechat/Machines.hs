{-# LANGUAGE RankNTypes #-}
module Network.Telechat.Machines where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Machine
import Data.Machine.Attoparsec (parsingMaybe)

import Data.ByteString as BS
import Data.Attoparsec.ByteString

import Data.Text
import Data.Text.Encoding (Decoding(..), streamDecodeUtf8)

import Network.Socket (Socket(..))
import Network.Socket.ByteString as Socket (sendAll, recv)

-- Data pipeline:
--  read from socket
--  send buffer to sender process - "EchoToClient ByteString"
--  feed line parser, emit lines
--  message to sender, "ClientCommand Line"

import Network.Telechat.Telnet (telnetDataParser)

-- | A 'ProcessT' to read chunks of data, strip telnet commands from them, and
-- produce only the raw data sent by the client.
strippingTelnet :: Monad m => ProcessT m ByteString ByteString
strippingTelnet = parsingMaybe telnetDataParser ~> results ~> flattened
  where results = repeatedly (await >>= maybe mzero yield)

-- | Read 'ByteString' input and produce 'Text' output as soon
-- as possible.
decodingText :: Monad m => ProcessT m ByteString Text
decodingText = construct (go streamDecodeUtf8)
  where
    go k = do
      (Some r _ k') <- k <$> await
      yield r
      go k'

readingSocket :: MonadIO m => Socket -> Int -> SourceT m ByteString
readingSocket sock recvBufSize = construct go
  where
    go = do
      r <- liftIO $ Socket.recv sock recvBufSize
      unless (BS.null r) $ yield r >> go

-- | The 'ProcessT' for a child socket-reading process.
-- 'readingMachine sock n' reads chunks of size 'n' from socket 'sock', and parses
-- out only raw client data, with no telnet commands.
readingMachine :: MonadIO m => Socket -> Int -> SourceT m Text
readingMachine sock recvBufSize
  =  readingSocket sock recvBufSize
  ~> strippingTelnet
  ~> decodingText
  {-~> parsingMaybe parseCommand-}
