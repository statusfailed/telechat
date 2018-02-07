module Network.Telechat.Machines where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Machine
import Data.Machine.Attoparsec (parsingMaybe)

import Data.ByteString
import Data.Attoparsec.ByteString

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
