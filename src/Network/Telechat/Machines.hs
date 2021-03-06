{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Telechat.Machines where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid

import Data.Machine
import Data.Machine.Attoparsec (parsingText, parsingMaybe, parsingEither)

import Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.Attoparsec.Text as AT

import Data.Text as Text
import Data.Text.Encoding (Decoding(..), streamDecodeUtf8, encodeUtf8)

import Network.Socket (Socket(..))
import Network.Socket.ByteString as Socket (sendAll, recv)

-- Data pipeline:
--  read from socket
--  send buffer to sender process - "EchoToClient ByteString"
--  feed line parser, emit lines
--  message to sender, "ClientCommand Line"

import Network.Telechat.Telnet (telnetData)
import Network.Telechat.Types
import Network.Telechat.Commands
import qualified Network.Telechat.Terminal as Terminal

import Debug.Trace (trace)

-- | Print every value going through the machine, and append a fixed message to
-- the start (can be empty string)
printing :: MonadIO m => Show a => String -> ProcessT m a a
printing = autoM . f
  where
    f msg x = do
      liftIO . Prelude.putStrLn $ msg ++ show x
      return x

-- | Debug.Trace.trace every value going through the pipeline with a fixed
-- message prepended.
tracing :: (Monad m, Show a) => String -> ProcessT m a a
tracing = mapping . f
  where f msg x = trace (msg ++ show x) x

-- | Read chunks of bytes from a sock. Ask for up to recvBufSize each time.
readingSocket :: MonadIO m => Socket -> Int -> SourceT m ByteString
readingSocket sock recvBufSize = construct go
  where
    go = do
      r <- liftIO $ Socket.recv sock recvBufSize
      unless (BS.null r) $ yield r >> go

-- | Repeatedly wait for input and write it all to a socket. Never buffers.
-- TODO: error checking?
writingSocket :: MonadIO m => Socket -> ProcessT m ByteString a
writingSocket sock = repeatedly (await >>= liftIO . Socket.sendAll sock)

-- | A 'ProcessT' to read chunks of data, strip telnet commands from them, and
-- produce only the raw data sent by the client.
strippingTelnet :: Monad m => ProcessT m ByteString ByteString
strippingTelnet = parsingMaybe telnetData ~> results ~> flattened
  where
    results = repeatedly (await >>= maybe mzero yield)

-- | Read 'ByteString' input and produce 'Text' output as soon
-- as possible.
decodingText :: Monad m => ProcessT m ByteString Text
decodingText = construct (go streamDecodeUtf8)
  where
    go k = do
      (Some r _ k') <- k <$> await
      yield r
      go k'

droppingLefts :: Monad m => ProcessT m (Either l r) r
droppingLefts = repeatedly (await >>= go)
  where go x = case x of
          Left err -> return ()
          Right v  -> yield v

droppingNothings :: Monad m => ProcessT m (Maybe a) a
droppingNothings = repeatedly (await >>= go)
  where go x = case x of { Nothing -> return (); Just v -> yield v }

-- | The 'ProcessT' for a child socket-reading process.
-- 'readingMachine sock n' reads chunks of size 'n' from socket 'sock', and parses
-- out only raw client data, with no telnet commands.
readingMachine :: Monad m => ProcessT m ByteString Command
readingMachine
  =  strippingTelnet
  ~> decodingText
  ~> fmap AT.eitherResult (parsingText command)
  ~> droppingLefts
  ~> droppingNothings


-- | Given a set of 'Command's, output bytes to be written to the client.
-- the only argument is a function which reprents the action of broadcasting
-- text to all writers, including this one.
--
-- NOTE: this is pretty inefficient - lots of 'Text' appending.
-- TODO: use a decent editor data-structure.
writingMachine :: Monad m => (Text -> m ()) -> ProcessT m Command ByteString
writingMachine broadcast = construct (yield greeting >> go mempty)
  where
    greeting = "say something > "

    go (WriterState buf) = do
      cmd <- await
      case cmd of
        Chat text  -> chat text >> render buf >> go (WriterState buf)
        Input text -> input text >> go (WriterState $ buf <> text)
        Send       -> send buf >> go (WriterState Text.empty)
        Backspace  -> let buf' = if Text.null buf then "" else Text.init buf
                      in render buf' >> go (WriterState buf')
        Clear      -> clear

    clear = render Text.empty >> go (WriterState Text.empty)

    -- Send the current buffer, unless it's empty.
    send buf = do
      unless (Text.null buf) $ do
        lift (broadcast buf)
        yield Terminal.wipeLine

    input text = do
      yield (encodeUtf8 text)

    -- Re-render the current client screen.
    render buf = do
      yield Terminal.wipeLine
      yield greeting
      yield (encodeUtf8 buf)

    -- "Chat" uses terminal codes to...
    --    1. wipe the current line
    --    2. output the chat message
    chat text = do
      yield Terminal.wipeLine
      yield (encodeUtf8 text)
      yield "\r\n"
