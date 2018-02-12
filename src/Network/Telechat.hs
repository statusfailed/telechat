{-# LANGUAGE OverloadedStrings #-}
module Network.Telechat
  ( module Network.Telechat
  , module Network.Telechat.Types
  , module Network.Telechat.Telnet
  , module Network.Telechat.Machines
  ) where

import Control.Exception.Base
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forever)
import Data.IORef (IORef(..), readIORef, newIORef, modifyIORef)

import Network.Transport (Transport(..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import Data.Machine hiding (Process)

import qualified Data.ByteString as BS

import Network.Socket
  ( Socket(..), socket, bind, listen, accept
  , SocketType(..), SockAddr(..), setSocketOption, SocketOption(..)
  , Family(..), iNADDR_ANY
  )

import Network.Socket.ByteString as Socket (sendAll, recv)

import Network.Telechat.Types
import Network.Telechat.Telnet
import Network.Telechat.Machines

recvBufSize :: Int
recvBufSize = 2048

-- | Listen for data from a child socket; broadcast all data to other children.
childReceiver :: ProcessId -> Socket -> Process ()
childReceiver writerPid sock = runT_
  $  readingSocket sock recvBufSize
  ~> autoM (\buf -> say ("raw bytes: " ++ show buf) >> return buf)
  ~> readingMachine
  ~> autoM (\msg -> say ("got msg: " ++ show msg) >> return msg)
  ~> autoM (send writerPid)

-- | Listen for process messages and write to socket
childSender :: IORef [ProcessId] -> Socket -> Process ()
childSender allWriterPids sock = do
  say "setting RAW mode"
  liftIO $ Socket.sendAll sock telnet_SET_RAW_MODE
  runT_
    $  expecting
    ~> autoM (\cmd -> say ("got command: " ++ show cmd) >> return cmd)
    ~> writingMachine broadcast
    ~> writingSocket sock
  where
    broadcast msg = liftIO (readIORef allWriterPids) >>= mapM_ (flip send msg)
    expecting = repeatedly (lift (expect :: Process Command) >>= yield)

-- | Spawn a pair of processes, reading and writing from a client socket connection
-- respectively.
-- The processes are linked, and will both die if one does.
spawnChild :: IORef [ProcessId] -> (Socket, SockAddr) -> Process ()
spawnChild cref (sock, _) = do
  -- Create the sender process
  senderPid <- spawnLocal $ do
    childSender cref sock

  -- add the sender to the set of sender processes
  liftIO $ modifyIORef cref (\ps -> senderPid:ps)

  -- create the receiver process
  receiverPid <- spawnLocal $ do
    link senderPid -- pair child processes so both die together
    childReceiver senderPid sock

  return ()

-- | Main loop. Creates a socket, binds it to port 4444, and accepts
-- new connections. On connect, runs 'spawnChild'
listener :: IORef [ProcessId] -> Process ()
listener cref = do
  sock <- liftIO $ socket AF_INET Stream 0
  liftIO $ do
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4444 iNADDR_ANY)
    listen sock 4096 -- listen for up to 4K connections

  forever $ do
    conn <- liftIO $ accept sock
    say $ "listener got connection from " ++ show (snd conn)
    -- spawn process with connection
    spawnChild cref conn
