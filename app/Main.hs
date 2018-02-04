{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef (IORef(..), readIORef, newIORef, modifyIORef)
import Control.Exception.Base

import Control.Distributed.Process.Node
  (newLocalNode, initRemoteTable, runProcess)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport (Transport(..))

import Network.Telechat

-- | Create the CloudHaskell transport.
getTransport :: String -> String -> IO (Either IOException Transport)
getTransport hostname port =
  createTransport hostname port externalAddress defaultTCPParameters
  -- not quite sure what this function is really for...
  where externalAddress bindPort = (hostname, port)

-- | Main program: run the main node listening for connections
main :: IO ()
main = do
  Right t <- getTransport "127.0.0.1" "10501"
  node    <- newLocalNode t initRemoteTable
  cref    <- newIORef []
  runProcess node $ listener cref
