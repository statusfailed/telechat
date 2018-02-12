{-# LANGUAGE OverloadedStrings #-}
module Network.Telechat.Terminal where

import Data.ByteString

ansiEL0  = "\ESC[\NULK"

-- https://en.wikipedia.org/wiki/ANSI_escape_code
wipeLine :: ByteString
wipeLine = ansiEL0
