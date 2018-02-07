module Network.Telechat.Types where

import Data.ByteString

-- | A 'Command' tells a sender process what to do next.
data Command = Chat ByteString | Input ByteString
