{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Telechat.Types where

import Data.Text
import Data.Monoid
import GHC.Generics
import Data.Binary

-- | A 'Command' tells the sender process what to do.
data Command
  = Chat Text
  -- ^ A chat message for immediate send
  | Input Text
  -- ^ Input is textual input from the client.
  | Send
  -- ^ 'Send' indicates the current client pressed enter, or some equivalent.
  | Backspace
  -- ^ Backspace signals the intention to delete the last character in the
  -- buffer.
  | Clear
  -- ^ Clear the buffer.
  deriving(Eq, Ord, Read, Show, Generic)

instance Binary Command


-- | The state of the writer process. Allows redrawing the client terminal.
data WriterState = WriterState
  { clientBuffer :: Text
  } deriving(Eq, Ord, Read, Show)

instance Monoid WriterState where
  mempty = WriterState ""
  mappend (WriterState bufA) (WriterState bufB) = WriterState (bufA <> bufB)
