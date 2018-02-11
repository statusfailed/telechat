module Network.Telechat.Types where

import Data.Text

-- | A 'Command' tells the sender process what to do.
data Command
  = Chat Text
  -- ^ A chat message for immediate send
  | Input Text
  -- ^ Input is textual input from the client.
  | Send
  -- ^ 'Send' indicates the current client pressed enter, or some equivalent.
  | Backspace
  -- ^ Backspace signals the intention to delete the last character in the buffer.
  deriving(Eq, Ord, Read, Show)
