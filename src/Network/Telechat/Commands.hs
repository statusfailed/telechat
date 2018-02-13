module Network.Telechat.Commands where

import Data.Char
import Data.Text as Text
import Control.Applicative
import Data.Attoparsec.Text

import Network.Telechat.Types

-- 0x7F is DEL, but Telnet sends it when I press backspace
backspace :: Parser Command
backspace = char (chr 0x7F) >> return Backspace

send :: Parser Command
send = do
  char (chr 0x0d)
  char (chr 0x00)
  return Send

-- | Strip unprintable characters from input.
-- Always returns nothing if unprintable characters are parsed.
unreadable :: Parser (Maybe a)
unreadable = takeWhile1 (not . isPrint) >> return Nothing

-- | Parse 'Input' - printable characters
input :: Parser Command
input = Input . Text.singleton <$> satisfy isPrint

-- | Try to parse a command
command :: Parser (Maybe Command)
command
  =   fmap Just backspace
  <|> fmap Just send
  <|> unreadable
  <|> fmap Just input
