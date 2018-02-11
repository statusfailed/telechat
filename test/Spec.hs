{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
{-import Test.Tasty.SmallCheck as SC-}
{-import Test.Tasty.QuickCheck as QC-}
import Test.Tasty.HUnit

import Network.Telechat.Telnet (telnet_SET_RAW_MODE, telnetDataParser)
import Data.Monoid
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString as BS
import qualified Data.Text as Text

-- testParsingText
import Data.Machine (run, supply)
import Network.Telechat.Machines (decodingText)

-- testParsingCommands
import Network.Telechat.Types (Command(..))
import Network.Telechat.Commands (command)
import Data.Machine.Attoparsec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testTelnetParser
  , testParsingText
  , testParsingCommands
  ]

testTelnetParser = testGroup "Parser unit tests"
  [ testCase "Check simple telnet commands are stripped from input" $
      Right ["abc", "def"] @=? (parseOnly telnetDataParser $
        ("abc" <> "\255\240\1" <> "def"))
  , testCase "Check simple telnet commands are stripped from input" $
      Right ["abc", "def"] @=? (parseOnly telnetDataParser $
        ("abc" <> telnet_SET_RAW_MODE <> "def"))
  , testCase "Check 0xFF data bytes are preserved" $
      Right ["abc", "\255", "def"] @=? (parseOnly telnetDataParser $
        ("abc\255\255" <> telnet_SET_RAW_MODE <> "def"))
  ]

testParsingText = testGroup "Test parsing UTF-8"
  [ testCase "Check parsingTelnet works with split unicode data snowman" $
      "snowman: ☃" @=? (Text.concat . run $ supply ["snowman: \xe2", "\x98\x83"] decodingText)
  ]

testParsingCommands = testGroup "Test parsing commands from UTF-8 strings"
  [ testCase "Parse test sequence" $ 
      Right [Just (Input "hi."), Just Backspace, Just Send, Nothing, Just (Input ".")]
      @=?
      (sequence . run $ supply (["hi.\x7F\x00\x0d\x03\x04." :: Text.Text]) (AT.eitherResult <$> parsingText command))
  , testCase "parse split-up test sequence" $ 
      Right [Just (Input "hi."), Just Backspace, Just Send, Nothing, Just (Input ".")]
      @=?
      (sequence . run $ supply (["hi.\x7F\x00" :: Text.Text, "\x0d\x03", "\x04."]) (AT.eitherResult <$> parsingText command))
  ]
