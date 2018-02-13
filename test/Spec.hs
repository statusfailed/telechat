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
import Network.Telechat.Machines (decodingText, writingMachine, readingMachine)

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
  , testWritingMachine
  , testReadingMachine
  ]

-- TODO: test if this command sequence breaks the readingMachine
-- This is what my (Ubuntu) telnet client sends back after receiving the raw mode command.
-- Make sure it's parsed (and therefore stripped) correctly by the telnet parser
rawModeResponse :: BS.ByteString
rawModeResponse = "\255\253\ETX\255\251\"\255\250\"\ETX\SOH\NUL\NUL\ETXb\ETX\EOT\STX\SI\ENQ\NUL\NUL\ab\FS\b\STX\EOT\tB\SUB\n\STX\DEL\v\STX\NAK\SI\STX\DC1\DLE\STX\DC3\DC1\NUL\NUL\DC2\NUL\NUL\255\240\255\253\SOH"

testTelnetParser = testGroup "Parser unit tests"
  [ testCase "Check simple telnet commands are stripped from input" $
      Right ["a", "b", "c", "d", "e", "f"] @=? (parseOnly telnetDataParser $
        ("abc" <> "\255\240\1" <> "def"))
  , testCase "Check simple telnet commands are stripped from input" $
      Right ["a", "b", "c", "d", "e", "f"] @=? (parseOnly telnetDataParser $
        ("abc" <> telnet_SET_RAW_MODE <> "def"))
  , testCase "Check 0xFF data bytes are preserved" $
      Right ["a", "b", "c", "\255", "d", "e", "f"] @=? (parseOnly telnetDataParser $
        ("abc\255\255" <> telnet_SET_RAW_MODE <> "def"))
  , testCase "Check raw mode response is stripped" $
      Right ["a", "b", "c"] @=? (parseOnly telnetDataParser $ rawModeResponse <> "abc")
  ]

testParsingText = testGroup "Test parsing UTF-8"
  [ testCase "Check parsingTelnet works with split unicode data snowman" $
      "snowman: ☃" @=? (Text.concat . run $ supply ["snowman: \xe2", "\x98\x83"] decodingText)
  ]

testParsingCommands = testGroup "Test parsing commands from UTF-8 strings"
  [ testCase "Parse test sequence" $ 
      Right [Just (Input "!"), Just Backspace, Just Send, Nothing, Just (Input ".")]
      @=?
      (sequence . run $ supply (["!\x7F\x0d\x00\x03\x04." :: Text.Text]) (AT.eitherResult <$> parsingText command))
  , testCase "parse split-up test sequence" $ 
      Right [Just (Input "!"), Just Backspace, Just Send, Nothing, Just (Input ".")]
      @=?
      (sequence . run $ supply (["!\x7F\x0d" :: Text.Text, "\x00\x03", "\x04."]) (AT.eitherResult <$> parsingText command))
  ]

testReadingMachine = testGroup "Test the client reader machine"
  [ testCase "Check the raw mode response + input results in a command" $ 
      [Input "a", Input "b", Input "c"]
      @=?
      (run . supply [rawModeResponse <> "abc"] $ readingMachine)
  , testCase "Check simple newlines are correct" $ 
      [Send, Input "a", Input "b", Input "c"]
      @=?
      (run . supply ["\r\NUL" <> "abc"] $ readingMachine)
  ]

testWritingMachine = testGroup "Test editing works for clients"
  [ testCase "Basic test that backspace outputs correct ANSI sequence" $ 
      "abcd\ESC[\NULKabc"
      @=?
      (BS.concat . run . supply [Input "abcd", Backspace] $
        writingMachine (const $ return ()))
  ]
