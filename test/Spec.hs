{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
{-import Test.Tasty.SmallCheck as SC-}
{-import Test.Tasty.QuickCheck as QC-}
import Test.Tasty.HUnit

import Network.Telechat.Telnet (telnet_SET_RAW_MODE, telnetDataParser)
import Data.Monoid
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.Text as Text

-- testParsingText
import Data.Machine (run, supply)
import Network.Telechat.Machines (parsingText)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testTelnetParser
  , testParsingText
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
      "snowman: ☃" @=? (Text.concat . run $ supply ["snowman: \xe2", "\x98\x83"] parsingText)
  ]
