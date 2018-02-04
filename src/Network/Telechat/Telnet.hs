module Network.Telechat.Telnet where

-- This module contains raw telnet commands (as bytes) which are used to put
-- the client into "raw" mode, with no local echoing of characters.

-- We briefly pretend to be telnet-aware in order to tell the
-- client to use unbuffered input, which lets us build an interactive prompt
-- properly
-- see: https://stackoverflow.com/questions/4532344/
--
-- and https://tools.ietf.org/html/rfc1184
-- and https://tools.ietf.org/html/rfc854
-- and https://tools.ietf.org/html/rfc857

-- IAC DO LINEMODE
-- sender (us) requests the remote side 
-- "Begin subnegotiation of editing status" (P4, RFC1184)
iac_do_linemode = "\xFF\xFD\x22"

-- IAC SB LINEMODE MODE 0
-- the "0" is a bitmask. By turning off everything, we basically tell
-- the client to be a dumb byte-entering-machine.
iac_sb_linemode_mode_0 = "\xFF\xFA\x22\x01\x00"

-- IAC 240, page 14 https://tools.ietf.org/html/rfc854
-- "End of parameters"
iac_se = "\xFF\xF0"

-- IAC WILL ECHO
-- "we'll output characters for this terminal, don't you worry!"
-- more or less instructs the client to stop echoing locally.
-- P1, RFC857
iac_will_echo '\xFF\xFB\x01'

-- Final command to set raw mode.
telnet_SET_RAW_MODE
  =  iac_do_linemode
  <> iac_sb_linemode_mode_0 <> iac_se
  <> iac_will_echo
