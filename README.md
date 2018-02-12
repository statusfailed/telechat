# telechat

A telnet chat server using distributed-process

# design

* Each socket connection spawns a pair of child processes
  * the "reader" process
    - Interprets (and discards) telnet commands
    - parse remaining data to UTF-8
    - Interprets and control codes / removes unreadables from
      stream of client data.
  * the "writer" process 
    - Receives commands from child reader
    - Renders the child's terminal
    - Receives chat messages from other clients
    - Broadcasts chat messages to other writers

# TODO

Main issue: telnet parser outputs a list of bytestrings, so it will never
terminate. Example: send a bunch of input to the server, then close your
connection. Server will (belately) parse to 'Command', and output to terminal

Implementation

- [x] high level design
  - [x] Reader/Writer
  - [x] Client identity
- [ ] Child processes
  * [ ] Reader
    - [ ] Discard telnet commands.
    - [x] Parse UTF-8
    - [x] Interpret control codes (DEL, Enter, etc.)
    - [x] Filter remaining non-readable chars from chat messages
    - [x] Forward messages to Writer
    - [x] Write IO to wrap readingMachine
  * [x] Writer
    - [x] Update state of client terminal
    - [x] Broadcast 'Send' messages to other writers
    - [x] Draw client terminal
    - [x] Convert 'Text' messages to valid telnet data (simply encodeUtf8 - see
          Telnet/UTF-8 note below)
    - [x] Write IO to wrap writingMachine

## Telnet/UTF note

0xFF is not a valid UTF-8 byte, so we'll never see it after encoding a value of
type 'Text'. This means we don't have to escape it in Telnet, so no extra work
needs to be done. See https://en.wikipedia.org/wiki/UTF-8

Bugs / miscellaneous TODOs

- [ ] Place limits on length of parsed telnet commands.
      Probably if we go over ~1000 bytes, we've encountered
      a telnet command that has broken the parser.
