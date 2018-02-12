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

Implementation

- [x] high level design
  - [x] Reader/Writer
  - [x] Client identity
- [ ] Child processes
  * [ ] Reader
    - [x] Discard telnet commands
    - [x] Parse UTF-8
    - [x] Interpret control codes (DEL, Enter, etc.)
    - [x] Filter remaining non-readable chars from chat messages
    - [ ] Forward messages to Writer
    - [ ] Write IO to wrap readingMachine
  * [ ] Writer
    - [ ] Update state of client terminal
    - [ ] Draw client terminal
    - [ ] Convert 'Text' messages to valid telnet data (simply encodeUtf8 - see
          Telnet/UTF-8 note below)
    - [ ] Write IO to wrap writingMachine

## Telnet/UTF note

0xFF is not a valid UTF-8 byte, so we'll never see it after encoding a value of
type 'Text'. This means we don't have to escape it in Telnet, so no extra work
needs to be done. See https://en.wikipedia.org/wiki/UTF-8

Bugs / miscellaneous TODOs

- [ ] Place limits on length of parsed telnet commands.
      Probably if we go over ~1000 bytes, we've encountered
      a telnet command that has broken the parser.
