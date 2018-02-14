# telechat

A telnet chat server using distributed-process.

Build and run:

    stack build
    stack exec telechat

Connect:

    telnet localhost 4444
    sf@mymachine>telnet localhost 4444
		Trying 127.0.0.1...
		Connected to localhost.
		Escape character is '^]'.
		hello!
		this is a message from someone else!
		say something > why haven't I added identity yet? laziness.

TODO:

* Command line options to select port
* Identity/Authentication
* Connection limits / spam filtering?
* Better line editing
	- Ctrl+w (rubout)
	- Ctrl+c (clear line)
	- No sending of blank lines

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
- [x] Child processes
  * [x] Reader
    - [x] Discard telnet commands.
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
    - [x] Test that messages displayed on local terminal as well as remote

Refactoring

- [ ] Test that "Chat" message is broadcast - refactor so Chat isn't added
      in Telechat.hs IO code

TODO: regression tests - already fixed, but make sure they don't break again.

- [ ] Reader/Writer dying means other process also dies (seems broken?)
- [ ] Backspace on empty buffer
- [ ] Unreadable characters don't crash client
- [ ] Property based tests!
  - [ ] Check all combinations of valid + invalid inputs
  - [ ] Service does not drop input
  - [ ] Service does not hang (i.e. stopping machines!)

## Telnet/UTF note

0xFF is not a valid UTF-8 byte, so we'll never see it after encoding a value of
type 'Text'. This means we don't have to escape it in Telnet, so no extra work
needs to be done. See https://en.wikipedia.org/wiki/UTF-8

Bugs / miscellaneous TODOs

- [ ] Place limits on length of parsed telnet commands.
      Probably if we go over ~1000 bytes, we've encountered
      a telnet command that has broken the parser.
