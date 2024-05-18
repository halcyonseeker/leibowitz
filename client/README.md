leibowitz-client
================

A controller application for Leibowitz as a daemon.  Still a
prototype, I plan for this to be part of the 0.2 release.

Why?
----
1. Leibowitz as compiled with SBCL v2.4.2 without core compression is
   a 64mb binary that takes a noticeable fraction of a second to run
   on my 12 year old x230 Thinkpad (and would likely be much worse on
   slow hardware, especially on systems with slow storage).  When
   using it from the shell this lag can be exceptionally annoying, so
   as a workaround on slower machines I'd like it to be possible to
   run the main application in the background and control it using a
   fast-running program implementing the same CLI interface.
2. When using Leibowitz on a web server, the CLI provides a way to
   manage the running program without using the web interface that is
   accessible to those not comfortable in a Common Lisp REPL.
3. In order to keep things up to date, I'd like to add a `leibowitz
   daemon` subcommand that listens for file system events (inotify(2)
   on Linux) and updates the database automatically.  In this case it
   would make sense for users to use the client to interact with the
   running instance rather than evoking two copies of the leibowitz
   process on a single library as that might cause race conditions.
