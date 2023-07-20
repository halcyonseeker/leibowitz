Leibowitz
=========

Leibowitz is an experimental Common Lisp library intended to abstract
traditional file systems to make them behave like object storage.
Features include:

* **Tagging**, so you can organize data into mutually inclusive
  ontologies.  You can also construct hierarchies of tags using the
  predicate system where data with a certain tag might be
  automatically be added to a "parent" tag.
* **Full text search** of all files.  There's room to add custom
  indexing methods to do things like OCR image files and import
  metadata for movies, books, and music and such.
* **Collections** of files that allow you to customize how Leibowitz
  sees certain files on disk grouped by directory or URL schema.  This
  allows Leibowitz to act as a manager for bookmarked webpages,
  torrents, and whatnot.
* **Modular backends** in which to store the tag ontology and text
  dump for search.  Right now only SQLite is supported.

At present Leibowitz is very much a work in progress and is varying
degrees of useful from a REPL, the command line, and a web UI. I
intend on also building several other user interface options,
including a JSON HTTP API, a 9p API, and a desktop application using
TK or maybe QT.

Installation
------------

Right now in order to build or use Leibowitz you need ffmpeg,
imagemagick, sqlite, make, quicklisp, and sbcl.  On my machine I can
build it as an executable with `make` or hack on it from a REPL with
`(load #P"leibowitz.asd"` and `(ql:quickload :leibowitz)`.  I'd like
Leibowitz to be easy to build, install, and use for people who don't
know Common Lisp, but it's still a fair long ways off from that goal.

Bugs, tasks, and fixmes
-----------------------
* Sometimes doing a full-text search yields an error `Code CORRUPT:
  database disk image is malformed.` with the offending stanza being
  `select data.* from search left join data on data.id = search.id
  where search match ? order by rank`.  Connecting to the database and
  running `pragma integrity_check` yields okay.  Some light
  stackoverflowing indicated this might be a result damaged indexes,
  which would make sense considering it only (so far) shows up when
  doing full-text search.
* The cli needs a way to normalize paths before passing them to the
  library; CL is absolutely clueless when it comes to resolving unix
  path notation.
* Add more error handling to the web UI!  Right now it is insanely
  easy to get this thing to crash.
* Make indexing and thumbnail generation operations run in parallel,
  they're way too slow right now.
* Improve search by allow the user to search in different fields
  instead of a homogeneous dump.  Also include tags in datum search
  terms.

Notes
-----
* For thumbnailing in the prototype, consider using an implementation
  of the freedesktop thumbnail dbus protocol (because that's a thing
  apparently???) like <https://docs.xfce.org/xfce/tumbler/start>.  I'd
  rather not depend on dbus and instead write a library with a simple
  API that wraps up ffmpeg and imagemagick, maybe with OS-specific
  logic for querying themes and pulling icons as a fallback.
* Good UI: <https://github.com/philomena-dev/philomena> for example in
  <https://derpibooru.org/>.  Leibowitz could integrate with a tagged
  forum like Tumblr or Dreamwidth/Livejournal where each datum is a
  post with replies/comments and tags...
