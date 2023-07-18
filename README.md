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

At present Leibowitz is only usable as a Common Lisp library from a
REPL, however I intend on building several user interface options,
including a subcommand-based CLI, a booru or wiki-style web UI, a JSON
HTTP API, a 9p API, and maybe a desktop application.

Bugs, tasks, and fixmes
-----------------------
* Calling `leibowitz find xyz` from the command line sometimes returns
  an error `Code CORRUPT: database disk image is malformed.` with the
  offending stanza being `select data.* from search left join data on
  data.id = search.id where search match ? order by rank`.  Connecting
  to the database and running `pragma integrity_check` yields okay.
  Several times when this happened there were multiple leibowitz
  processes running on that same database, so maybe that is partially
  the culprit?  Some light stackoverflowing indicated this might be a
  result damaged indexes, which would make sense considering it only
  (so far) shows up when doing full-text search.
* The cli needs a way to normalize paths before passing them to the
  library; CL is absolutely clueless when it comes to resolving unix
  path notation.

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
