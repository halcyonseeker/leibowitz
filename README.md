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
