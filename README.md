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

Roadmap to 0.1 version; minimum viable product
----------------------------------------------

### Core

- [ ] Fix predicate/predicand bug where tags are being automatically
      applied where they shouldn't be.  This *might* be a usage error
      in web, which itself would indicate that my API is probably too
      unintuitive and in need of revision.
- [ ] Improve full text search to index different fields (path, title,
      body, tags, tag descriptions) separately so that the user may
      selectively search in them.
- [ ] Optimize the indexer method and make it run multiple workers in
      parallel, right now it's very slow.
- [ ] Sometimes doing a full-text search yields an error `Code
      CORRUPT: database disk image is malformed.` with the offending
      stanza being `select data.* from search left join data on
      data.id = search.id where search match ? order by rank`.
      Connecting to the database and running `pragma integrity_check`
      yields okay.  Some light stackoverflowing indicated this might
      be a result damaged indexes, which would make sense considering
      it only (so far) shows up when doing full-text search.

### Web

- [ ] Add more error handling to the web UI!  Right now it is insanely
      easy to get this thing to crash.
- [ ] Expose the full API functionality in the web frontend:
  - [ ] Editing data entries:
    - [X] Adding tags
    - [X] Removing tags
    - [ ] Moving/renaming
    - [ ] Uploading/importing from URL
    - [ ] Manually reindexing files and directories
    - [ ] Deleting
  - [ ] Editing tag entries:
    - [ ] Adding data
    - [ ] Removing data
    - [ ] Renaming tags
    - [ ] Editing tag description
    - [X] Adding parents
    - [X] Removing parents
    - [ ] Adding children
    - [ ] Removing children
  - [ ] Search and listing:
    - [ ] Support changing the sort order and criterion for all data
          listings
    - [ ] Support filtering by tag for all data listings
    - [ ] Advanced search options, requires change to core full-text
          search schema.

### CLI

- [ ] The cli needs a way to normalize paths before passing them to
      the library; CL is absolutely clueless when it comes to
      resolving unix path notation.
- [ ] Expose the full API functionality in the CLI interface:
  - [ ] Editing data entries:
    - [ ] Adding tags
    - [ ] Removing tags
    - [ ] Moving/renaming
    - [ ] Uploading/importing from URL
    - [ ] Manually reindexing files and directories
    - [ ] Deleting
    - [ ] Editing metadata
    - [ ] Viewing data summaries
  - [ ] Editing tag entries:
    - [ ] Adding data
    - [ ] Removing data
    - [ ] Renaming tags
    - [ ] Editing tag description
    - [ ] Adding parents
    - [ ] Removing parents
    - [ ] Adding children
    - [ ] Removing children
    - [ ] Viewing tag summaries
  - [ ] Search and listing:
    - [ ] Support changing the sort order and criterion for all data
          listings
    - [ ] Support filtering by tag for all data listings
    - [ ] Advanced search options, requires change to core full-text
          search schema.

Future Work
-----------
- Implement collections for gallery-dl, man, etc.
- Add support for file and collection specific metadata fields,
  probably implemented as a special kind of tags.
- Add a native GUI.
- Add full support for saving URLs, including saving magnets and
  archiving HTTP docs.  These files should have special options in all
  frontends.

Notes
-----
* Good UI: <https://github.com/philomena-dev/philomena> for example in
  <https://derpibooru.org/>.  Leibowitz could integrate with a tagged
  forum like Tumblr or Dreamwidth/Livejournal where each datum is a
  post with replies/comments and tags...
