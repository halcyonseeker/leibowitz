A file thumbnailing library for Common Lisp
===========================================

A simple thumbnail generation library with support for caching on the
file system.  It works by shelling out Imagemagick and ffmpeg and
should in theory work for all files with `video/*` or `image/*` mime
types, as well as `audio/*` files with embedded cover art.  The only
audio format I've found so far that don't work are flacs with ffmpeg
6.1.1.  It also works for pdf, postscript, and a handful of Microsoft
office formats — these require that you have LibreOffice installed as
ImageMagick calls it under the hood to convert them to pdf.

In order to use it, install ffmpeg, ImageMagick (for best results make
sure the former has been compiled with support for patent-encumbered
codecs), and optionally LibreOffice, then clone this repo somewhere
quicklisp can find it, and finally `(ql:quickload :thumbnailer)`.

Once I've satisfied every criterion on the following roadmap I'll
seperate this project into its own repository and publish it on
quicklisp.

Usage
-----

A typically use-case to generate thumbnails with a specific directory
for your application's cache will probably look something like this:

```lisp

(handler-case
    (let ((thumbnailer:*thumbnail-cache-dir* #P"my/apps/thumbnail/cache"))
      (thumbnailer:get-thumbnail #P"path/to/some/file.ext" "mime/type"))
  (thumbnailer:unsupported-file-type ())
  (thumbnailer:thumbnail-creation-failed ()))
```

Catching `thumbnailer:unsupported-file-type` and
`thumbnailer:thumbnail-creation-failed` like this will silently skip
unsupported or mangled files.  If your ImageMagick or ffmpeg
executables are not in your `$PATH`, you can set the
`thumbnailer:*ffmpeg-exe*` or `thumbnailer:*imagemagick-exe*`
accordingly.

Roadmap
-------

Required before I submit to quicklisp:

- [X] Support images
- [X] Support video
- [ ] Make sure more exotic animated images like apng and animated
      webp are supported by imagemagick, if not detect them and use
      ffmpeg or something instead
- [X] Don't hardcode the default `thumbnailer:*thumbnail-cache-dir*`
      under `/tmp/` then verify Windows and OSX support.
- [X] Verify support for audio with embedded cover art
- [ ] Verify support for audio WITHOUT embedded cover art
- [X] Support PDF and Postscript
- [ ] Support plain text
- [ ] Support EPUB, MOBI, and other ebook formats
- [ ] Optionally fall back to looking for generic file icons using the
      OS/DE appropriate mechanisms, optionally respecting the user's
      theme.
- [ ] Have a configurable way of handling executable files; should
      they all get the same generic thumbnail or should it be by mime
      type?

Cool but optional features:

- [ ] Figure out a way to portably infer the mime type so the user
      doesn't need to pass it all the time.  Parsing the output of
      file(1) isn't portable and building the existing CL libmagic
      bindings on quicklisp invariably results in a mess of CFFI
      errors.  The *right* way to do this is to reimplement magic as a
      CL library, but that would be very difficult and time
      consuming.
- [ ] Optionally support directory thumbnails showing previews of the
      contents.
- [ ] Optionally support little thumbnail badges to show, eg, the logo
      of the programming language used in a text file.  License
      permitting, we could steal these from the [treemacs
      project](https://github.com/Alexander-Miller/treemacs/tree/master/icons/default).
- [X] Maybe support MS Office, LibreOffice, and other fancy formats.
- [ ] Maybe optionally support previews for weirder things like
      torrents, tarballs, and zip archives?
- [ ] Imagemagick doesn't seem to support rtf (text/rtf) or odp
      (application/vnd.oasis.opendocument.presentation), which seems
      odd to me since for docx, doc, and pptx support it shells out to
      a headless libreoffice for pdf export.  I imagine this would be
      easy to fix upstream.

Derivative work:

- [ ] Use this library to implement a provider for the FreeDesktop
      thumbnail DBUS protocol?
