;;; HTTP endpoint handlers

(in-package :leibowitz.web)

;; FIXME: adapt the hackjob to let us push static file handlers onto
;; the dispatch table per hunchentoot's documentation.
;; FIXME: For prod we'll obviously need a proper way of finding static
;; resources.
(leibowitz-route (stylesheet lib "/static/style.css") ()
  (hunchentoot:handle-static-file
   (merge-pathnames #P"code/leibowitz/web/static/style.css" (user-homedir-pathname))))
(leibowitz-route (script lib "/static/fluff.js") ()
  (hunchentoot:handle-static-file
   (merge-pathnames #P"code/leibowitz/web/static/fluff.js" (user-homedir-pathname))))
(leibowitz-route (script-display-pdf lib "/static/display_pdf.js") ()
  (hunchentoot:handle-static-file
   (merge-pathnames #P"code/leibowitz/web/static/display_pdf.js" (user-homedir-pathname))))

(defun %parse-post-body-to-list (data)
  (when data
    (with-input-from-string (s data)
      (loop for line = (read-line s nil 'eof)
            until (eq line 'eof)
            for tag = (string-trim '(#\Space #\Return) line)
            unless (= 0 (length tag))
              collect tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level pages

(leibowitz-route (index-page lib "/") (limit offset sort-by direction view)
  ;; FIXME: `list-data' is the single-source-of-truth when it comes to
  ;; validating its parameters; it would
  (let ((limit (if limit (parse-integer limit) 50))
        (offset (if offset (parse-integer offset) 0))
        (sort-by (if sort-by (intern (string-upcase sort-by) :keyword)
                     :modified))
        (direction (if direction (intern (string-upcase direction) :keyword)
                       :descending))
        (view (if view (intern (string-upcase view) :keyword)
                  :tile)))
    (make-page lib
               :here "/"
               :title "All | Leibowitz Web"
               :sidebar (make-datum-listing-sidebar lib)
               :body (list-data-as-html lib view
                                        :sort-by sort-by
                                        :direction direction
                                        :limit limit
                                        :offset offset)
               :total (library-data-quantity lib)
               :limit limit :offset offset)))

(leibowitz-route (tags-page lib "/tags") ()
  (make-page lib
             :here "/tags"
             :title "Tags | Leibowitz Web"
             :sidebar `((:section "Idk yet"))
             :body (list-tags-as-html lib)))

(leibowitz-route (tree-page lib "/tree") (dir)
  (let ((dir (if dir dir (user-homedir-pathname))))
    (if (and (uiop:directory-exists-p dir)
             (uiop:absolute-pathname-p (uiop:parse-unix-namestring dir)))
        (make-page lib
                   :here "/tree"
                   :title (format NIL "~A | Leibowitz Web" dir)
                   :header (make-tree-breadcrumbs "Tree | Leibowitz Web" dir)
                   :sidebar (make-tree-sidebar dir)
                   :body (list-contents-of-directory lib dir))
        (return-404 lib (format NIL "Directory ~S does not exist" dir)))))

;; FIXME: parameter validations hould be in the core, just parse here
(leibowitz-route (search-page lib "/search") (q limit offset sort-by direction view)
    (let ((limit (if limit (parse-integer limit) 50))
          (offset (if offset (parse-integer offset) 0))
          (sort-by (if sort-by (intern (string-upcase sort-by) :keyword)
                       :modified))
          (direction (if direction (intern (string-upcase direction) :keyword)
                         :descending))
          (view (if view (intern (string-upcase view) :keyword)
                    :tile)))
      (make-page lib
                 :here "/search"
                 :title "Search | Leibowitz Web"
                 :sidebar `((:section "Idk yet"))
                 :body (if q
                           (list-search-results-as-html
                            lib q limit offset sort-by direction view)
                           `(,(make-search-page-search-box lib)))
                 ;; FIXME; :total here is the number of results,
                 ;; figure out a way to get that or perhaps replace
                 ;; numeric pagination with a more button.  As a
                 ;; marginally acceptable but inaccurate placeholder
                 ;; we'll go with the number of data
                 :total (library-data-quantity lib)
                 :limit limit :offset offset)))

(leibowitz-route (type-view-page lib "/type/:major/:minor") ()
  (let ((type (if minor (format NIL "~A/~A" major minor) major)))
    (make-page lib
               :title (format NIL "List of ~A files | Leibowitz Web" type)
               :sidebar `((:section "Idk yet"))
               :body `((:p "FIXME: All data listings should be sortable, filterable, and paginated!")
                       (:section :id "tiles"
                                 ,@(loop for datum in (library-list-files-by-type
                                                       lib type)
                                         collect (datum-html-preview lib datum)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uploading data

(leibowitz-route (new-page lib "/new") ()
  (make-page lib
             :here "new"
             :title "Upload a New Datum | Leibowitz Web"
             :body `((:section :class "upload-form"
                               (:form :method "post" :action "/new"
                                      :enctype "multipart/form-data"
                                      (:fieldset
                                       (:legend "Upload A File")
                                       (:input :type "file" :name "file")
                                       (:button "Upload")))))))

(leibowitz-route (new-datum lib ("/new" :method :post)) ()
  (let ((file (hunchentoot:post-parameter "file")))
    (if file
      (let* ((temp (nth 0 file))
             (name (nth 1 file))
             ;; FIXME: Upload directory needs to be something
             ;; user-configurable that makes sense in the context of
             ;; collections.  As-is this breaks when the current working
             ;; directory is not a subdirectory of library's :homedir
             ;; initarg (passed to collection-homedir).  While we're on
             ;; that topic, the collection API feels simultaneously
             ;; half-baked (what's even doing??) and overengineered
             ;; (whyyyy God are there so many things to keep track of)
             (dest (merge-pathnames (uiop:parse-unix-namestring name))))
        (if (probe-file dest)
            ;; FIXME: create a conflict-resolution page that should also
            ;; be used for copying and moving files.
            (format NIL "You uploaded ~S, but ~S already exists!" name dest)
            (progn
              (uiop:copy-file temp dest)
              (let ((datum (car (index lib dest))))
                (hunchentoot:redirect (format NIL "/datum?id=~A"
                                              (datum-id datum)))))))
      (hunchentoot:redirect "/new"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datum view machinery

(leibowitz-route (datum-view lib "/datum") (id)
  (if id
      (handler-case
          (let ((d (get-datum lib id :error T)))
            (make-page lib
                       :title (format NIL "~A | Leibowitz Web" (datum-title d))
                       :header (make-tree-breadcrumbs (datum-title d)
                                                      (uiop:parse-unix-namestring id))
                       :sidebar (datum-html-sidebar lib d)
                       :body (make-datum-view-page lib d)))
        (datum-not-indexed ()
          (return-404 lib (format NIL "Datum with ID ~S not found" id))))
      (hunchentoot:redirect "/")))

(leibowitz-route (datum-raw lib "/raw") (id)
  (if id
      (let ((d (get-datum lib id)))
        (if d
            (progn
              (setf (hunchentoot:content-type*) (datum-kind d))
              (injest-raw-datum d))
            (return-404 lib (format NIL "Datum with ID ~S not found" id))))
      (hunchentoot:redirect "/")))

;; FIXME: Temporary workaround to load a thumbnail from the library's
;; cache directory.  We really need to store static files in a set
;; place and use hunchentoot's static file handler instead!
(leibowitz-route (datum-thumbnail lib "/thumbnail") (path)
  (setf (hunchentoot:content-type*) "image/jpeg")
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (handler-case
          (loop for byte = (read-byte s)
                for index from 0 to (file-length s)
                do (setf (aref buf index) byte))
        (end-of-file () buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tag view machinery

(leibowitz-route (tag-view lib "/tag") (name limit offset sort-by direction view)
  (if name
      (let ((tag (get-tag lib name))
            (limit (if limit (parse-integer limit) 50))
            (offset (if offset (parse-integer offset) 0))
            ;; The library uses keywords internally, but here we'll
            ;; keep these as strings for creating pagination urls
            (sort-by (if sort-by sort-by "modified"))
            (direction (if direction direction "descending"))
            (view (if view view "tile")))
        (if tag
            (make-page lib
                       :title (format NIL "~A | Leibowitz Web" name)
                       :sidebar (make-tag-view-sidebar lib tag)
                       :body (make-tag-view-page
                              lib tag (intern (string-upcase view) :keyword)
                              :sort-by (intern (string-upcase sort-by) :keyword)
                              :direction (intern (string-upcase direction) :keyword)
                              :limit limit
                              :offset offset)
                       :here "/tag"
                       :total (tag-count tag)
                       :limit limit
                       :offset offset
                       :more-params (format NIL "name=~A&sort-by=~A&direction=~A&view=~A"
                                            (url name) sort-by direction view))
            (return-404 lib (format NIL "Tag named ~S not found" name))))
      (hunchentoot:redirect "/tags")))

;; FIXME: for the redirects here, we really should get the full URL so
;; that the user doesn't lose their place
(leibowitz-route (edit-tag lib ("/tag" :method :post)) (name)
  (let ((predicates (%parse-post-body-to-list (hunchentoot:post-parameter "tags")))
        ;; FIXME: Add support for recursive deletion to the core!
        ;; (delete-children (hunchentoot:post-parameter "delete-children"))
        ;; (delete-data (hunchentoot:post-parameter "delete-data"))
        (move-to (hunchentoot:post-parameter "move-to"))
        (copy-to (hunchentoot:post-parameter "copy-to"))
        (delete  (hunchentoot:post-parameter "delete"))
        (desc    (hunchentoot:post-parameter "description"))
        (ajax    (hunchentoot:post-parameter "ajax")))
    (handler-case
        (cond (move-to
               (move-tag lib name move-to :merge T)
               (hunchentoot:redirect (format NIL "/tag?name=~A" (url move-to))))
              (copy-to
               (copy-tag lib name copy-to)
               (hunchentoot:redirect (format NIL "/tag?name=~A" (url copy-to))))
              (delete
               (del-tag lib name)
               (hunchentoot:redirect "/tags"))
              (desc
               (let ((tag (get-tag lib name)))
                 (setf (tag-label tag) desc)
                 (add-tag lib tag))
               (hunchentoot:redirect (format NIL "/tag?name=~A" (url name))))
              (predicates
               (add-tag-predicate lib name predicates :replace T)
               (if ajax
                   (html-snippet (make-tag-view-sidebar lib (get-tag lib name)))
                   (hunchentoot:redirect (format NIL "/tag?name=~A" (url name))))))
      (no-such-tag ()
        (return-404 lib (format NIL "Tag with NAME ~S not found" name))))))

;;; FIXME: Per the new forms, when :retroactive and :replace are both
;;; passed to `datum-add-predicates', we should also go through and
;;; REMOVE the old tags from all affected data!  Wire up the forms!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing data

(leibowitz-route (edit-datum lib ("/datum" :method :post)) (id)
  (let ((move-to (hunchentoot:post-parameter "move-to"))
        (copy-to (hunchentoot:post-parameter "copy-to"))
        (delete  (hunchentoot:post-parameter "delete"))
        (tags    (%parse-post-body-to-list (hunchentoot:post-parameter "tags")))
        (ajax    (hunchentoot:post-parameter "ajax")))
    ;; FIXME: some validation would be good, though this endpoint
    ;; really should only be used internally
    (handler-case
        ;; FIXME: Add conflict resolution page to handle overwrites.
        (cond (move-to
               (move-datum lib id move-to :overwrite NIL)
               (hunchentoot:redirect (format NIL "/datum?id=~A" (url move-to))))
              (copy-to
               (copy-datum lib id copy-to :overwrite NIL)
               (hunchentoot:redirect (format NIL "/datum?id=~A" (url copy-to))))
              (delete
               (del-datum lib id)
               (hunchentoot:redirect "/"))
              (tags
               (add-datum-tags lib id tags :replace T)
               (if ajax
                   (let ((datum (get-datum lib id :error T)))
                     (html-snippet (datum-html-sidebar lib datum)))
                   (hunchentoot:redirect (format NIL "/datum?id=~A" (url id))))))
      (datum-not-indexed ()
        (return-404 lib (format NIL "Datum with ID ~S not found" id))))))

(leibowitz-route (index-datum lib ("/index" :method :post)) (path)
  (if path
      (progn
        (index lib path)
        (hunchentoot:redirect (format NIL "/~A=~A"
                                      (if (uiop:directory-exists-p path)
                                          "tree?dir" "datum?id")
                                      (url path))))
      (return-400 lib "You didn't specify what you want me to index")))

