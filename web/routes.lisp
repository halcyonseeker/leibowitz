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

(leibowitz-route (index-page lib "/") (limit offset)
  (let ((limit (if limit (parse-integer limit) 50))
        (offset (if offset (parse-integer offset) 0)))
    (make-page lib
               :here "/"
               :title "Recent | Leibowitz Web"
               :sidebar (make-datum-listing-sidebar lib)
               :body (list-data-as-html lib :sort-by :modified
                                            :direction :descending
                                            :limit limit
                                            :offset offset)
               :limit limit :offset offset)))

(leibowitz-route (popular-page lib "/popular") (limit offset)
  (let ((limit (if limit (parse-integer limit) 50))
        (offset (if offset (parse-integer offset) 0)))
    (make-page lib
               :here "/popular"
               :title "Popular | Leibowitz Web"
               :sidebar (make-datum-listing-sidebar lib)
               :body (list-data-as-html lib :sort-by :accesses
                                            :direction :descending
                                            :limit limit
                                            :offset offset)
               :limit limit :offset offset)))

(leibowitz-route (timeline-page lib "/timeline") (limit offset)
  (let ((limit (if limit (parse-integer limit) 50))
        (offset (if offset (parse-integer offset) 0)))
    (make-page lib
               :here "/timeline"
               :title "Timeline | Leibowitz Web"
               :sidebar (make-datum-listing-sidebar lib)
               :body (list-data-as-html lib :sort-by :birth
                                            :direction :ascending
                                            :limit limit
                                            :offset offset)
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
             (uiop:absolute-pathname-p (pathname dir)))
        (make-page lib
                   :here "/tree"
                   :title "Tree | Leibowitz Web"
                   :header (make-tree-breadcrumbs dir)
                   :sidebar (make-tree-sidebar)
                   :body (list-contents-of-directory dir))
        (return-404 lib (format NIL "Directory ~S does not exist" dir)))))

(leibowitz-route (search-page lib "/search") (q limit offset)
    (let ((limit (if limit (parse-integer limit) 50))
          (offset (if offset (parse-integer offset) 0)))
      (make-page lib
                 :here "/search"
                 :title "Search | Leibowitz Web"
                 :sidebar `((:section "Idk yet"))
                 :body (if q
                           (list-search-results-as-html lib q limit offset)
                           `(,(make-search-page-search-box lib)))
                 :limit limit :offset offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uploading data

;; FIXME wtf am I even doing with the datum-link?  Should I just
;; stop supporting it altogether?

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
             (dest (merge-pathnames name)))
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
  ;; FIXME: get-datum should be changed to throw a condition when the
  ;; datum isn't found, returning NIL is in-band signaling.
  (if id
      (let ((d (get-datum lib id)))
        (if d
            (make-page lib
                       :title (format NIL "~A | Leibowitz Web" (datum-title d))
                       :header `((:h1 ,(datum-title d))
                                 (:small ,id))
                       :sidebar (datum-html-sidebar lib d)
                       :body (make-datum-view-page lib d))
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
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (handler-case
          (loop for byte = (read-byte s)
                for index from 0 to (file-length s)
                do (setf (aref buf index) byte))
        (end-of-file () buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tag view machinery

(leibowitz-route (tag-view lib "/tag") (name)
  (if name
      (let ((tag (get-tag lib name)))
        (if tag
            (make-page lib
                       :title (format NIL "~A | Leibowitz Web" name)
                       :sidebar (make-tag-view-sidebar lib tag)
                       :body (make-tag-view-page lib tag))
            (return-404 lib (format NIL "Tag named ~S not found" name))))
      (hunchentoot:redirect "/tags")))

(leibowitz-route (edit-tag lib ("/tag" :method :post)) (name)
  (let ((predicates (%parse-post-body-to-list
                     (hunchentoot:post-parameter "tags")))
        (ajax (hunchentoot:post-parameter "ajax")))
    (add-tag-predicate lib name predicates :replace T)
    (if ajax
        ;; FIXME: handle NIL case or a no-such-tag condition, just
        ;; like tag-view!
        (html-snippet (make-tag-view-sidebar lib name))
        (hunchentoot:redirect
         (format NIL "/tag?name=~A" (hunchentoot:url-encode name))))))

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
