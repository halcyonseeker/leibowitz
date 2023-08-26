;;; HTTP endpoint handlers

(in-package :leibowitz.web)

;; FIXME: adapt the hackjob to let us push static file handlers onto
;; the dispatch table per hunchentoot's documentation.
;; FIXME: For prod we'll obviously need a proper way of finding static
;; resources.
(leibowitz-route (stylesheet lib "/style.css") ()
  (hunchentoot:handle-static-file
   (merge-pathnames #P"code/leibowitz/web/style.css" (user-homedir-pathname))))

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
        ;; FIXME: make a template for expected errors
        (progn
          (setf (hunchentoot:return-code*) 404)
          (format NIL "Directory ~S does not exist" dir)))))

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
;; Datum view machinery

(leibowitz-route (datum-view lib "/datum") (id)
  ;; FIXME: get-datum should be changed to throw a condition when the
  ;; datum isn't found, returning NIL is in-band signaling.
  (let ((d (get-datum lib id)))
    (if d
        (make-page lib
                   :title (format NIL "~A | Leibowitz Web" (datum-title d))
                   :header `((:h1 ,(datum-title d))
                             (:small ,id))
                   :sidebar (datum-html-sidebar lib d)
                   :body (make-datum-view-page lib d))
        ;; FIXME: maybe make a template page for expected errors
        (progn
          (setf (hunchentoot:return-code*) 404)
          (format NIL "Datum with ID ~S not found" id)))))

(leibowitz-route (datum-raw lib "/raw") (id)
  (let ((d (get-datum lib id)))
    (setf (hunchentoot:content-type*) (datum-kind d))
    (injest-raw-datum d)))

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
  (make-page lib
             :title "Tag View | Leibowitz Web"
             :sidebar (make-tag-view-sidebar lib name)
             :body (make-tag-view-page lib name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing data

(leibowitz-route (edit-datum lib ("/datum" :method :post)) (id)
  (let ((data (hunchentoot:post-parameter "tags")))
    (let ((tags (with-input-from-string (s data)
                  (loop for line = (read-line s nil 'eof)
                        until (eq line 'eof)
                        collect  (remove-if (lambda (elem)
                                              (eql elem #\Return))
                                            line)))))
      (add-datum-tags lib id tags :replace T)
      (hunchentoot:redirect
       (format NIL "/datum?id=~A" (hunchentoot:url-encode id))))))
