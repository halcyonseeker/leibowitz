;;; HTTP endpoint handlers

(in-package :leibowitz-web)

;; FIXME: adapt the hackjob to let us push static file handlers onto
;; the dispatch table per hunchentoot's documentation.
;; FIXME: For prod we'll obviously need a proper way of finding static
;; resources.
(leibowitz-route (stylesheet lib :uri "/style.css") ()
  (hunchentoot:handle-static-file
   (merge-pathnames #P"code/leibowitz/web/style.css" (user-homedir-pathname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level pages

(leibowitz-route (index-page lib :uri "/") (limit offset)
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

(leibowitz-route (popular-page lib :uri "/popular") (limit offset)
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

(leibowitz-route (timeline-page lib :uri "/timeline") (limit offset)
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

(leibowitz-route (tags-page lib :uri "/tags") ()
  (make-page lib
             :here "/tags"
             :title "Tags | Leibowitz Web"
             :sidebar `((:section "Idk yet"))
             :body (list-tags-as-html lib)))


(leibowitz-route (tree-page lib :uri "/tree") ()
  (make-page lib
             :here "/tree"
             :title "Tree | Leibowitz Web"
             :sidebar `((:section "File system highlights or smth"))
             :body `((:section (:b "FIXME ") "Write a file browser"))))

(leibowitz-route (search-page lib :uri "/search") (q limit offset)
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

(leibowitz-route (datum-view lib :uri "/datum") (id)
  (let ((id (hunchentoot:url-decode id)))
    (make-page lib
               :title "Datum View | Leibowitz Web"
               :sidebar (make-datum-view-sidebar lib id)
               :body (make-datum-view-page lib id))))

(leibowitz-route (datum-raw lib :uri "/raw") (id)
  (let ((d (get-datum lib (hunchentoot:url-decode id))))
    (setf (hunchentoot:content-type*) (datum-kind d))
    (injest-raw-datum d)))

;; FIXME: Temporary workaround to load a thumbnail from the library's
;; cache directory.  We really need to store static files in a set
;; place and use hunchentoot's static file handler instead!
(leibowitz-route (datum-thumbnail lib :uri "/thumbnail") (path)
  (format T "---- We're being asked to load ~S~%" (hunchentoot:url-decode path))
  (with-open-file (s (hunchentoot:url-decode path) :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (handler-case
          (loop for byte = (read-byte s)
                for index from 0 to (file-length s)
                do (setf (aref buf index) byte))
        (end-of-file () buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tag view machinery

(leibowitz-route (tag-view lib :uri "/tag") (name)
  (let ((name (hunchentoot:url-decode name)))
    (make-page lib
               :title "Tag View | Leibowitz Web"
               :sidebar (make-tag-view-sidebar lib name)
               :body (make-tag-view-page lib name))))
