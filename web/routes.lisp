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

(leibowitz-route (index-page lib :uri "/") ()
  (make-page :here "/"
             :title "Recent | Leibowitz Web"
             :sidebar (make-datum-listing-sidebar lib)
             :body (list-data-as-html lib :sort-by :modified
                                          :direction :descending)))

(leibowitz-route (popular-page lib :uri "/popular") ()
  (make-page :here "/popular"
             :title "Popular | Leibowitz Web"
             :sidebar (make-datum-listing-sidebar lib)
             :body (list-data-as-html lib :sort-by :accesses
                                          :direction :descending)))

(leibowitz-route (timeline-page lib :uri "/timeline") ()
  (make-page :here "/timeline"
             :title "Timeline | Leibowitz Web"
             :sidebar (make-datum-listing-sidebar lib)
             :body (list-data-as-html lib :sort-by :birth
                                          :direction :ascending)))

(leibowitz-route (tags-page lib :uri "/tags") ()
  (make-page :here "/tags"
             :title "Tags | Leibowitz Web"
             :sidebar `((:section "Idk yet"))
             :body (list-tags-as-html lib)))


(leibowitz-route (tree-page lib :uri "/tree") ()
  (make-page :here "/tree"
             :title "Tree | Leibowitz Web"
             :sidebar `((:section "File system highlights or smth"))
             :body `((:section (:b "FIXME ") "Write a file browser"))))

(leibowitz-route (search-page lib :uri "/search") (q)
  (make-page :here "/search"
             :title "Search | Leibowitz Web"
             :sidebar `((:section "Idk yet"))
             :body (if q
                       (list-search-results-as-html lib q)
                       `(,(make-search-page-search-box lib)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datum view machinery

(leibowitz-route (datum-view lib :uri "/datum") (id)
  (let ((id (hunchentoot:url-decode id)))
    (make-page :title "Datum View | Leibowitz Web"
               :sidebar (make-datum-view-sidebar lib id)
               :body (make-datum-view-page lib id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tag view machinery

(leibowitz-route (tag-view lib :uri "/tag") (name)
  (let ((name (hunchentoot:url-decode name)))
    (make-page :title "Tag View | Leibowitz Web"
               :sidebar (make-tag-view-sidebar lib name)
               :body (make-tag-view-page lib name))))
