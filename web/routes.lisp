;;; HTTP endpoint handlers

(in-package :leibowitz-web)

;; FIXME: adapt the hackjob to let us push static file handlers onto
;; the dispatch table per hunchentoot's documentation.
;; FIXME: For prod we'll obviously need a proper way of finding static
;; resources.
(leibowitz-route (stylesheet lib :uri "/style.css") ()
  (hunchentoot:handle-static-file
   (merge-pathnames #P"code/leibowitz/web/style.css" (user-homedir-pathname))))

(leibowitz-route (index-page lib :uri "/") ()
  (make-page :here "/"
             :title "Recent | Leibowitz Web"
             :sidebar (make-datum-listing-sidebar)
             :body (list-data-as-html lib :sort-by :modified
                                          :direction :descending)))

(leibowitz-route (popular-page lib :uri "/popular") ()
  (make-page :here "/popular"
             :title "Popular | Leibowitz Web"
             :sidebar (make-datum-listing-sidebar)
             :body `((:section (:b "FIXME ") "Implement access logging!"))))

(leibowitz-route (timeline-page lib :uri "/timeline") ()
  (make-page :here "/timeline"
             :title "Timeline | Leibowitz Web"
             :sidebar (make-datum-listing-sidebar)
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

(leibowitz-route (search-page lib :uri "/search") ()
  (make-page :here "/search"
             :title "Search | Leibowitz Web"
             :sidebar `((:section "Idk"))
             :body `((:section (:b "FIXME ") "Search results"))))
