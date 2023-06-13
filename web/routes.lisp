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
             :body `((:section (:pre
                                ,(cl-who:escape-string
                                  (with-output-to-string (s)
                                    (describe lib s))))))))
