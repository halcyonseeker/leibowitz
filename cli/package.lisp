
(defpackage :leibowitz.cli
  (:use #:cl
        #:leibowitz.util
        #:leibowitz.core
        #:leibowitz.web)
  (:local-nicknames (#:alx #:alexandria))
  (:export #:main)
  (:export #:*data-directory*
           #:*cache-directory*
           #:*base-directory*
           #:*library*
           #:*webserver*))
