;;
;; https://hpc.guix.info/blog/2023/06/a-guide-to-reproducible-research-papers/
;;

;; guix shell -Cm manifest.scm -- make
;; guix shell -Cm manifest.scm -- make test

;; guix describe -f channels > channels.scm
;; guix time-machine -C channels.scm -- shell -Cm manifest.scm -- make
;; guix time-machine -C channels.scm -- shell -Cm manifest.scm -- make test

(specifications->manifest
 (list
  ;; basic
  "coreutils" "make" "sbcl" "cl-asdf"
  ;; shared
  "cl-alexandria" "cl-bordeaux-threads"
  ;; tests
  "cl-parachute"
  ;; core
  "cl-sqlite" "cl-local-time" "cl-osicat" "cl-fad"
  ;; web
  "cl-hunchentoot" "cl-who" ; packageme "cl-easy-routes"
  ;; cli
  "cl-slynk" ; packageme "cl-clingon"
  ;; thumbnailer
  "cl-zip" "cl-lquery"
  ))
