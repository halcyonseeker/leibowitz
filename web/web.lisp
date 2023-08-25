;;; A booru-style web UI

(in-package :leibowitz.web)

(defclass webserver (hunchentoot:acceptor)
  ((library
    :initarg :library
    :initform (error "You can't run a webserver without a library, dummy!")
    :accessor webserver-library
    :documentation "Back-link to the library instance this webserver has access to."))
  (:documentation "Expand the hunchentoot acceptor implementation with
a back-link to a library and some custom methods that in theory allow
us to run a bunch of servers on different ports.

Each http route in Leibowitz's web ui needs to have access to the
underlying `library', however, because of how hunchentoot manages its
route dispatch table, there's no way to pass it to the route functions
directly.  Therefore; in order to avoid introducing egregious global
state, we subclass `hunchentoot:acceptor' in a way that lets each
route handler access the corresponding library"))

(defmethod webserver-run ((w webserver))
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*catch-errors-p* NIL)
  (hunchentoot:start w))

(defmethod webserver-die ((w webserver))
  (hunchentoot:stop w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN QUESTIONABLE HACKS

;; FIXME: trictly speaking, these work for a single webserver
;; instance.  But what about multiple simultaneous webservers on
;; different libraries; do these global variables break that?  And
;; what about the JSON API that we might want separate from the web
;; UI?

(defparameter *handler-alist* nil)
(defparameter *dispatch-table* (list 'dispatch-handlers))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor webserver) request)
  "An implementation of `hunchentoot:acceptor-dispatch-request' on the
custom `webserver' acceptor.  When hunchentoot receives a HTTP
request, it calls this method which calls the appropriate handler
function with the library instance as an argument."
  (loop for dispatcher in *dispatch-table*
        for action = (funcall dispatcher request)
        when action
          return (funcall action (webserver-library acceptor))
        finally (call-next-method)))

(defun dispatch-handlers (request)
  "This is used to initialize `*dispatch-table*' and returns the
appropriate handler created with `leibowitz-route'.  It's copied
almost verbatim from `hunchentoot:dispatch-easy-handlers'."
  (loop for (uri acceptor-names handler host) in *handler-alist*
        when (and (or (eq acceptor-names t)
                      (find (hunchentoot:acceptor-name hunchentoot:*acceptor*)
                            acceptor-names :test #'eq))
                  (cond ((stringp uri)
                         (and (or (null host)
                                  (string= (or (hunchentoot:host request) "unknown")
                                           host))
                              ;; Support RE for matching host names as well (wildcards)?
                              (string= (hunchentoot:script-name request) uri)))
                        (t (funcall uri request))))
          do (return handler)))

(defmacro leibowitz-route (description lambda-list &body body)
  "Cribbed almost verbatim from `hunchentoot:define-easy-handler' except
that it creates handlers as methods of `library' rather than as normal
functions, and that routes are stored in globals local to this
package."
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (name library &key uri host (acceptor-names t)
                                      (default-parameter-type ''string)
                                      (default-request-type :both))
      description
      (let ((name (read-from-string (format NIL "webserver-route-~A" name))))
        `(progn
           ,@(when uri
               (list
                (alexandria:once-only (uri host acceptor-names)
                                      `(progn
                                         (setq *handler-alist*
                                               (delete-if (lambda (list)
                                                            (and (or (and (equal ,uri (first list))
                                                                          ,(if host
                                                                               `(string= ,host (fourth list))))
                                                                     (eq ',name (third list)))
                                                                 (or (eq ,acceptor-names t)
                                                                     (eq (second list) t)
                                                                     (intersection ,acceptor-names
                                                                                   (second list)))))
                                                          *handler-alist*))
                                         (push (list ,uri ,acceptor-names ',name ,host)
                                               *handler-alist*)))))
           (defmethod ,name
             ((,library library) &key ,@(loop for part in lambda-list
                                              collect (hunchentoot::make-defun-parameter
                                                       part
                                                       default-parameter-type
                                                       default-request-type)))
             ,@body)))))

;;; END QUESTIONABLE HACKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

