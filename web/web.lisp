;;; A booru-style web UI

(in-package :leibowitz.web)

(defclass webserver (easy-routes:easy-routes-acceptor)
  ((library
    :initarg :library
    :initform (error "You can't run a webserver without a library, dummy!")
    :accessor webserver-library
    :documentation "Back-link to the library instance this webserver has access to."))
  (:documentation
   "Expand the easy-routes implementation of hunchentoot's acceptor with
a back-link to a library and some custom methods that in theory allow
us to run a bunch of servers on different ports from the same lisp.

Each http route in Leibowitz's web ui needs to have access to the
underlying `library', however, because of how hunchentoot manages its
route dispatch table, there's no way to pass it to the route functions
directly.  Therefore; in order to avoid introducing egregious global
state, we subclass `hunchentoot:acceptor' in a way that lets each
route handler access the corresponding library"))

(defmethod webserver-run ((w webserver))
  (setf hunchentoot:*catch-errors-p* NIL)
  (setf hunchentoot:*show-lisp-errors-p* T)
  (setf hunchentoot:*show-lisp-backtraces-p* T)
  (hunchentoot:start w))

(defmethod webserver-die ((w webserver))
  (hunchentoot:stop w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN QUESTIONABLE HACKS

;; FIXME: In theory I think these hacks allow us to run multiple
;; webservers on multiple libraries on multiple ports from the same
;; lisp, make sure this is actually the case!

(defmethod easy-routes::process-route
    ((acceptor webserver) (route easy-routes:route) bindings)
  "A specialization of `easy-routes::process-route' on our custom
`webserver' acceptor.  This method is responsible for calling the
endpoint handler method, which needs a library."
  (easy-routes::call-with-decorators
   (easy-routes::route-decorators route)
   (lambda ()
     (apply (easy-routes::route-symbol route)
            (nconc (list (webserver-library acceptor))
                   (loop for item in (slot-value route 'easy-routes::variables)
                         collect (cdr (assoc item bindings
                                             :test #'string=))))))))

(defmacro leibowitz-route ((name library template-and-options) params &body body)
  "Cribbed almost verbatim from `easy-routes:defroute' except that it
creates handlers as methods of `library' rather than as normal
functions."
  (let* ((template (if (listp template-and-options)
                       (first template-and-options)
                       template-and-options))
         (variables (routes:template-variables
                     (routes:parse-template template)))
         (arglist (mapcar (alexandria:compose #'intern #'symbol-name)
                          variables))
         (method (or (and (listp template-and-options)
                          (getf (rest template-and-options) :method))
                     :get))
         (acceptor-name (and (listp template-and-options)
                             (getf (rest template-and-options) :acceptor-name)))
         (decorators (and (listp template-and-options)
                          (getf (rest template-and-options) :decorators))))
    (multiple-value-bind (body declarations docstring)
	(alexandria:parse-body body :documentation t)
      (easy-routes::assoc-bind ((params nil)
                                (get-params :&get)
                                (post-params :&post)
                                (path-params :&path))
          (easy-routes::lambda-list-split '(:&get :&post :&path) params)
	`(let ((%route (make-instance 'easy-routes:route
                                      :symbol ',name
                                      :template ',(routes:parse-template template)
                                      :variables ',variables
                                      :required-method ',method
                                      :decorators ',decorators)))
           ,(if acceptor-name
		`(let ((easy-routes::%routes-and-mapper
                         (ensure-acceptor-routes-and-mapper ',acceptor-name)))
                   (setf (gethash ',name (getf easy-routes::%routes-and-mapper :routes))
                         %route))
		`(setf (gethash ',name easy-routes::*routes*) %route))
           (easy-routes::connect-routes ',acceptor-name)
           (defmethod ,name ((,library library) ,@arglist)
             ,@(when docstring
		 (list docstring))
	     (let (,@(loop for param in params
                           collect
                           (hunchentoot::make-defun-parameter param ''string :both))
                   ,@(loop for param in get-params
                           collect
                           (hunchentoot::make-defun-parameter param ''string :get))
                   ,@(loop for param in post-params
                           collect
                           (hunchentoot::make-defun-parameter param ''string :post))
                   ,@(loop for param in path-params
                           collect
                           (destructuring-bind (parameter-name parameter-type) param
                             `(,parameter-name (hunchentoot::convert-parameter
                                                ,parameter-name ,parameter-type)))))
	       ,@declarations
               ,@body)))))))

;;; END QUESTIONABLE HACKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
