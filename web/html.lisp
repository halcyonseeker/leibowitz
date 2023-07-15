;;; Page generators

(in-package :leibowitz-web)

(defun make-page (&key here sidebar title body)
  (eval
   `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
      (:html
       :xmlns     "http://www.w3.org/1999/xhtml"
       :xml\:lang "en"
       :lang      "en"
       (:head (:title ,title)
              (:meta :http-equiv "Content-Type"
                     :content "text/html;charset=utf-8")
              (:link :rel "stylesheet" :href "/style.css" :media "screen"))
       (:body (:div :id "header-and-navbar-container"
                    (:header :id "header" (:h1 ,title))
                    (:nav :id "navbar" :class "ui"
                          ;; Apply the "here" class to whichever link
                          ;; corresponds to the current page, if any
                          ,@(loop for link in '((:a :href "/" "Recent")
                                                (:a :href "/popular" "Popular")
                                                (:a :href "/timeline" "Timeline")
                                                (:a :href "/tags" "Tags")
                                                (:a :href "/tree" "Tree")
                                                (:a :href "/search" "Search"))
                                  do (when (equal (nth 2 link) here)
                                       (setf link (append (subseq link 0 3)
                                                          '(:class "here")
                                                          (last link))))
                                  collect link)))
              (:div :id "sidebar-and-content-container"
                    (:aside :id "sidebar" :class "ui" ,@sidebar)
                    (:main :id "content" ,@body))
              (:footer :id "footer"
                       (:hr)
                       (:a :href "https://sr.ht/~thalia/leibowitz"
                           "Project Home")
                       "//"
                       (:a :href "mailto:~thalia/leibowitz@lists.sr.ht"
                           "Report a Bug")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-datum-listing-sidebar (lib)
  `((:section
     (:h2 "Search")
     (:form :method "get" :action "/search" :id "search-form"
            (:input :name "q")
            (:button "Go!")))
    (:section
     (:h2 "Top Tags")
     ;; FIXME: add an argument to list-tags to return only the top N.
     (:ul ,@(loop for tag in (list-tags lib)
                  collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                   (hunchentoot:url-encode
                                                    (tag-name tag)))
                                    ,(tag-name tag))
                                (:span :class "tag-count"
                                       ,(format nil "(~a)" (tag-count tag)))))))
    (:section
     (:h2 "File Types"))))

(defun list-data-as-html (lib &rest options &key &allow-other-keys)
  "Beautify the output of `leibowitz-core:list-data' as a HTML datum
listing.  Key arguments are passed unmodified to that method."
  (check-type lib library)
  `((:section :id "tiles"
              ,@(loop for datum in (apply #'list-data (nconc (list lib) options))
                      collect (datum-html-preview lib datum)))))

(defun list-search-results-as-html (lib terms)
  (check-type lib library)
  (check-type terms string)
  `(,(make-search-page-search-box lib terms)
    (:section :id "tiles"
              ,@(loop for datum in (query lib terms)
                      collect (datum-html-preview lib datum)))))

(defun make-search-page-search-box (lib &optional terms)
  (check-type lib library)
  `(:section (:form :method "get" :action "/search"
                    (:fieldset
                     (:legend "Advanced Search")
                     (:div :class "form-row"
                           (:input :name "q" :type "text" :value ,(when terms terms)))
                     (:div :class "form-row"
                           (:small "FIXME: add tag, collection, and kind filters here; improve style!"))
                     (:input :class "form-row" :type "submit" :value "Go!")))))

(defun list-tags-as-html (lib &rest options &key &allow-other-keys)
  (check-type lib library)
  `((:section
     (:ul ,@(loop for tag in (apply #'list-tags (nconc (list lib) options))
                  collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                   (hunchentoot:url-encode (tag-name tag)))
                                    ,(tag-name tag))
                                (:span :class "tag-count"
                                       ,(format nil "(~a)" (tag-count tag)))
                                (:span :class "tag-desc"
                                       ,(tag-label tag))))))))

(defun make-datum-view-sidebar (lib datum-id)
  (check-type lib library)
  (check-type datum-id string)
  (datum-html-sidebar lib (get-datum lib datum-id)))

(defun make-datum-view-page (lib datum-id)
  (check-type lib library)
  (check-type datum-id string)
  (datum-html-report lib (get-datum lib datum-id)))

(defun make-tag-view-sidebar (lib tag-name)
  (check-type lib library)
  (check-type tag-name string)
  `((:section (:p "Metadata will go here")
              (:h2 "Related tags")
              (:p "Not sure how we'll do this"))))

(defun make-tag-view-page (lib tag-name)
  (check-type lib library)
  (check-type tag-name string)
  (let ((tag (get-tag lib tag-name)))
    `((:section (:h2 ,tag-name)
                (:span :class "tag-count"
                       ,(format NIL "(~A)" (tag-count tag)))
                (:span :class "tag-desc"
                       ,(tag-label tag)))
      (:section :id "tiles"
                ,@(loop for datum in (get-tag-data lib tag-name)
                        collect (datum-html-preview lib datum))))))
