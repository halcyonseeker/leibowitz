;;; Page generators

(in-package :leibowitz.web)

(defun html-snippet (snippet)
  (eval
   `(cl-who:with-html-output-to-string (*standard-output* nil :prologue NIL :indent t)
      ,@snippet)))

(defun make-page (lib &key here sidebar title body limit offset header)
  (eval
   `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
      (:html
       :xmlns     "http://www.w3.org/1999/xhtml"
       :xml\:lang "en"
       :lang      "en"
       (:head (:title ,title)
              (:meta :http-equiv "Content-Type"
                     :content "text/html;charset=utf-8")
              (:link :rel "stylesheet" :href "/static/style.css" :media "screen"))
       (:body (:div :id "header-and-navbar-container"
                    (:header :id "header" ,@(if header header `((:h1 ,title))))
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
              ,(when (and limit offset)
                 `(:nav :id "pagination" :class "ui"
                        ,@(loop with total = (library-data-quantity lib)
                                with pages = (ceiling (/ total limit))
                                for p from 1 to pages
                                for pth-link-offset = (* limit (- p 1))
                                collect `(:a :class ,(if (= offset pth-link-offset)
                                                        "page-link here"
                                                        "page-link")
                                             :href ,(format NIL "~A?limit=~A&offset=~A"
                                                            here limit pth-link-offset)
                                             ,(format NIL "~A" p)))))
              (:footer :id "footer"
                       (:hr)
                       (:a :href "https://sr.ht/~thalia/leibowitz"
                           "Project Home")
                       "//"
                       (:a :href "mailto:~thalia/leibowitz@lists.sr.ht"
                           "Report a Bug")))
              (:script :src "/static/fluff.js")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-datum-listing-sidebar (lib)
  `((:section
     (:h2 "Search")
     (:form :method "get" :action "/search" :class "sidebar-form"
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
     (:h2 "File Types")
     (:ul ,@(loop for type in (library-all-file-types lib)
                  collect `(:li (:a :href "/fixme/implement/search/filters"
                                    ,(car type))
                                (:small ,(format NIL "(~A)" (cdr type)))))))))

(defun list-data-as-html (lib &rest options &key &allow-other-keys)
  "Beautify the output of `leibowitz.core:list-data' as a HTML datum
listing.  Key arguments are passed unmodified to that method."
  (check-type lib library)
  `((:section :id "tiles"
              ,@(loop for datum in (apply #'list-data (nconc (list lib) options))
                      collect (datum-html-preview lib datum)))))

(defun list-search-results-as-html (lib terms limit offset)
  (check-type lib library)
  (check-type terms string)
  `(,(make-search-page-search-box lib terms)
    (:section :id "tiles"
              ,@(loop for datum in (query lib terms :limit limit :offset offset)
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

(defun make-tree-breadcrumbs (dir)
  `((:h1 "Tree | Leibowitz Web")
    (:nav
     (:ul :class "path-breadcrumbs"
          ,@(loop with path-so-far = "/"
                  for part in (pathname-directory dir)
                  when (stringp part)
                    do (setf path-so-far
                             (concatenate 'string path-so-far part "/"))
                  when (stringp part)
                    collect `(:li
                              (:a :href ,(format NIL "/tree?dir=~A"
                                                 (hunchentoot:url-encode
                                                  path-so-far))
                                  ,part)))))))

(defun make-tree-sidebar ()
  `((:section
     (:h2 "Quick Links")
     (:ul ,@(loop for d in (reverse (uiop:subdirectories (user-homedir-pathname)))
                  collect `(:li (:a :href ,(format NIL "/tree?dir=~A"
                                                   (hunchentoot:url-encode
                                                    (namestring d)))
                                    ,(cl-who:escape-string
                                      (enough-namestring
                                       d (user-homedir-pathname))))))))))

;; FIXME: make the listing prettier and more informative and figure
;; out a proper policy for opening files.
(defun list-contents-of-directory (dir)
  `((:section
     (:ul ,@(loop for p in (nconc (reverse (uiop:subdirectories dir))
                                  (reverse (uiop:directory-files dir)))
                  collect `(:li (:a :href ,(format NIL "/tree?dir=~A"
                                                   (hunchentoot:url-encode
                                                    (namestring p)))
                                    ,(cl-who:escape-string
                                      (enough-namestring p dir)))))))))

(defun make-datum-view-page (lib datum)
  (incf (datum-accesses datum))
  (add-datum lib datum)
  (nconc
   (datum-html-report lib datum)
   (list
    `(:section
      (:h2 "Edit Metadata")
      (:div :id "editor-widgets-container"
            (:div :id "editor-widget-left"
                  (:fieldset
                   (:legend "Edit Tags")
                   (:form :method "post"
                          (:textarea
                           :id "tag-editor-textarea"
                           :name "tags"
                           :placeholder "No tags yet, enter each on a new line"
                           ,(with-output-to-string (s)
                              (loop for tag in (get-datum-tags lib datum)
                                    do (format s "~A~%" (tag-name tag)))))
                          (:button :id "tag-editor-submit" "Save Tags"))))
            ;;; FIXME: wire these up!
            (:div :id "editor-widget-right"
                  (:fieldset
                   (:legend "Move or Rename")
                   (:form :method "put"
                          (:input :type "text" :name "new-name" :value ,(datum-id datum))
                          (:button "Move")))
                  (:fieldset
                   (:legend "Copy")
                   (:form :method "patch"
                          (:input :type "text" :name "new-name" :value ,(datum-id datum))
                          (:button "Copy")))
                  (:fieldset
                   (:legend "Delete")
                   (:form :method "delete"
                          (:button ,(format NIL "Permanently Delete ~A"
                                            (datum-title datum)))))))))))

(defun make-tag-view-sidebar (lib tag-name)
  (check-type lib library)
  (check-type tag-name string)
  `((:section
     (:h2 "About")
     ,(let ((tag (get-tag lib tag-name)))
        `(:ul (:li (:span :class "sidebar-metadata-key"
                          "Count")
                   (:span :class "sidebar-metadata-var"
                          ,(format NIL "~A" (tag-count tag))))
              (:li (:span :class "sidebar-metadata-key"
                          "Label")
                   (:span :class "sidebar-metadata-var"
                          ,(cl-who:escape-string (tag-label tag)))))))
    (:section
     (:h2 "Automatically Adds")
     (:ul ,@(loop for tag in (get-tag-predicates lib tag-name)
                  collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                   (hunchentoot:url-encode (tag-name tag)))
                                    ,(tag-name tag))
                                (:span :class "tag-count"
                                       ,(format nil "(~a)" (tag-count tag)))))))
    (:section
     (:h2 "Automatically Added By")
     (:ul ,@(loop for tag in (get-tag-predicands lib tag-name)
                  collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                   (hunchentoot:url-encode (tag-name tag)))
                                    ,(tag-name tag))
                                (:span :class "tag-count"
                                       ,(format nil "(~a)" (tag-count tag)))))))
    (:section
     (:h2 "Related tags")
     (:p "I'll need to figure out something clever here :p"))))

(defun make-tag-view-page (lib tag-name)
  (check-type lib library)
  (check-type tag-name string)
  (let ((tag (get-tag lib tag-name)))
    `((:section :id "tiles"
                ,@(loop for datum in (get-tag-data lib tag-name)
                        collect (datum-html-preview lib datum)))
      (:section
       (:h2 "Edit Tag")
       (:div :id "editor-widgets-container"
             (:div :id "editor-widget-left"
                   (:fieldset
                    (:legend "Also Apply These Tags")
                    (:form :method "post"
                           (:textarea
                            :id "tag-editor-textarea"
                            :name "tags"
                            :placeholder "None yet, enter each on a new line"
                            ,(with-output-to-string (s)
                               (loop for tag in (get-tag-predicates lib tag-name)
                                     do (format s "~A~%" (tag-name tag)))))
                           (:button :id "tag-editor-submit" "Save Parent Tags"))))
             ;;; FIXME: wire these up!
             (:div :id "editor-widget-right"
                   (:fieldset
                    (:legend "Edit Tag Description")
                    (:form :method "post"
                           (:textarea :id "description-editor-textarea"
                                      :name "description"
                                      :placeholder "Something about this tag"
                                      ,(tag-label tag))
                           (:button :id "description-editor-submit"
                                    "Save Description")))
                   (:fieldset
                    (:legend "Rename Tag")
                    (:form :method "put"
                           (:input :type "text" :name "new-name" :value ,(tag-name tag))
                           (:button "Rename")))
                   (:fieldset
                    (:legend "Delete Tag")
                    (:form :method "delete"
                           (:div :class "form-row"
                                 (:input :type "checkbox"
                                         :name "delete-children"
                                         :id "delete-children")
                                 (:label :for "delete-children"
                                         "Also delete child tags."))
                           (:div :class "form-row"
                                 (:input :type "checkbox"
                                         :name "delete-data"
                                         :id "delete-data")
                                 (:label :for "delete-data"
                                         "Also delete this tag's data.")))
                    (:button ,(format NIL "Permanently Delete ~A"
                                      (tag-name tag))))))))))
