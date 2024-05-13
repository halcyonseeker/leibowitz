;;; Page generators

(in-package :leibowitz.web)

(defun html-snippet (snippet)
  (eval
   `(cl-who:with-html-output-to-string (*standard-output* nil :prologue NIL :indent t)
      ,@snippet)))

(defun make-page (lib &key here sidebar title body limit offset total header more-params)
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
                          ,@(loop for link in '((:a :href "/" "All")
                                                (:a :href "/tags" "Tags")
                                                (:a :href "/tree" "Tree")
                                                (:a :href "/search" "Search")
                                                (:a :href "/new" "New"))
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
                        ,@(loop with pages = (ceiling (/ total limit))
                                for p from 1 to pages
                                for pth-link-offset = (* limit (- p 1))
                                collect `(:a :class ,(if (= offset pth-link-offset)
                                                        "page-link here"
                                                        "page-link")
                                             :href ,(let ((url (format NIL "~A?limit=~A&offset=~A"
                                                                       here (url limit)
                                                                       (url pth-link-offset))))
                                                      (if more-params
                                                          (format NIL "~A&~A" url more-params)
                                                          url))
                                             ,(format NIL "~A" p)))))
              (:footer :id "footer"
                       (:hr)
                       (:a :href "https://sr.ht/~thalia/leibowitz"
                           "Project Home")
                       "//"
                       (:a :href "mailto:~thalia/leibowitz@lists.sr.ht"
                           "Report a Bug")))
              (:script :src "/static/fluff.js")))))

(defun return-404 (lib &optional msg)
  (setf (hunchentoot:return-code*) 404)
  (make-page lib :here ""
                 :title "404 Not Found | Leibowitz Web"
                 :body `((:section (:p ,(html msg))))))

(defun return-400 (lib &optional msg)
  (setf (hunchentoot:return-code*) 400)
  (make-page lib :here ""
                 :title "400 Bad Request | Leibowitz Web"
                 :body `((:section (:p ,(html msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-datum-listing-sidebar (lib)
  `((:section
     (:h2 "Search")
     (:form :method "get" :action "/search" :class "sidebar-form"
            (:input :name "q")
            (:button "Go!")))
    (:section
     (:h2 "Top Tags")
     (:ul ,@(loop for tag in (list-tags lib :limit 15)
                  collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                   (html (url (tag-name tag))))
                                    ,(html (tag-name tag)))
                                (:span :class "tag-count"
                                       ,(format nil "(~a)" (tag-count tag))))))
     (:a :href "/tags"
         (:b ,(format NIL "Show all ~A tags" (library-tag-quantity lib)))))
    (:section
     (:h2 "File Types")
     (:ul ,@(loop for type in (library-all-file-types lib)
                  collect `(:li (:a :href ,(format NIL "/type/~A" (car type))
                                    ,(car type))
                                (:small ,(format NIL "(~A)" (cdr type)))))))))

(defun list-data-as-html (lib view &rest options &key &allow-other-keys)
  "Beautify the output of `leibowitz.core:list-data' as a HTML datum
listing.  Key arguments are passed unmodified to that method."
  (check-type lib library)
  (let ((data (loop for datum in (apply #'list-data (nconc (list lib) options))
                    collect (datum-html-preview lib datum :view view))))
    `(,@(make-datum-listing-filter-bar view
                                       (getf options :sort-by)
                                       (getf options :direction))
      ,(cond ((eql view :tile) `(:section :id "tiles" ,@data))
             ((eql view :card) `(:section :id "cards" ,@data))))))

(defun list-search-results-as-html (lib terms limit offset sort-by direction view)
  (check-type lib library)
  (check-type terms string)
  `(,(make-search-page-search-box lib terms)
    ,@(make-datum-listing-filter-bar view sort-by direction)
    (:small "FIXME: query should support the same kinds of filters as list-data!")
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
                                                   (url (tag-name tag)))
                                    ,(tag-name tag))
                                (:span :class "tag-count"
                                       ,(format nil "(~a)" (tag-count tag)))
                                (:span :class "tag-desc"
                                       ,(tag-label tag))))))))

(defun make-tree-breadcrumbs (title absolute-path)
  `((:h1 ,title)
    (:nav
     (:ul :class "path-breadcrumbs"
          ,@(loop with path-so-far = "/"
                  for part in (pathname-directory absolute-path)
                  when (stringp part)
                    do (setf path-so-far
                             (concatenate 'string path-so-far part "/"))
                  when (stringp part)
                    collect `(:li
                              (:a :href ,(format NIL "/tree?dir=~A"
                                                 (url path-so-far))
                                  ,part)))))))

(defun make-tree-sidebar (dir)
  `((:section
     ;; FIXME: This allows index files outside of their
     ;; $LEIBOWITZ_ROOT or $HOME, which currently causes a
     ;; no-applicable-collection error and if allowed in practice will
     ;; almost certainly be a security issue.  Really, this problem
     ;; stems from the fact that we're storing absolute paths.  When
     ;; using $LEIBOWITZ_ROOT, the library should be fully portable
     ;; with respect to the location of that directory on the
     ;; filesystem.
     ,(index-files-form dir "Index files under this directory"))
    (:section
     (:h2 "Subdirectories")
     (:ul ,@(loop for sd in (reverse (uiop:subdirectories dir))
                  for name = (car (last (pathname-directory sd)))
                  collect `(:li (:a :href ,(format NIL "/tree?dir=~A"
                                                   (url
                                                    (namestring sd)))
                                    ,(html name))))))))


(defun list-contents-of-directory (lib dir)
  (let ((indexed NIL)
         (unindexed NIL))
    (mapcar (lambda (file)
              (etypecase file
                (datum (push file indexed))
                (pathname (push file unindexed))))
            (library-list-files-in-dir lib dir :include-unindexed T))
    `((:section
       (:p "FIXME: Add controls to index/reindex individual files or the whole directory")
       (:p "FIXME: Rework the core so I can have list-data controls here too!")
       (:p "FIXME: Eventually query will also need to be able to filter by directory..."))
      (:section
       (:h2 "Indexed files")
       (:div :id "tiles"
             ,@(loop for datum in indexed collect (datum-html-preview lib datum))))
      (:section
       (:h2 "Unindexed files")
       (:div
        :id "tiles"
        ,@(loop for f in unindexed
                collect `(:div :class "tile"
                               (:form :method "post" :action "/index"
                                      (:button :name "path" :value ,(html (url f))
                                               "Index file")
                                      (:label ,(html (pathname-name
                                                      (uiop:parse-unix-namestring f))))))))))))

(defun make-datum-view-page (lib datum)
  (incf (datum-accesses datum))
  (add-datum lib datum)
  (let ((action (format NIL "/datum?id=~A" (html (url (datum-id datum))))))
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
              (:div :id "editor-widget-right"
                    (:fieldset
                     (:legend "Move or Rename")
                     (:form :method "post"
                            :action ,action
                            (:input :type "text" :name "move-to"
                                    :value ,(html (datum-id datum)))
                            (:button "Move")))
                    (:fieldset
                     (:legend "Copy")
                     (:form :method "post"
                            :action ,action
                            (:input :type "text" :name "copy-to"
                                    :value ,(html (datum-id datum)))
                            (:button "Copy")))
                    (:fieldset
                     (:legend "Delete")
                     (:form :method "post"
                            :action ,action
                            (:button :name "delete"
                                     :value "yes"
                                     ,(format NIL "Permanently Delete ~A"
                                              (html (datum-title datum)))))))))))))

(defun make-tag-view-sidebar (lib tag)
  (check-type tag tag)
  (check-type lib library)
  `((:section
     (:h2 "About")
     (:ul (:li (:span :class "sidebar-metadata-key"
                      "Count")
               (:span :class "sidebar-metadata-var"
                      ,(format NIL "~A" (tag-count tag))))
          (:li (:span :class "sidebar-metadata-key"
                      "Label")
               (:span :class "sidebar-metadata-var"
                      ,(html (tag-label tag))))))
    (:section
     (:h2 "Automatically Adds")
     (:ul ,@(loop for tag in (get-tag-predicates lib tag)
                  collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                   (url (tag-name tag)))
                                    ,(tag-name tag))
                                (:span :class "tag-count"
                                       ,(format nil "(~a)" (tag-count tag)))))))
    (:section
     (:h2 "Automatically Added By")
     (:ul ,@(loop for tag in (get-tag-predicands lib tag)
                  collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                   (url (tag-name tag)))
                                    ,(tag-name tag))
                                (:span :class "tag-count"
                                       ,(format nil "(~a)" (tag-count tag)))))))
    (:section
     (:h2 "Related tags")
     (:p "I'll need to figure out something clever here :p"))))

(defun make-tag-view-page (lib tag view &rest options &key &allow-other-keys)
  (check-type lib library)
  (check-type tag tag)
  `(,@(let ((data (loop for datum in (apply #'get-tag-data (nconc (list lib tag) options))
                        collect (datum-html-preview lib datum :view view))))
        `(,@(make-datum-listing-filter-bar view
                                           (getf options :sort-by)
                                           (getf options :direction))
          (:small "FIXME get-tag-data should support sort-by/direction/limit/offset!")
          (:small "FIXME get-tag-data should support pagination!")
          ,(cond ((eql view :tile) `(:section :id "tiles" ,@data))
                 ((eql view :card) `(:section :id "cards" ,@data)))))
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
                             (loop for tag in (get-tag-predicates lib tag)
                                   do (format s "~A~%" (tag-name tag)))))
                         (:button :id "tag-editor-submit" "Save Parent Tags"))))
           (:div :id "editor-widget-right"
                 (:fieldset
                  (:legend "Edit Tag Description")
                  (:form :method "post"
                         (:textarea :id "description-editor-textarea"
                                    :name "description"
                                    :placeholder "Something about this tag"
                                    ,(html (tag-label tag)))
                         (:button :id "description-editor-submit"
                                  "Save Description")))
                 (:fieldset
                  (:legend "Rename Tag")
                  (:form :method "post"
                         (:input :type "text" :name "move-to" :value ,(html (tag-name tag)))
                         (:button "Rename")))
                 (:fieldset
                  (:legend "Copy Tag")
                  (:form :method "post"
                         (:input :type "text" :name "copy-to" :value ,(html (tag-name tag)))
                         (:button "Copy")))
                 (:fieldset
                  (:legend "Delete Tag")
                  (:form :method "post"
                         ;; FIXME: In order to ensure atomicity,
                         ;; del-tag (or some specific recursive
                         ;; deletion method) must support this.
                         ;; (:div :class "form-row"
                         ;;       (:input :type "checkbox"
                         ;;               :name "delete-children"
                         ;;               :id "delete-children")
                         ;;       (:label :for "delete-children"
                         ;;               "Also delete child tags."))
                         ;; (:div :class "form-row"
                         ;;       (:input :type "checkbox"
                         ;;               :name "delete-data"
                         ;;               :id "delete-data")
                         ;;       (:label :for "delete-data"
                         ;;               "Also delete this tag's
                         ;;               data."))
                         (:button :name "delete"
                                  :value "yes"
                                  ,(format NIL "Permanently Delete ~S"
                                           (html (tag-name tag)))))))))))

;; FIXME: Also filter by tags and terms once supported by the core
(defun make-datum-listing-filter-bar (view sort-by direction)
  `((:nav :id "listing-filter-controls"
          (:form :method "get" :id "datum-listing-filter-form"
                 (:labal :for "view"
                         "View As")
                 (:select :name "view"
                          :id "view"
                          ,(if (eql view :tile)
                               `(:option :value "tile" :selected "" "Tiles")
                               `(:option :value "tile" "Tiles"))
                          ,(if (eql view :card)
                               `(:option :value "card" :selected "" "Cards")
                               `(:option :value "card" "Cards")))
                 (:label :for "sort-by"
                         "Sort by")
                 (:select :name "sort-by"
                          :id "sort-by"
                          ,(if (eql sort-by :modified)
                               `(:option :value "modified" :selected "" "Date Modified")
                               `(:option :value "modified" "Date Modified"))
                          ,(if (eql sort-by :birth)
                               `(:option :value "birth" :selected "" "Date Created")
                               `(:option :value "birth" "Date Created"))
                          ,(if (eql sort-by :accesses)
                               `(:option :value "accesses" :selected "" "Number of Views")
                               `(:option :value "accesses" "Number of Views")))
                 (:span :class "checkbox-label-container"
                        ,(if (eql direction :descending)
                             `(:input :type "radio" :name "direction" :value "descending" :checked "")
                             `(:input :type "radio" :name "direction" :value "descending"))
                        (:label :for "descending" "Descending"))
                 (:span :class "checkbox-label-container"
                        ,(if (eql direction :ascending)
                             `(:input :type "radio" :name "direction" :value "ascending" :checked "")
                             `(:input :type "radio" :name "direction" :value "ascending"))
                        (:label :for "ascending" "Ascending"))
                 (:input :type "submit" :value "Filter")))))

(defun index-files-form (path msg)
  `(:form :method "post" :action "/index" :id "index-files-form"
            (:div :class "form-row"
                  (:input :type "text" :name "path"
                          :value ,(html (namestring path))))
            (:div :class "form-row"
                  (:input :type "submit" :value ,msg)0)))
