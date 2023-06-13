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
                                                (:a :href "/search" "Search")
                                                (:a :href "/shared" "Shared"))
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

(defun make-datum-listing-sidebar ()
  `((:section
     (:h2 "Search")
     (:form :method "get" :action "/search" :id "search-form"
            (:input :name "q")
            (:button "Go!")))
    (:section
     (:h2 "Top Tags")
    (:section
     (:h2 "File Types")))))
