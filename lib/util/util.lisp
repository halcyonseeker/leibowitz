
(defpackage :leibowitz.util
  (:use #:cl)
  (:export #:url
           #:html))

(in-package :leibowitz.util)

(defun html (str)
  (typecase str
    (string (cl-who:escape-string str))
    (T str)))

(defun url (str)
  (typecase str
    (string
     ;; Copied strait from hunchentoot:url-encode except that we
     ;; url-encode single quotes too.
     (with-output-to-string (s)
       (loop for c across str
             for index from 0
             do (cond ((or (char<= #\0 c #\9)
                           (char<= #\a c #\z)
                           (char<= #\A c #\Z)
                           ;; Note the lack of a single-quote here
                           (find c "$-_.!*()" :test #'char=))
                       (write-char c s))
                      (t (loop for octet across (hunchentoot::string-to-octets
                                                 str :start index :end (1+ index)
                                                     :external-format hunchentoot::+utf-8+)
                               do (format s "%~2,'0x" octet)))))))
    (T str)))

