;; A library backend to store the ontology in an SQLite database.

(in-package :leibowitz-core)

(defclass sqlite-library (library)
  ((db-path
    :type (or string pathname)
    :initarg :db-path
    :initform (error "db-path required.")
    :documentation "The path to the SQLite database.")
   (handle
    :type sqlite:sqlite-handle
    :documentation "The handle through which database queries are made."))
  (:documentation "A library backend to store the ontology in a SQLite database."))

(defmethod initialize-instance :after
    ((l sqlite-library) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value l 'handle) (sqlite:connect (slot-value l 'db-path)))
  (mapcar (lambda (tbl) (sqlite:execute-non-query (slot-value l 'handle) tbl))
          '("
create table if not exists 'data' (
  'id' text not null unique,
  'type' text not null,
  'birth' datetime not null,
  'modified' datetime not null,
  'terms' text
)" "
create table if not exists 'tags' (
  'name' text not null unique,
  'label' text,
  'count' integer
)" "
create table if not exists 'tag_datum_junctions' (
  'tag_name' text not null,
  'datum_id' text not null
)" "
create table if not exists 'tag_predicates' (
  'iftag' text not null,
  'thentag' text not null
)")))

;;; Reading and writing data

(defmethod add-datum ((l sqlite-library) (d datum))
  (sqlite-nq
   l "insert into data (id, type, birth, modified, terms) values (?, ?, ?, ?, ?)"
   (datum-id d) (datum-kind d) (datum-birth d) (datum-modified d)
   (datum-terms d))
  T)

(defmethod get-datum ((l sqlite-library) id)
  (check-type id (or string pathname))
  (multiple-value-bind
        (id kind birth modified terms)
      (sqlite-row l "select * from data where id = ?" (namestring id))
    (make-instance 'datum :id id :kind kind :birth birth
                          :modified modified :terms terms)))

(defmethod del-datum ((l sqlite-library) datum))

;;; Reading and writing tags

(defmethod add-tag ((l sqlite-library) tag))

(defmethod get-tag ((l sqlite-library) name))

(defmethod del-tag ((l sqlite-library) tag))

;;; Reading and writing datum-tag relationships

(defmethod get-datum-tags ((l sqlite-library) datum))

(defmethod add-datum-tags ((l sqlite-library) datum tags))

(defmethod del-datum-tags ((l sqlite-library) datum tags))

(defmethod get-tag-data ((l sqlite-library) tag))

;;; Reading and writing tag hierarchies

(defmethod add-tag-predicate ((l sqlite-library) iftag thentag))

(defmethod get-tag-predicates ((l sqlite-library) tag))

(defmethod add-tag-predicands ((l sqlite-library) tag))

(defmethod del-tag-predicate ((l sqlite-library) iftag thentag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Methods

(defmethod sqlite-nq ((l sqlite-library) &rest args)
  "Run a non-query on this database."
  (apply #'sqlite:execute-non-query (nconc (list (slot-value l 'handle)) args)))

(defmethod sqlite-row ((l sqlite-library) &rest args)
  "Run a query on this database that returns a single row."
  (apply #'sqlite:execute-one-row-m-v (nconc (list (slot-value l 'handle)) args)))

(defmethod sqlite-rows ((l sqlite-library) &rest args)
  "Run a query on this database that returns multiple rows."
  (apply #'sqlite:execute-to-list (nconc (list (slot-value l 'handle)) args)))

(defmacro with-sqlite-tx ((sqlite-library) &body body)
  "Run BODY as an atomic SQLite transaction."
  `(progn
     (sqlite-nq ,sqlite-library "begin transaction")
     (handler-case (progn ,@body)
       (T () (sqlite-nq ,sqlite-library "rollback"))
       (:no-error () (sqlite-nq ,sqlite-library "commit")))))
