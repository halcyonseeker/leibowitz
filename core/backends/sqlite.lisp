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
    (if id
        (make-instance 'datum :id id :kind kind :birth birth
                              :modified modified :terms terms)
        NIL)))

(defmethod del-datum ((l sqlite-library) datum-or-id)
  (check-type datum-or-id (or datum string pathname))
  (sqlite-nq l "delete from data where id = ?"
             (etypecase datum-or-id
               (string datum-or-id)
               (pathname (namestring datum-or-id))
               (datum (datum-id datum-or-id))))
  T)

;;; Reading and writing tags

(defmethod add-tag ((l sqlite-library) tag)
  (check-type tag (or tag string))
  (unless (sqlite-row l "select * from tags where name = ?"
                      (etypecase tag (tag (tag-name tag)) (string tag)))
    (when (stringp tag)
      (setf tag (make-instance 'tag :name tag)))
    (sqlite-nq l "insert into tags (name, label, count) values (?, ?, ?)"
               (tag-name tag)
               (if (slot-boundp tag 'label)
                   (tag-label tag)
                   NIL)
               (tag-count tag)))
  T)

(defmethod get-tag ((l sqlite-library) name)
  (check-type name string)
  (multiple-value-bind
        (name label count)
      (sqlite-row l "select * from tags where name = ?" name)
    (if name
        (make-instance 'tag :name name :label label :count count)
        NIL)))

(defmethod del-tag ((l sqlite-library) tag))

;;; Reading and writing datum-tag relationships

(defmethod add-datum-tags ((l sqlite-library) datum-or-id tags)
  (check-type datum-or-id (or datum pathname string))
  (check-type tags list)
  (loop for tag in tags
        for id = (etypecase datum-or-id
                   (string datum-or-id)
                   (pathname (namestring datum-or-id))
                   (datum (datum-id datum-or-id)))
        for name = (etypecase tag (tag (tag-name tag)) (string tag))
        do (with-sqlite-tx (l)
             (add-tag l tag)
             (sqlite-nq l (ccat "insert into tag_datum_junctions "
                                "(tag_name, datum_id) values (?, ?)")
                        name id)
             (sqlite-nq l (ccat "update tags set count = count + 1 "
                                "where name = ?")
                        name))))

(defmethod get-datum-tags ((l sqlite-library) datum-or-id)
  (check-type datum-or-id (or datum pathname string))
  (loop for row in (sqlite-rows l (ccat "select tags.* from tags "
                                        "inner join tag_datum_junctions "
                                        "on tag_name = name where datum_id = ?")
                                (etypecase datum-or-id
                                  (string datum-or-id)
                                  (pathname (namestring datum-or-id))
                                  (datum (datum-id datum-or-id))))
        collect (destructuring-bind (name label count) row
                  (make-instance 'tag :name name :count count :label label))))

(defmethod del-datum-tags ((l sqlite-library) datum-or-id tags)
  (check-type datum-or-id (or datum pathname string))
  (check-type tags list)
  (loop for tag in tags
        for name = (etypecase tag (tag (tag-name tag)) (string tag))
        for id = (etypecase datum-or-id
                   (string datum-or-id)
                   (pathname (namestring datum-or-id))
                   (datum (datum-id datum-or-id)))
        do (with-sqlite-tx (l)
             (sqlite-nq l (ccat "delete from tag_datum_junctions "
                                "where tag_name = ? and datum_id = ?")
                        name id)
             (sqlite-nq l (ccat "update tags set count = count - 1 "
                                "where name = ?")
                        name)
             (sqlite-nq l (ccat "delete from tags where name = ? and "
                                "count = 0 and label is null")
                        name))))

(defmethod get-tag-data ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or string tag))
  (loop for row in (sqlite-rows l (ccat "select data.* from data "
                                        "inner join tag_datum_junctions on "
                                        "datum_id = data.id where tag_name = ?")
                                (etypecase tag-or-name
                                  (tag (tag-name tag-or-name))
                                  (string tag-or-name)))
        collect (destructuring-bind (id kind birth modified terms) row
                  (make-instance 'datum :id id :kind kind :birth birth
                                        :modified modified :terms terms))))

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
       (T (c)
         (sqlite-nq ,sqlite-library "rollback")
         (error c))
       (:no-error (c)
         (declare (ignore c))
         (sqlite-nq ,sqlite-library "commit")))))

(defmacro ccat (&rest strings)
  "Concatenate some strings at compile-time.  Used internally to shorten
lines with really long SQL queries."
  (format NIL "~{~A~}" strings))
