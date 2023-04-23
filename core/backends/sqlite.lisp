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
  'datum_id' text not null,
  unique(tag_name, datum_id) on conflict ignore
)" "
create table if not exists 'tag_predicates' (
  'iftag' text not null,
  'thentag' text not null,
  unique(iftag, thentag) on conflict ignore
)" "
create trigger if not exists inc_tag_count after insert on tag_datum_junctions begin
  update tags set count = count + 1 where name = new.tag_name;
end
" "
create trigger if not exists dec_tag_count after delete on tag_datum_junctions begin
  update tags set count = count - 1 where name = old.tag_name;
  delete from tags where name = old.tag_name and count = 0 and label is null;
end
")))

;;; Reading and writing data

(defmethod add-datum ((l sqlite-library) (d datum))
  (sqlite-nq
   l "insert into data (id, type, birth, modified, terms) values (?, ?, ?, ?, ?)"
   (datum-id d) (datum-kind d) (datum-birth d) (datum-modified d)
   (datum-terms d))
  d)

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
  (let ((id (%need-datum-id datum-or-id)))
    (with-sqlite-tx (l)
      (loop for tag in (get-datum-tags l id)
            do (sqlite-nq l (ccat "delete from tags where name = ? and "
                                  "count = 0 and label is null")
                          (tag-name tag)))
      (sqlite-nq l "delete from data where id = ?" id)
      (sqlite-nq l "delete from tag_datum_junctions where datum_id = ?" id))))

;;; Reading and writing tags

(defmethod add-tag ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or tag string))
  (when (stringp tag-or-name)
    (setf tag-or-name (make-instance 'tag :name tag-or-name)))
  (sqlite-nq l "insert or ignore into tags (name, label, count) values (?, ?, ?)"
             (tag-name tag-or-name)
             (if (slot-boundp tag-or-name 'label)
                 (tag-label tag-or-name)
                 NIL)
             (tag-count tag-or-name))
  T)

(defmethod get-tag ((l sqlite-library) name)
  (check-type name string)
  (multiple-value-bind
        (name label count)
      (sqlite-row l "select * from tags where name = ?" name)
    (if name
        (make-instance 'tag :name name :label label :count count)
        NIL)))

(defmethod del-tag ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or datum string))
  (let ((name (%need-tag-name tag-or-name)))
    (with-sqlite-tx (l)
      (sqlite-nq l "delete from tags where name = ?" name)
      (sqlite-nq l "delete from tag_datum_junctions where tag_name = ?" name)
      (sqlite-nq l "delete from tag_predicates where iftag = ? or thentag = ?"
                 name name))))

;;; Reading and writing datum-tag relationships

(defmethod add-datum-tags ((l sqlite-library) datum-or-id tags)
  (check-type datum-or-id (or datum pathname string))
  (check-type tags list)
  (labels ((add-assoc (l name id)
             (add-tag l name)
             (sqlite-nq l (ccat "insert into tag_datum_junctions "
                                "(tag_name, datum_id) values (?, ?)")
                        name id)))
    (with-sqlite-tx (l)
      (loop for tag in tags
            for id = (%need-datum-id datum-or-id)
            for name = (%need-tag-name tag)
            for required-tags = (%cascade-down-predicate-tree l name)
            do (loop for required-tag being each hash-key of required-tags
                     do (add-assoc l required-tag id))))))

(defmethod get-datum-tags ((l sqlite-library) datum-or-id)
  (check-type datum-or-id (or datum pathname string))
  (loop for row in (sqlite-rows l (ccat "select tags.* from tags "
                                        "inner join tag_datum_junctions "
                                        "on tag_name = name where datum_id = ?")
                                (%need-datum-id datum-or-id))
        collect (destructuring-bind (name label count) row
                  (make-instance 'tag :name name :count count :label label))))

(defmethod del-datum-tags ((l sqlite-library) datum-or-id tags &key (cascade NIL))
  (check-type datum-or-id (or datum pathname string))
  (check-type tags list)
  (labels ((del-assoc (l name id)
             (sqlite-nq l (ccat "delete from tag_datum_junctions "
                                "where tag_name = ? and datum_id = ?")
                        name id)
             (sqlite-nq l (ccat "delete from tags where name = ? and "
                                "count = 0 and label is null")
                        name)))
    (with-sqlite-tx (l)
      (loop for tag in tags
            for name = (%need-tag-name tag)
            for id = (%need-datum-id datum-or-id)
            do (del-assoc l name id)
               (when cascade
                 (loop for req being each hash-key of (%cascade-down-predicate-tree l name)
                       do (del-assoc l req id)))))))

(defmethod get-tag-data ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or string tag))
  (loop for row in (sqlite-rows l (ccat "select data.* from data "
                                        "inner join tag_datum_junctions on "
                                        "datum_id = data.id where tag_name = ?")
                                (%need-tag-name tag-or-name))
        collect (destructuring-bind (id kind birth modified terms) row
                  (make-instance 'datum :id id :kind kind :birth birth
                                        :modified modified :terms terms))))

;;; Reading and writing tag hierarchies

(defmethod add-tag-predicate ((l sqlite-library) iftag-or-name thentag-or-name)
  (check-type iftag-or-name (or tag string))
  (check-type thentag-or-name (or tag string))
  (let ((ifname (%need-tag-name iftag-or-name))
        (thenname (%need-tag-name thentag-or-name)))
    (unless (get-tag l ifname)
      (add-tag l (make-instance 'tag :name ifname)))
    (unless (get-tag l thenname)
      (add-tag l (make-instance 'tag :name thenname)))
    (sqlite-nq l (ccat "insert or ignore into tag_predicates (iftag, thentag)"
                       "values (?, ?)")
               ifname thenname)))

(defmethod get-tag-predicates ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or tag string))
  (loop for row in (sqlite-rows l (ccat "select tags.* from tags "
                                        "inner join tag_predicates "
                                        "on thentag = name where iftag = ?")
                                (%need-tag-name tag-or-name))
        collect (destructuring-bind (name label count) row
                  (make-instance 'tag :name name :count count :label label))))

(defmethod get-tag-predicands ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or tag string))
  (loop for row in (sqlite-rows l (ccat "select tags.* from tags "
                                        "inner join tag_predicates "
                                        "on iftag = name where thentag = ?")
                                (%need-tag-name tag-or-name))
        collect (destructuring-bind (name label count) row
                  (make-instance 'tag :name name :count count :label label))))

(defmethod del-tag-predicate ((l sqlite-library) iftag-or-name thentag-or-name)
  (check-type iftag-or-name (or tag string))
  (check-type thentag-or-name (or tag string))
  (let ((ifname (%need-tag-name iftag-or-name))
        (thenname (%need-tag-name thentag-or-name)))
    (sqlite-nq l "delete from tag_predicates where iftag = ? and thentag = ?"
               ifname thenname)))

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

(defun %cascade-down-predicate-tree (lib root &optional (tbl NIL))
  "Given ROOT as the root tag of a tag hierarchy, traverse down it and
return a hash table of all tags that should be added.  If we encounter
a tag that is already in the table of tags to add, simply skip it; ie,
cycles in the tag hierarchy are just ignored."
  ;; Create the hash table the first time this function is called
  (unless tbl (setf tbl (make-hash-table :test #'equal)))
  ;; Do a recursive breadth-first search of the graph, skipping
  ;; whenever we find a tag that was previously encountered in order
  ;; to avoid recursing infinitely.
  (setf (gethash root tbl) root)
  (loop for tag in (get-tag-predicates lib root)
        for name = (tag-name tag)
        unless (gethash (tag-name tag) tbl)
          do (setf (gethash (tag-name tag) tbl) (tag-name tag))
             (%cascade-down-predicate-tree lib (tag-name tag) tbl))
  tbl)

