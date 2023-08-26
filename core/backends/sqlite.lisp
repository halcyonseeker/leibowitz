;; A library backend to store the ontology in an SQLite database.

(in-package :leibowitz.core)

;; Helper macros; these need to be declared before they're called.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (ensure-directories-exist (directory-namestring (slot-value l 'db-path)))
  (setf (slot-value l 'handle) (sqlite:connect (slot-value l 'db-path)))
  (mapcar (lambda (tbl) (sqlite:execute-non-query (slot-value l 'handle) tbl))
          '("
create table if not exists 'data' (
  'id' text not null unique,
  'accesses' integer not null,
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
create virtual table if not exists 'search' using fts5 (
  'terms',
  'id',
  content='data'
)" "
create trigger if not exists inc_tag_count after insert on tag_datum_junctions begin
  update tags set count = count + 1 where name = new.tag_name;
end" "
create trigger if not exists dec_tag_count after delete on tag_datum_junctions begin
  update tags set count = count - 1 where name = old.tag_name;
  delete from tags where name = old.tag_name and count = 0 and label is null;
end" "
create trigger if not exists data_into_fts after insert on data begin
  insert into search (id, terms) values (new.id, new.terms);
end" "
create trigger if not exists data_from_fts after delete on data begin
  insert into search (search, id, terms) values ('delete', old.id, old.terms);
end
" "
create trigger if not exists data_update_fts after update on data begin
  insert into search (search, id, terms) values ('delete', old.id, old.terms);
  insert into search (id, terms) values (new.id, new.terms);
end")))

(defmethod library-data-quantity ((l sqlite-library))
  (sqlite-row l "select count(*) from data"))

;;; Reading and writing data

;; FIXME: calling on a five mb git repo is monstrously slow...
(defmethod index ((l sqlite-library) path-or-url)
  (check-type path-or-url (or string pathname))
  (cond ((uiop:directory-exists-p path-or-url)
         (let ((data NIL))
           (cl-fad:walk-directory
            path-or-url
            (lambda (path)
              (push (collection-index l (library-get-datum-collection l path) path)
                    data)))
           data))
        (T (list (collection-index
                  l (library-get-datum-collection l path-or-url) path-or-url)))))

(defmethod add-datum ((l sqlite-library) (d datum))
  (sqlite-nq l (ccat "insert or replace into data "
                     "(id, accesses, type, birth, modified, terms) "
                     "values (?, ?, ?, ?, ?, ?)")
             (datum-id d) (datum-accesses d) (datum-kind d) (datum-birth d)
             (datum-modified d) (datum-terms d))
  d)

(defmethod get-datum ((l sqlite-library) path-or-url)
  (check-type path-or-url (or string pathname))
  (multiple-value-bind
        (id accesses kind birth modified terms)
      (sqlite-row l "select * from data where id = ?"
                  (if (pathnamep path-or-url) (namestring path-or-url) path-or-url))
    (if id
        (make-instance 'datum :id id :accesses accesses :kind kind :birth birth
                              :modified modified :terms terms
                              :collection (library-get-datum-collection l id))
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

(defmethod add-datum-tags ((l sqlite-library) datum-or-id tags &key (replace NIL))
  (check-type datum-or-id (or datum pathname string))
  (check-type tags list)
  (labels ((add-assoc (l name id)
             (add-tag l name)
             (sqlite-nq l (ccat "insert into tag_datum_junctions "
                                "(tag_name, datum_id) values (?, ?)")
                        name id)))
    (with-sqlite-tx (l)
      (when replace
        (%del-datum-tags-inner-transaction l datum-or-id (get-datum-tags l datum-or-id)))
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

(defun %del-datum-tags-inner-transaction (lib datum-or-id tags &key cascade)
  "All the tag operations require multiple update/insert/delete queries,
which we want to run atomically in a single transaction in order to
avoid corrupting the db.  Unfortunately, transactions cannot be nested
and `add-datum-tags' needs to call `del-datum-tags' within its own
transaction, hence this little helper function."
  (labels ((del-assoc (l name id)
             (sqlite-nq l (ccat "delete from tag_datum_junctions "
                                "where tag_name = ? and datum_id = ?")
                        name id)
             (sqlite-nq l (ccat "delete from tags where name = ? and "
                                "count = 0 and label is null")
                        name)))
    (loop for tag in tags
          for name = (%need-tag-name tag)
          for id = (%need-datum-id datum-or-id)
          do (del-assoc lib name id)
             (when cascade
               (loop for req being each hash-key of (%cascade-down-predicate-tree lib name)
                     do (del-assoc lib req id))))))

(defmethod del-datum-tags ((l sqlite-library) datum-or-id tags &key (cascade NIL))
  (check-type datum-or-id (or datum pathname string))
  (check-type tags list)
  (with-sqlite-tx (l)
    (%del-datum-tags-inner-transaction l datum-or-id tags :cascade cascade)))

(defmethod get-tag-data ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or string tag))
  (loop for row in (sqlite-rows l (ccat "select data.* from data "
                                        "inner join tag_datum_junctions on "
                                        "datum_id = data.id where tag_name = ?")
                                (%need-tag-name tag-or-name))
        collect (destructuring-bind (id accesses kind birth modified terms) row
                  (make-instance 'datum :id id :accesses accesses :kind kind
                                        :birth birth :modified modified
                                        :terms terms
                                        :collection (library-get-datum-collection l id)))))

;;; Reading and writing tag hierarchies

(defmethod add-tag-predicate ((l sqlite-library) iftag-or-name thentag-or-name)
  (check-type iftag-or-name (or tag string))
  (check-type thentag-or-name (or tag string))
  (let ((ifname (%need-tag-name iftag-or-name))
        (thenname (%need-tag-name thentag-or-name)))
    (with-sqlite-tx (l)
      (unless (get-tag l ifname)
        (add-tag l (make-instance 'tag :name ifname)))
      (unless (get-tag l thenname)
        (add-tag l (make-instance 'tag :name thenname)))
      (sqlite-nq l (ccat "insert or ignore into tag_predicates (iftag, thentag)"
                         "values (?, ?)")
                 ifname thenname))))

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

;;; Searching and Listing

;; FIXME: Improve search!  We should be able to search in different or
;; all fields (id, body, tags) and filter for tags at the same time as
;; searching for text.

;; FIXME: add support for tag filtering
(defmethod query ((l sqlite-library) terms &key (limit NIL) (offset NIL))
  (check-type terms string)
  (check-type offset (or null integer))
  (check-type limit (or null integer))
  (when (or offset limit) (assert (and offset limit)))
  (loop for row in (sqlite-rows
                    l (format NIL "~A ~A ~A ~A"
                              "select data.* from search"
                              "left join data on data.id = search.id"
                              "where search match ? order by rank"
                              (if (and limit offset)
                                  (format NIL "limit ~A offset ~A" limit offset)
                                  ""))
                    terms)
        collect (destructuring-bind (id accesses kind birth modified terms) row
                  (make-instance 'datum :id id :accesses accesses :kind kind
                                        :birth birth :modified modified
                                        :terms terms
                                        :collection (library-get-datum-collection l id)))))

;; FIXME: currently untested
;; FIXME: how would we sort by popularly viewed tags?
(defmethod list-tags ((l sqlite-library))
  (loop for row in (sqlite-rows l "select * from tags order by count desc")
        collect (destructuring-bind (name label count) row
                  (make-instance 'tag :name name :count count :label label))))

;; FIXME: Track datum tag-count so we can sort by them
(defmethod list-data ((l sqlite-library) &key (sort-by :modified) (direction :descending)
                                           (limit NIL) (offset NIL))
  (assert (member sort-by '(:modified :birth :accesses)))
  (assert (member direction '(:descending :ascending)))
  (check-type limit (or null integer))
  (check-type offset (or null integer))
  (when (or limit offset) (assert (and limit offset)))
  (loop for row in (sqlite-rows
                    l (format NIL "select * from data order by ~A ~A ~A"
                              (cond ((eql sort-by :modified) "modified")
                                    ((eql sort-by :birth) "birth")
                                    ((eql sort-by :accesses) "accesses"))
                              (cond ((eql direction :descending) "desc")
                                    ((eql direction :ascending) "asc"))
                              (if (and limit offset)
                                  (format NIL "limit ~A offset ~A" limit offset)
                                  "")))
        collect (destructuring-bind (id accesses kind birth modified terms) row
                  (make-instance 'datum :id id :accesses accesses :kind kind
                                        :birth birth :modified modified
                                        :terms terms
                                        :collection (library-get-datum-collection l id)))))

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

