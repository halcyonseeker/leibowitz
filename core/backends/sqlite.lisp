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

(defmethod print-object ((sqlite-library sqlite-library) stream)
  (print-unreadable-object (sqlite-library stream :type T)
    (format stream "~A" (slot-value sqlite-library 'db-path))))

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
-- FIXME bruh why are we tracking this, just use a select count(*) instead
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
end" "
create trigger if not exists update_junction_on_datum_rename after update on data begin
  update tag_datum_junctions set datum_id = new.id where datum_id = old.id;
end")))

(defmethod library-data-quantity ((l sqlite-library))
  (sqlite-row l "select count(*) from data"))

;;; FIXME: get this into one statement so that we can sort by count
;;; and group by the major part.  This is apparently Very Hard in SQL.
;;; FIXME: is it worth writing tests for this?
(defmethod library-all-file-types ((l sqlite-library))
  (loop for type in (sqlite-rows l "select distinct type from data")
        collect (cons (car type)
                      (sqlite-row
                       l "select count(*) from data where type = ?"
                       (car type)))))

(defmethod library-print-info ((l sqlite-library))
  (format T "SQLite Library on ~A with ~A data indexed~%"
          (namestring (slot-value l 'db-path)) (library-data-quantity l)))

(defmethod datum-num-tags ((l sqlite-library) (d datum))
  (sqlite-row l "select count(*) from tag_datum_junctions where datum_id = ? "
              (datum-id d)))

;;; Reading and writing data

;; FIXME: Make indexing jobs run in parallel!
(defmethod index ((l sqlite-library) (path-or-paths list)
                  &key (log T) (promote-error NIL))
  (mapcar (lambda (path) (index l path :log log :promote-error promote-error))
          path-or-paths))
(defmethod index ((l sqlite-library) (path-or-paths string)
                  &key (log T) (promote-error NIL))
  (index l (pathname path-or-paths) :log log :promote-error promote-error))
(defmethod index ((l sqlite-library) (path-or-paths pathname)
                  &key (log T) (promote-error NIL))
  (let ((indexed NIL))
    (macrolet ((with-logging ((p) &body body)
                 `(progn
                    (when log (format T "Indexing ~A..." ,p) (finish-output))
                    (handler-case
                        ,@body
                      ;; FIXME: `no-such-file', `file-not-readable',
                      ;; etc.  The fact that `no-such-file' errors end
                      ;; up as `no-applicable-collection' is really
                      ;; annoying, where would be the proper place to
                      ;; correct this?  Perhaps in `datum's
                      ;; `initialize-instance'?  Really I should rip
                      ;; out collections altogether, they're feeling
                      ;; more and more like an overly-complex and
                      ;; inconvenient way of having custom indexing
                      ;; rules for given directories.
                      (file-not-regular (c)
                        (if promote-error
                            (error c)
                            (format T "failed!~%Error: ~A~%" c)))
                      (:no-error (c)
                        (declare (ignore c))
                        (when log (format T "done~%")))))))
      (if (uiop:directory-exists-p path-or-paths)
          (cl-fad:walk-directory
           path-or-paths
           (lambda (path)
             (with-logging (path)
               (push (collection-index l (library-get-datum-collection l path) path)
                     indexed))))
          (with-logging (path-or-paths)
            (push (collection-index l (library-get-datum-collection l path-or-paths) path-or-paths)
                  indexed))))
    indexed))

(defmethod add-datum ((l sqlite-library) (d datum))
  (sqlite-nq l (ccat "insert or replace into data "
                     "(id, accesses, type, birth, modified, terms) "
                     "values (?, ?, ?, ?, ?, ?)")
             (datum-id d) (datum-accesses d) (datum-kind d) (datum-birth d)
             (datum-modified d) (datum-terms d))
  d)

(defmethod get-datum ((l sqlite-library) path-or-url &key (error NIL))
  (check-type path-or-url (or string pathname))
  (multiple-value-bind
        (id accesses kind birth modified terms)
      (sqlite-row l "select * from data where id = ?"
                  (if (pathnamep path-or-url) (namestring path-or-url) path-or-url))
    (if id
        (make-instance 'datum :id id :accesses accesses :kind kind :birth birth
                              :modified modified :terms terms
                              :collection (library-get-datum-collection l id))
        (if error
            (error 'datum-not-indexed :lib l :id (namestring path-or-url))
            NIL))))

(defun %del-datum-inner-transaction (lib id)
  (loop for tag in (get-datum-tags lib id)
        do (sqlite-nq lib (ccat "delete from tags where name = ? and "
                              "count = 0 and label is null")
                      (tag-name tag)))
  (sqlite-nq lib "delete from data where id = ?" id)
  (sqlite-nq lib "delete from tag_datum_junctions where datum_id = ?" id))

(defmethod del-datum ((l sqlite-library) datum-or-id &key (error NIL) (disk T))
  (check-type datum-or-id (or datum string pathname))
  (let ((id (%need-datum-id datum-or-id)))
    (when error (get-datum l id :error T))
    (with-sqlite-tx (l)
      (%del-datum-inner-transaction l id))
    ;; FIXME: this should be mediated by this datum's collection, and
    ;; will break with datum-link!  Use the former, remove the latter!
    ;; Placing this last means that if this datum is in db but not in
    ;; disk, the orphaned entries will be removed before an error is
    ;; signaled.
    (when disk (if (probe-file id)
                   (delete-file id)
                   (when error
                     (error 'datum-is-orphaned :id id))))))

;;; FIXME: Handle non-file data, they don't have entries on disk.  Or
;;; maybe just tear the link stuff out altogether, there's probably a
;;; better way of doing that...
(defmethod move-datum ((l sqlite-library) old-datum-or-id new-datum-or-id
                             &key (overwrite NIL))
  (check-type old-datum-or-id (or string pathname datum))
  (check-type new-datum-or-id (or string pathname datum))
  (let* ((old (%need-datum-id old-datum-or-id))
         (new (%need-datum-id new-datum-or-id))
         ;; FIXME: aren't we appending an image file extension?  I
         ;; think there'll be a bug here!
         (oldth (merge-pathnames old (library-thumbnail-cache-dir l)))
         (newth (merge-pathnames new (library-thumbnail-cache-dir l))))
    (when (equal old new) (error 'cannot-mv-or-cp-to-itself :d new))
    (unless (get-datum l old) (error 'datum-not-indexed :lib l :id old))
    (when (and (or (get-datum l new) (probe-file new))
               (not overwrite))
      (error 'datum-already-exists :d new))
    (when (and (not (probe-file old))
               (not (probe-file new)))
      (error 'datum-is-orphaned :id old))
    ;; It's okay if old doesn't exist on disk as long as the entry is
    ;; still in the DB; the user might have moved it and want to
    ;; update the ID.
    (when (probe-file old)
      (rename-file old new)
      ;; The user probably passed a relative path for new, and we want
      ;; the absolute for all ids.
      (setf new (namestring (truename new))))
    (when (probe-file oldth)
      (rename-file oldth newth))
    ;; Triggers do the hard lifting of keeping everything up to date.
    (with-sqlite-tx (l)
      (%del-datum-inner-transaction l new)
      (sqlite-nq l "update data set id = ? where id = ?" new old))
    (namestring new)))

(defmethod copy-datum ((l sqlite-library) old-datum-or-id new-datum-or-id
                       &key (overwrite NIL))
  (check-type old-datum-or-id (or string pathname datum))
  (check-type new-datum-or-id (or string pathname datum))
  (let* ((old (%need-datum-id old-datum-or-id))
         (new (%need-datum-id new-datum-or-id))
         (oldth (merge-pathnames old (library-thumbnail-cache-dir l)))
         (newth (merge-pathnames new (library-thumbnail-cache-dir l))))
    (when (equal old new) (error 'cannot-mv-or-cp-to-itself :d new))
    (unless (get-datum l old) (error 'datum-not-indexed :lib l :id old))
    (unless (probe-file old) (error 'datum-is-orphaned :lib l :id old))
    (when (and (or (get-datum l new) (probe-file new))
               (not overwrite))
      (error 'datum-already-exists :d new))
    (uiop:copy-file old new)
    (uiop:copy-file oldth newth)
    (with-sqlite-tx (l)
      (let* ((new (namestring (truename new)))
             (datum (get-datum l old))
             (tags (get-datum-tags l datum)))
        (setf (datum-id datum) new)
        (add-datum l datum)
        (if overwrite
            (%add-datum-tags-inner-transaction l datum tags :replace T)
            (%add-datum-tags-inner-transaction l datum tags))))
    ;; FIXME: with-sqlite-tx clobbers return values
    (get-datum l new)))

;;; Reading and writing tags

;; FIXME: again, tag-count could easily be found by a method that does
;; a select count(*) query on the junction table so we wouldn't have
;; to use multiple queries to update the label.
(defun %add-tag-inner-transaction (l tag)
  (let* ((tag   (if (stringp tag) (make-instance 'tag :name tag) tag))
         (prev-label (sqlite-row l "select label from tags where name = ?" (tag-name tag)))
         (curr-label (if (slot-boundp tag 'label) (tag-label tag) NIL)))
    ;; We insert-or-ignore here in order to not clobber count.
    (sqlite-nq l "insert or ignore into tags (name, label, count) values (?, ?, ?)"
               (tag-name tag) curr-label (tag-count tag))
    (when curr-label
      (unless (equal prev-label curr-label)
        (sqlite-nq l "update tags set label = ? where name = ?"
                   curr-label (tag-name tag))))
    tag))

(defmethod add-tag ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or tag string))
  (with-sqlite-tx (l)
    (%add-tag-inner-transaction l tag-or-name))
  ;; fixme with-sqlite-tx clobbers return value
  (get-tag l (%need-tag-name tag-or-name)))

(defmethod get-tag ((l sqlite-library) tag-name &key (error NIL))
  (check-type tag-name string)
  (multiple-value-bind
        (name label count)
      (sqlite-row l "select * from tags where name = ?" tag-name)
    (if name
        (make-instance 'tag :name name :label label :count count)
        (if error
            (error 'no-such-tag :name tag-name)
            NIL))))

(defun %del-tag-inner-transaction (l name)
  (check-type l sqlite-library)
  (check-type name string)
  (sqlite-nq l "delete from tags where name = ?" name)
  (sqlite-nq l "delete from tag_datum_junctions where tag_name = ?" name)
  (sqlite-nq l "delete from tag_predicates where iftag = ? or thentag = ?"
             name name))

(defmethod del-tag ((l sqlite-library) tag-or-name)
  (check-type tag-or-name (or datum string))
  (with-sqlite-tx (l)
    (%del-tag-inner-transaction l (%need-tag-name tag-or-name))))

(defmethod move-tag ((l sqlite-library) old-tag-or-name new-tag-or-name
                     &key (overwrite NIL) (merge NIL))
  (check-type old-tag-or-name (or string tag))
  (check-type new-tag-or-name (or string tag))
  (check-type overwrite boolean)
  (check-type merge boolean)
  (assert (not (and merge overwrite)))
  (let ((old (%need-tag-name old-tag-or-name))
        (new (%need-tag-name new-tag-or-name)))
    (when (equal old new) (error 'cannot-mv-or-cp-to-itself :d new))
    (unless (get-tag l old) (error 'no-such-tag :name old))
    (when (and (get-tag l new)
               (not (or overwrite merge)))
      (error 'tag-already-exists :name new))
    (with-sqlite-tx (l)
      (when overwrite
        (%del-tag-inner-transaction l new))
      (if merge
          (let ((old-tag (get-tag l old)))
            (sqlite-nq l "update tags set label = ? where name = ?" (tag-label old-tag) new)
            (sqlite-nq l "update tags set count = count + ? where name = ?" (tag-count old-tag) new)
            (sqlite-nq l "delete from tags where name = ?" old)
            ;; Since we're merging, the data of old all need to be
            ;; updated to the existing new's predicate tags.  Worst
            ;; case runtime of O(fuck-my-life-I-hate-graphs).
            (let* ((predicates (%cascade-down-predicate-tree l new))
                   (tags (loop for tag being each hash-key of predicates
                               collect tag)))
              (loop for datum in (get-tag-data l old)
                    do (%add-datum-tags-inner-transaction l datum tags))))
          (sqlite-nq l "update tags set name = ? where name = ?" new old))
      (sqlite-nq l "update tag_datum_junctions set tag_name = ? where tag_name = ?" new old)
      (sqlite-nq l "update tag_predicates set iftag = ? where iftag = ?" new old)
      (sqlite-nq l "update tag_predicates set thentag = ? where thentag = ?" new old))
    ;; FIXME: with-sqlite-tx clobbers return values
    (get-tag l new)))

(defmethod copy-tag ((l sqlite-library) old-tag-or-name new-tag-or-name
                     &key (overwrite NIL) (merge NIL))
  (check-type old-tag-or-name (or string tag))
  (check-type new-tag-or-name (or string tag))
  (check-type overwrite boolean)
  (check-type merge boolean)
  (assert (not (and merge overwrite)))
  (let ((old (%need-tag-name old-tag-or-name))
        (new (%need-tag-name new-tag-or-name)))
    (when (equal old new) (error 'cannot-mv-or-cp-to-itself :d new))
    (unless (get-tag l old) (error 'no-such-tag :name old))
    (when (and (get-tag l new)
               (not (or overwrite merge)))
      (error 'tag-already-exists :name new))
    (with-sqlite-tx (l)
      (when overwrite (%del-tag-inner-transaction l new))
      (let ((new-tag (make-instance 'tag :name new
                                         :label (tag-label (get-tag l old)))))
        (%add-tag-inner-transaction l new-tag))
      (loop for predicate in (get-tag-predicates l old)
            do (%add-tag-predicate-inner-transaction l new (list predicate)))
      (loop for datum in (get-tag-data l old)
            do (%add-datum-tags-inner-transaction l datum (list new))))
    ;; FIXME: with-sqlite-tx clobbers return values
    (get-tag l new)))

;;; Reading and writing datum-tag relationships

;; FIXME: if tags is a list of tag instances, their labels are
;; discarded!
(defun %add-datum-tags-inner-transaction (lib datum-or-id tags &key replace)
  "Same deal as `%del-datum-tags-inner-transaction'."
  (check-type datum-or-id (or datum pathname string))
  (check-type tags list)
  (labels ((add-assoc (l name id)
             (%add-tag-inner-transaction l name)
             (sqlite-nq l (ccat "insert into tag_datum_junctions "
                                "(tag_name, datum_id) values (?, ?)")
                        name id)))
    (when replace
      (%del-datum-tags-inner-transaction
       lib datum-or-id (get-datum-tags lib datum-or-id)))
    (loop for tag in tags
          for id = (%need-datum-id datum-or-id)
          for name = (%need-tag-name tag)
          for required-tags = (%cascade-down-predicate-tree lib name)
          do (loop for required-tag being each hash-key of required-tags
                   do (add-assoc lib required-tag id)))))

(defmethod add-datum-tags ((l sqlite-library) datum-or-id tags &key (replace NIL))
  (unless (get-datum l (%need-datum-id datum-or-id))
    (error 'datum-not-indexed :lib l :id (%need-datum-id datum-or-id)))
  (with-sqlite-tx (l)
    (%add-datum-tags-inner-transaction l datum-or-id tags :replace replace)))

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

(defmethod get-tag-data ((l sqlite-library) tag-or-name
                         &key (sort-by :modified) (direction :descending)
                           (limit NIL) (offset NIL))
  (assert (member sort-by '(:modified :birth :accesses)))
  (assert (member direction '(:descending :ascending)))
  (check-type limit (or null integer))
  (check-type offset (or null integer))
  (check-type tag-or-name (or string tag))
  (when (or limit offset) (assert (and limit offset)))
  (loop for row in (sqlite-rows l (format NIL (ccat "select data.* from data "
                                                    "inner join tag_datum_junctions on "
                                                    "datum_id = data.id where tag_name = ? "
                                                    "order by ~A ~A ~A")
                                          (cond ((eql sort-by :modified) "modified")
                                                ((eql sort-by :birth) "birth")
                                                ((eql sort-by :accesses) "accesses"))
                                          (cond ((eql direction :descending) "desc")
                                                ((eql direction :ascending) "asc"))
                                          (if (and limit offset)
                                              (format NIL "limit ~A offset ~A" limit offset)
                                              ""))
                                (%need-tag-name tag-or-name))
        collect (destructuring-bind (id accesses kind birth modified terms) row
                  (make-instance 'datum :id id :accesses accesses :kind kind
                                        :birth birth :modified modified
                                        :terms terms
                                        :collection (library-get-datum-collection l id)))))

;;; Reading and writing tag hierarchies

(defun %add-tag-predicate-inner-transaction (l iftag-or-name thentags-or-names
                                             &key (retroactive T) (replace NIL))
  (labels ((add-assoc (ifname thenname)
             (let ((ifname (%need-tag-name ifname))
                   (thenname (%need-tag-name thenname)))
               ;; FIXME: if this methods arguments are tags we'll
               ;; lose information like labels!  In general the core
               ;; library's handling of different argument types is
               ;; abysmally convoluted.
               (unless (get-tag l ifname)
                 (%add-tag-inner-transaction l ifname))
               (unless (get-tag l thenname)
                 (%add-tag-inner-transaction l thenname))
               ;; FIXME: give del-tag-predicate a retroactive option
               ;; and pass it whatever value we were passed here
               (sqlite-nq l (ccat "insert or ignore into tag_predicates "
                                  "(iftag, thentag) values (?, ?)")
                          ifname thenname)
               (when retroactive
                 ;; Big O of deez nuts
                 (let ((predicates (%cascade-down-predicate-tree l iftag-or-name)))
                   (loop for tag being each hash-key of predicates
                         do (loop for datum in (get-tag-data l tag)
                                  do (%add-datum-tags-inner-transaction
                                      l datum (list thenname)))))))))
    ;; FIXME: refactor del-tag-predicate to optionally take a list
    (when replace
      (loop for tag in (get-tag-predicates l iftag-or-name)
            do (del-tag-predicate l iftag-or-name tag)))
    (etypecase thentags-or-names
      ((or string tag) (add-assoc iftag-or-name thentags-or-names))
      (list (loop for thentag-or-name in thentags-or-names
                  do (add-assoc iftag-or-name thentag-or-name))))))

(defmethod add-tag-predicate ((l sqlite-library) iftag-or-name thentags-or-names
                              &key (retroactive T) (replace NIL))
  (check-type iftag-or-name (or tag string))
  (with-sqlite-tx (l)
    (%add-tag-predicate-inner-transaction
     l iftag-or-name thentags-or-names :replace replace :retroactive retroactive)))

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
;; FIXME: the search terms must follow FTS's search syntax and we will
;; receive an error if they contain certain stray characters.  This
;; may be fixed by double-quoting the entire string, but then we lose
;; the search syntax.  Figure out a policy here!
(defmethod query ((l sqlite-library) terms &key (sort-by :rank) (direction :ascending)
                                             (limit NIL) (offset NIL))
  (assert (member sort-by '(:rank :modified :birth :accesses)))
  (assert (member direction '(:descending :ascending)))
  (check-type terms string)
  (check-type offset (or null integer))
  (check-type limit (or null integer))
  (when (or offset limit) (assert (and offset limit)))
  (loop for row in (sqlite-rows
                    l (format NIL (ccat "select data.* from search "
                                        "left join data on data.id = search.id "
                                        "where search match ? order by ~A ~A ~A")
                              (cond ((eql sort-by :rank) "rank")
                                    ((eql sort-by :modified) "modified")
                                    ((eql sort-by :birth) "birth")
                                    ((eql sort-by :accesses) "accesses"))
                              (cond ((eql direction :descending) "desc")
                                    ((eql direction :ascending) "asc"))
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

