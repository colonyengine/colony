(in-package :first-light.util)

;;; Macros

(defmacro fn-> (function args values)
  "Declaim the `FTYPE` of function from `ARGS` to `VALUES`."
  `(declaim (ftype (function ,args ,values) ,function)))

(defmacro define-printer ((object stream &key (type t) identity) &body body)
  "Define a PRINT-OBJECT method for `OBJECT`."
  `(defmethod print-object ((,object ,object) ,stream)
     (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
       ,@body)))

(defmacro defun-inline (name &body body)
  "Conveniently define the function `NAME` and also inline it."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     ',name))

(defmacro when-found ((var lookup) &body body)
  "If `LOOKUP` is successful, perform `BODY` with `VAR` bound to the result.
`LOOKUP` is an expression that returns two values, with the second value indicating if the lookup
was successful, such as with GETHASH."
  (with-unique-names (found)
    `(multiple-value-bind (,var ,found) ,lookup
       (declare (ignorable ,var))
       (when ,found
         ,@body))))

(defmacro unless-found ((var lookup) &body body)
  "If `LOOKUP` is not successful, perform `BODY` with `VAR` bound to the result.
`LOOKUP` is an expression that returns two values, with the second value indicating if the lookup
was successful, such as with GETHASH."
  (with-unique-names (found)
    `(multiple-value-bind (,var ,found) ,lookup
       (declare (ignorable ,var))
       (unless ,found
         ,@body))))

(defmacro if-found ((var lookup) then else)
  "Depending if `LOOKUP` is successful or not, perform `THEN` or `ELSE` with `VAR` bound to the
result. `LOOKUP` is an expression that returns two values, with the second value indicating if the
lookup was successful, such as with GETHASH."
  (with-unique-names (found)
    `(multiple-value-bind (,var ,found) ,lookup
       (if ,found
           ,then
           ,else))))

(defmacro while (predicate &body body)
  "Loop until `PREDICATE` returns NIL."
  `(loop :while ,predicate
         :do ,@body))

;;; Strings

(defun string-starts-with-p (string prefix)
  "Check if `STRING` starts with `PREFIX`."
  (let ((prefix-length (length prefix)))
    (when (<= prefix-length (length string))
      (string= prefix (subseq string 0 prefix-length)))))

;;; Sequences

(defun flatten (sequence)
  "Traverses a sequence in order, collecting non-nil values into a list. This is different than
Alexandria's version in that this also works for vectors or hybrid sequences."
  (let ((list))
    (labels ((traverse (sub-tree)
               (when sub-tree
                 (typecase sub-tree
                   (cons
                    (traverse (car sub-tree))
                    (traverse (cdr sub-tree)))
                   (vector
                    (map nil #'traverse sub-tree))
                   (t (push sub-tree list))))))
      (traverse sequence))
    (nreverse list)))

;;; Association lists

(defun alist-get (alist key &rest args)
  "Get the value associated with `KEY` in `ALIST`."
  (let ((cell (apply #'assoc key alist args)))
    (values (cdr cell) cell)))

;;; Property lists

(deftype plist () '(satisfies plist-p))

(defun plist-p (item)
  "Check whether or not `ITEM` is a property list."
  (and (listp item)
       (evenp (length item))
       (every #'keywordp
              (loop :for element :in item :by #'cddr
                    :collect element))))

(defun plist-values (plist)
  "Get a list of all values in `PLIST`."
  (if (plist-p plist)
      (loop :for (key value) :on plist :by #'cddr
            :collect value)
      (error "~a is not a property list." plist)))


(defun plist->hash (plist &rest args)
  "Convert `PLIST` to a hash table."
  (if (plist-p plist)
      (let ((table (apply #'make-hash-table args)))
        (loop :for (key value) :on plist :by #'cddr
              :do (setf (gethash key table) value))
        table)
      (error "~a is not a property list." plist)))

;;; Hash tables

(defmacro do-hash ((key value table &optional result) &body body)
  "Iterates over hash table `TABLE`, binding each key to `KEY`, and its value to `VALUE`."
  (with-unique-names (block-name)
    (multiple-value-bind (body decls) (alexandria:parse-body body)
      `(block ,block-name
         (maphash
          (lambda (,key ,value)
            ,@decls
            (tagbody ,@body))
          ,table)
         ,(when result
            `(let (,key ,value)
               ,result))))))

(defmacro do-hash-keys ((key table) &body body)
  "Iterate over hash table `TABLE`, binding each key to `KEY`."
  (with-unique-names (value)
    `(do-hash (,key ,value ,table)
       (declare (ignore ,value))
       ,@body)))

(defmacro do-hash-values ((value table) &body body)
  "Iterate over hash table `TABLE`, binding each value to `VALUE`."
  (with-unique-names (key)
    `(do-hash (,key ,value ,table)
       (declare (ignore ,key))
       ,@body)))

(defun maphash-keys (fn table)
  "Iterate over the keys of the hash table `TABLE`, calling the function `FN` on each."
  (do-hash (key value table)
    (declare (ignore value))
    (funcall fn key)))

(defun maphash-values (fn table)
  "Iterate over the values of the hash table `TABLE`, calling the function `FN` on each."
  (do-hash (key value table)
    (declare (ignore key))
    (funcall fn value)))

(defun hash-keys (table)
  "Collect a list of all keys in the hash table `TABLE`."
  (collecting (maphash-keys #'collect table)))

(defun hash-values (table)
  "Collect a list of all values in the hash table `TABLE."
  (collecting (maphash-values #'collect table)))

(defun hash->alist (table)
  "Convert the keys and values of the hash table `TABLE` to an association list."
  (let (result)
    (do-hash (key value table)
      (push (cons key value) result))
    result))

;;; Filesystem

(defmacro with-file-input ((stream path) &body body)
  "Open the file at location `PATH` as input and perform `BODY`."
  `(with-open-file (,stream ,path :direction :input
                                  :if-does-not-exist :error)
     ,@body))

(defmacro with-file-output ((stream path) &body body)
  "Open the file at location `PATH`, as output and perform `BODY`.
If the file already exists, it is overwritten.
If the file does not exist, it is created."
  `(with-open-file (,stream ,path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
     ,@body))

(defmacro with-binary-input ((stream file) &body body)
  "Open the file at location `PATH` as binary input and perform `BODY`."
  `(with-open-file (,stream ,file :direction :input
                                  :if-does-not-exist :error
                                  :element-type 'octet)
     ,@body))

(defmacro with-binary-output ((stream file) &body body)
  "Open the file at location `PATH`, as binary output and perform `BODY`.
If the file already exists, it is overwritten.
If the file does not exist, it is created."
  `(with-open-file (,stream ,file :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :element-type 'octet)
     ,@body))

(defun resolve-system-path (system &optional path)
  "Resolve the absolute path of the filesystem where `PATH` is located, relative to the ASDF system,
`SYSTEM`, or relative to the program location in the case of running a dumped Lisp image from the
command line.
Note: A dumped image must have either been created with UIOP:DUMP-IMAGE, or have manually set
UIOP/IMAGE:*IMAGE-DUMPED-P* prior to dumping."
  (if uiop/image:*image-dumped-p*
      (truename (uiop/pathname:merge-pathnames*
                 path
                 (uiop:pathname-directory-pathname (uiop:argv0))))
      (asdf/system:system-relative-pathname (asdf:find-system system) path)))

(defun map-files (path function &key (test (constantly t)) (recursive-p t))
  "Map over all files located in the directory of `PATH`, applying `FUNCTION` to each file's path.
`TEST` is a function that takes a file path and decides if `FUNCTION` should be applied to it.
`RECURSIVE-P`, when non-NIL will descend into sub-directories of `PATH` recursively."
  (labels ((process-files (dir)
             (map nil
                  (lambda (x)
                    (when (funcall test x)
                      (funcall function x)))
                  (uiop/filesystem:directory-files dir))))
    (uiop/filesystem:collect-sub*directories
     (uiop/pathname:ensure-directory-pathname path) t recursive-p #'process-files)))

(defun get-directory-contents (path)
  (uiop:directory* (uiop:merge-pathnames* uiop:*wild-file* path)))

(defun copy-directory (source target)
  (labels ((recurse (path target)
             (if (uiop:directory-pathname-p path)
                 (let* ((source-sub-dir (first (last (pathname-directory path))))
                        (new-target (uiop:merge-pathnames*
                                     (make-pathname :directory `(:relative ,source-sub-dir))
                                     target)))
                   (dolist (sub-path (get-directory-contents path))
                     (recurse sub-path new-target)))
                 (progn
                   (ensure-directories-exist target)
                   (uiop:copy-file path (make-pathname :name (pathname-name path)
                                                       :type (pathname-type path)
                                                       :defaults target))))))
    (recurse source target)))

(defun safe-read-file-form (path &key (package :cl))
  "Read the first form of the file located at `PATH`, with *PACKAGE* bound to `PACKAGE`."
  (with-standard-io-syntax
    (let ((*package* (find-package package))
          (*read-eval* nil))
      (with-open-file (in path)
        (read in)))))

(defun safe-read-file-forms (path &key (package :cl))
  "Read all forms of the file located at `PATH`, with *PACKAGE* bound to `PACKAGE`."
  (with-standard-io-syntax
    (let ((*package* (find-package package))
          (*read-eval* nil))
      (with-open-file (in path)
        (loop :for form = (read in nil in)
              :until (eq form in)
              :collect form)))))

;;; Math

(defun-inline degrees->radians (degrees)
  "Convert `DEGREES` to radians."
  (* degrees (/ pi 180)))

(defun-inline radians->degrees (radians)
  "Convert `RADIANS` to degrees."
  (* radians (/ 180 pi)))

(defun-inline map-domain (source-min source-max dest-min dest-max value)
  "Map `VALUE` from the domain denoted by `SOURCE-MIN` and `SOURCE-MAX` to the domain denoted by
`DEST-MIN` and `DEST-MAX`."
  (alexandria:lerp (/ (- value source-min)
                      (- source-max source-min))
                   dest-min
                   dest-max))

;;; Miscellaneous

(defun noop (&rest args)
  "Do nothing."
  (declare (ignore args))
  (values))
