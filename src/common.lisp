(in-package #:%first-light)

(defgeneric destroy-after-time (thing &key ttl)
  (:documentation "Takes either an ACTOR or a COMPONENT. The keyword argument
:TTL supplied in real seconds, how long the thing has yet to live, with NIL
meaning infinity."))

(defun destroy (thing)
  (destroy-after-time thing :ttl 0))

(defun type-table (key type-table)
  (u:href type-table key))

(defun (setf type-table) (entry type-name-key type-table)
  (symbol-macrolet ((entry-table (u:href type-table type-name-key)))
    (unless (nth-value 1 entry-table)
      (setf entry-table (u:dict)))
    (setf (u:href entry-table entry) entry)))

(defun type-table-drop (component component-type type-table)
  (remhash component (type-table component-type type-table)))

(defun eql/package-relaxed (obj1 obj2)
  (cond
    ((eql obj1 obj2) t)
    ((and (symbolp obj1) (symbolp obj2))
     (string= (symbol-name obj1)
              (symbol-name obj2)))))

(defun ensure-nested-hash-table (ht test-fn-list key-list)
  "Walk down the nested hash table `HT` ensuring that we have the correct number
of hash tables made (one less than the `KEY-LIST` set of keys) with the correct
tests, gotten from `TEST-FN-LIST`. NOTE: The first entry in `TEST-FN-LIST` is
the test function for `HT` itself."
  ;; TODO: This looks painful for performance., oh well, we'll see if the
  ;; profiler actually cares or not. It is likely that these won't be nested
  ;; deeply. Also, the algorithm is slightly painful, but the deal is that we
  ;; can't make a hash table containing the last key, since the last key is
  ;; where we'll either look something up or store it.
  (loop :with keylen = (length key-list)
        :for test-fn :in (cdr test-fn-list)
        :for key :in key-list
        :for i :below keylen
        :for lastp = (= i (1- keylen))
        :with current-ht = ht
        :do (unless (nth-value 1 (gethash key current-ht))
              ;; If the key doesn't exist, we make a new hash table and store it
              ;; at the key UNLESS it is the last entry, in which case we do
              ;; nothing.
              (unless lastp
                (setf (gethash key current-ht)
                      (u:dict (fdefinition test-fn)))))
            ;; The key is potentially newly minted.
            (setf current-ht (gethash key current-ht)))
  ht)

;; TODO: This function is not entirely correct in that it won't copy structures
;; or CLOS instances. This need fixing. I would guess this is actually hard to
;; do generally. WHen it becomes a problem we'll deal with it then.
(defun copy-thing (thing)
  (if (typep thing 'sequence)
      (map-into (copy-seq thing) #'copy-thing thing)
      thing))

;;; Recompilation queue

(defun recompile-queued-items (core)
  (loop :with queue = (recompilation-queue core)
        :for ((kind data) found-p) = (multiple-value-list (queues:qpop queue))
        :while found-p
        :do (ecase kind
              ;; NOTE: Look at function GENERATE-SHADER-MODIFY-HOOK for how we
              ;; put data into the recompilation queue that this case in the
              ;; ecase handles.
              (:shader-recompilation (fl.gpu:recompile-shaders data))
              ;; NOTE: You will need a similar one for putting prefab
              ;; recompilation tasks into the recompilation queue too.
              (:prefab-recompilation 'put-your-function-here))))

(defun get-time ()
  #+sbcl
  (u:mvlet ((s ms (sb-ext:get-time-of-day)))
    (+ (- s (load-time-value (sb-ext:get-time-of-day)))
       (/ ms 1d6)))
  #-sbcl
  (float (/ (get-internal-real-time) internal-time-units-per-second) 1d0))

(defun resolve-system-path (system &optional path)
  "Resolve the absolute path of the filesystem where `PATH` is located, relative
to the ASDF system, `SYSTEM`, or relative to the program location in the case of
running a dumped Lisp image from the command line. Note: A dumped image must
have either been created with UIOP:DUMP-IMAGE, or have manually set
UIOP/IMAGE:*IMAGE-DUMPED-P* prior to dumping."
  (if uiop/image:*image-dumped-p*
      (truename (uiop/pathname:merge-pathnames*
                 path
                 (uiop:pathname-directory-pathname (uiop:argv0))))
      (asdf/system:system-relative-pathname (asdf:find-system system) path)))

(defmacro without-float-traps (&body body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-sbcl
  `(progn ,@body))
