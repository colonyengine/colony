(in-package #:virality.engine)

(defvar *core-debug*)

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
  (loop :for ((kind data) found-p) = (multiple-value-list
                                      (pop-queue core :live-recompile))
        :while found-p
        :do (ecase kind
              (:shader
               (gpu:recompile-shaders data))
              ((:texture :material)
               (funcall data core)))))
