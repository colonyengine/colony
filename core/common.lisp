(in-package :%first-light)

(defgeneric destroy (thing &key ttl)
  (:documentation "Destroy may take either an ACTOR or a COMPONENT. The keyword
argument :TTL supplied in real seconds, how long the thing has yet to live."))

(defun type-table (key type-table)
  (au:href type-table key))

(defun (setf type-table) (entry type-name-key type-table)
  (symbol-macrolet ((entry-table (au:href type-table type-name-key)))
    (unless (nth-value 1 entry-table)
      (setf entry-table (au:dict #'eq)))
    (setf (au:href entry-table entry) entry)))

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
  (loop :with keylen = (length key-list)
        :for test-fn :in (cdr test-fn-list)
        :for key :in key-list
        :for i :below keylen
        :for last-p = (= i (1- keylen))
        :with current-ht = ht
        :do (unless (nth-value 1 (gethash key current-ht))
              (unless last-p
                (setf (gethash key current-ht)
                      (au:dict (fdefinition test-fn)))))
            (setf current-ht (gethash key current-ht)))
  ht)

;; TODO: This function is not entirely correct in that it won't copy structures
;; or CLOS instances. This need fixing. I would guess this is actually hard to
;; do generally. WHen it becomes a problem we'll deal with it then.
(defun copy-thing (thing)
  (if (typep thing 'sequence)
      (map-into (copy-seq thing) #'copy-thing thing)
      thing))

(defun recompile-queued-items (core)
  (loop :with queue = (recompilation-queue core)
        :for ((kind data) found-p) = (multiple-value-list (fl.dst:qpop queue))
        :while found-p
        :do (ecase kind
              ;; NOTE: Look at function GENERATE-SHADER-MODIFY-HOOK for how we
              ;; put data into the recompilation queue that this case in the
              ;; ecase handles.
              (:shader-recompilation (fl.gpu:recompile-shaders data))
              ;; NOTE: You will need a similar one for putting prefab
              ;; recompilation tasks into the recompilation queue too.
              (:prefab-recompilation 'put-your-function-here))))
