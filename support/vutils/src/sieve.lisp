(in-package #:vutils)

;; TODO: This function really needs some documentation. It is used to
;; partition a pile of forms in a list into group based upon a categorization
;; function.

(defun sieve (pred sequ &key (key #'identity)
                          (collector nil)
                          (values t)
                          (pred-range-sort nil)
                          (initial-key-pool nil) ;; ensure all buckets present!
                          (result-transformer-func #'identity)
                          (decorate-position nil))

  (let ((result (make-hash-table :test #'equal)))
    ;; Initialize the key pool if supplied.
    (when initial-key-pool
      (dolist (initial-key initial-key-pool)
        (setf (gethash initial-key result) nil)))

    (flet ((separator-func (elem pos)
             (let ((decision (funcall pred (funcall key elem))))
               (let ((presentp (nth-value 1 (gethash decision result))))
                 (unless presentp
                   (setf (gethash decision result) nil))
                 (push (if decorate-position
                           (list pos elem)
                           elem)
                       (gethash decision result))))))

      (loop :for elem :in sequ
            :for pos :by 1
            :do (separator-func elem pos))

      (let ((result-list nil))
        (maphash (lambda (k v)
                   (push (list k (nreverse v)) result-list))
                 result)
        (let* ((sorted-result-list
                 (if pred-range-sort
                     (stable-sort result-list pred-range-sort :key #'first)
                     result-list))
               (transformed-result-list
                 (mapcar (lambda (entry)
                           (list (first entry)
                                 (funcall result-transformer-func
                                          (second entry))))
                         sorted-result-list))
               (collected-list
                 (if collector
                     (mapcar collector transformed-result-list)
                     transformed-result-list)))
          (if values
              (values-list collected-list)
              collected-list))))))

(defun sort-by-symbols (sym-order &key (unknown :end))
  "Returns a comparison function suitable for STABLE-SORT which sorts
list of symbols into the order specified by SYM-ORDER. Any symbols found not in
the SYM-ORDER will be kept in the same order as found in the list being sorted,
but placed at the location defined by the :UNKNOWN keyword argument--which can
be the :START or :END of the list and defaults to :END. The function returned
by SORT-BY-SYMBOLS is often passed to the :PRED-RANGE-SORT keyword argument for
the SIEVE function."
  (let* ((tbl (make-hash-table))
         (minval -1)
         (maxval 0))
    (dolist (sym sym-order)
      (setf (gethash sym tbl) maxval)
      (incf maxval))
    (let ((default-val (ecase unknown (:end maxval) (:start minval))))
      (lambda (l r)
        (let ((lval (gethash l tbl default-val))
              (rval (gethash r tbl default-val)))
          (< lval rval))))))
