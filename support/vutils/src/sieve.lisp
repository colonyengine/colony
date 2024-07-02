(in-package #:vutils)

;; TODO: This function really needs some documentation. It is used to
;; partition a pile of forms in a list into group based upon a categorization
;; function.

(defun sieve (pred sequ &key (key #'identity)
                          (values t)
                          (pred-range-sort (constantly nil))
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
                 (stable-sort result-list pred-range-sort :key #'first))
               (transformed-result-list
                 (mapcar (lambda (entry)
                           (list (first entry)
                                 (funcall result-transformer-func
                                          (second entry))))
                         sorted-result-list)))
          (if values
              (values-list transformed-result-list)
              transformed-result-list))))))
