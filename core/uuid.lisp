(in-package :%first-light)

(defmacro write-uuid-chunk (string count offset bits word)
  `(setf
    ,@(loop :for i :below count
            :collect `(aref ,string ,(+ offset i))
            :collect `(aref "0123456789ABCDEF"
                            (ldb (byte 4 ,(- bits (* i 4)))
                                 ,word)))))

(defun make-uuid ()
  (declare (optimize speed))
  (symbol-macrolet ((rand
                      (random #.(expt 2 64)
                              (load-time-value (make-random-state t)))))
    (let* ((high (dpb #b100 (byte 3 61) rand))
           (low (dpb 4 (byte 4 12) rand))
           (string (make-string 36 :element-type 'base-char)))
      (psetf (aref string 8) #\-
             (aref string 13) #\-
             (aref string 18) #\-
             (aref string 23) #\-)
      (write-uuid-chunk string 8 0 60 high)
      (write-uuid-chunk string 4 9 28 high)
      (write-uuid-chunk string 4 14 12 high)
      (write-uuid-chunk string 4 19 60 low)
      (write-uuid-chunk string 12 24 44 low)
      string)))
