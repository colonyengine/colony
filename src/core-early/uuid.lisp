(in-package #:virality)

;; Implementation of UUID

(u:define-printer (uuid stream)
  (format stream "~a" (uuid->string uuid)))

(defun uuid->string (uuid)
  (declare (optimize speed))
  (macrolet ((%write (string hextable count offset bits word)
               (declare (type fixnum count offset bits))
               `(setf ,@(loop :for i :of-type fixnum :below count
                              :collect `(aref ,string ,(+ offset i))
                              :collect `(aref ,hextable
                                              (ldb (byte 4 ,(- bits (* i 4)))
                                                   ,word))))))
    (check-type uuid uuid)
    (let ((hextable "0123456789ABCDEF")
          (high (uuid-high uuid))
          (low (uuid-low uuid))
          (string (make-string 36 :element-type 'base-char)))
      (locally (declare (optimize (safety 0)))
        (psetf (aref string 8) #\-
               (aref string 13) #\-
               (aref string 18) #\-
               (aref string 23) #\-)
        (%write string hextable 8 0 60 high)
        (%write string hextable 4 9 28 high)
        (%write string hextable 4 14 12 high)
        (%write string hextable 4 19 60 low)
        (%write string hextable 12 24 44 low))
      string)))

(defun string->uuid (string)
  (check-type string (simple-string 36))
  (flet ((parse-variant (bits)
           (cond
             ((not (logbitp 2 bits))
              :reserved/ncs)
             ((not (logbitp 1 bits))
              :rfc-4122)
             ((not (logbitp 0 bits))
              :reserved/microsoft)
             (t
              :reserved/future))))
    (declare (inline parse-variant %make-uuid))
    (let* ((string (remove #\- string))
           (high (parse-integer string :end 16 :radix 16))
           (low (parse-integer string :start 16 :radix 16)))
      (declare (type u:ub64 high low))
      (%make-uuid :version (ldb (byte 4 12) high)
                  :variant (parse-variant (ldb (byte 3 61) low))
                  :low low
                  :high high))))

(defun make-uuid ()
  (declare (optimize speed)
           (inline %make-uuid))
  (symbol-macrolet ((rand (random (expt 2 64))))
    (%make-uuid :version 4
                :low (dpb #b100 (byte 3 61) rand)
                :high (dpb 4 (byte 4 12) rand))))
