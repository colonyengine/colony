(in-package #:virality)

;; Implementation of UUID (gotten and modified from mfiano's random-uuid
;; project).

(u:define-printer (uuid stream)
  (format stream "~a" (uuid->string uuid)))

(u:fn-> valid-string-p (string) boolean)
(declaim (inline valid-string-p))
(defun valid-string-p (string)
  "Check whether the given string is a valid representation of a UUID."
  (and (char= (char string 8) (char string 13)
              (char string 18) (char string 23)
              #\-)
       (= (nth-value 1 (parse-integer (remove #\- string)
                                      :radix 16 :junk-allowed t)) 32)))

(u:fn-> uuid->string (uuid) string)
(defun uuid->string (uuid)
  "Convert a UUID to its string representation."
  (declare (optimize speed))
  (macrolet ((%write (string hextable count offset bits word)
               (declare (type fixnum count offset bits))
               `(setf ,@(loop :for i :of-type fixnum :below count
                              :collect `(aref ,string ,(+ offset i))
                              :collect `(aref ,hextable
                                              (ldb (byte 4 ,(- bits (* i 4)))
                                                   ,word))))))
    (let ((hextable "0123456789abcdef")
          (high (uuid-high uuid))
          (low (uuid-low uuid))
          (string (make-string 36 :element-type 'base-char)))
      (psetf (aref string 8) #\-
             (aref string 13) #\-
             (aref string 18) #\-
             (aref string 23) #\-)
      (%write string hextable 8 0 60 high)
      (%write string hextable 4 9 28 high)
      (%write string hextable 4 14 12 high)
      (%write string hextable 4 19 60 low)
      (%write string hextable 12 24 44 low)
      string)))

(u:fn-> string->uuid ((simple-string 36)) uuid)
(defun string->uuid (string)
  "Construct a UUID object by parsing the string representation of a UUID."
  (declare (optimize speed))
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
    (declare (inline parse-variant))
    (unless (valid-string-p string)
      (error 'invalid-string :string string))
    (let* ((string (remove #\- string))
           (high (parse-integer string :end 16 :radix 16))
           (low (parse-integer string :start 16 :radix 16)))
      (declare (type u:ub64 high low))
      (%make-uuid :version (ldb (byte 4 12) high)
                  :variant (parse-variant (ldb (byte 3 61) low))
                  :low low
                  :high high))))

(u:fn-> make-uuid (&optional (or null function)) uuid)
(defun make-uuid (&optional generator)
  "Construct a new RFC-4122 v4 UUID. GENERATOR is a function that takes two
values, a min and max integer, and generates a number inclusive to that
range. This allows generating a deterministic sequence of random UUID's."
  (declare (optimize speed)
           (inline %make-uuid))
  (flet ((%random ()
           (if generator
               (let ((max (1- (expt 2 32))))
                 (dpb (funcall generator 0 max)
                      (byte 32 32)
                      (ldb (byte 32 0) (funcall generator 0 max))))
               (random (expt 2 64)))))
    (declare (inline %random))
    (%make-uuid :version 4
                :low (dpb 4 (byte 3 61) (%random))
                :high (dpb 4 (byte 4 12) (%random)))))
