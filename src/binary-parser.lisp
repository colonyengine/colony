(in-package #:virality.engine)

(defun get-string-length (buffer bytes null-terminated-p)
  (let* ((sequence (fast-io:input-buffer-vector buffer))
         (max-length (or bytes (length sequence)))
         (start (fast-io:buffer-position buffer))
         (end (min (length sequence) (+ start max-length)))
         (index (if null-terminated-p
                    (position 0 sequence :start start :end end)
                    end)))
    (- index start)))

(defun split-string (string delimiter)
  (let ((pos (position delimiter string)))
    (list (subseq string 0 pos)
          (subseq string (1+ pos)))))

(defun octets= (octet-vector octet-list)
  (equalp octet-vector (fast-io:octets-from octet-list)))

(defun read-bytes (buffer count &key (processor #'identity))
  (let ((octet-vector (fast-io:make-octet-vector count)))
    (fast-io:fast-read-sequence octet-vector buffer)
    (funcall processor octet-vector)))

(defun read-uint-be (buffer byte-count &key (processor #'identity))
  (let ((value 0))
    (loop :for i :from (* (1- byte-count) 8) :downto 0 :by 8
          :for byte = (fast-io:fast-read-byte buffer)
          :do (setf (ldb (byte 8 i) value) byte))
    (funcall processor value)))

(defun read-uint-le (buffer byte-count &key (processor #'identity))
  (let ((value 0))
    (loop :for i :below (* byte-count 8) :by 8
          :for byte = (fast-io:fast-read-byte buffer)
          :do (setf (ldb (byte 8 i) value) byte))
    (funcall processor value)))

(defun read-int-be (buffer byte-count &key (processor #'identity))
  (let* ((value (read-uint-be buffer byte-count :processor processor))
         (size (* byte-count 8)))
    (logior (* (ldb (byte 1 (1- size)) value)
               (- (expt 2 size)))
            value)))

(defun read-int-le (buffer byte-count &key (processor #'identity))
  (let* ((value (read-uint-le buffer byte-count :processor processor))
         (size (* byte-count 8)))
    (logior (* (ldb (byte 1 (1- size)) value)
               (- (expt 2 size)))
            value)))

(defun read-string (buffer &key bytes (encoding :ascii) (processor #'identity)
                             null-terminated-p)
  (let ((octet-vector (fast-io:make-octet-vector
                       (get-string-length buffer bytes null-terminated-p))))
    (fast-io:fast-read-sequence octet-vector buffer)
    (when null-terminated-p
      (fast-io:fast-read-byte buffer))
    (babel:octets-to-string
     (funcall processor octet-vector)
     :encoding encoding)))
