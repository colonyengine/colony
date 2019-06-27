(in-package #:%first-light)

(defvar *buffer*)

(defclass buffer ()
  ((%bytes :reader %buffer-bytes
           :initarg :bytes)
   (%bits :reader %buffer-bits
          :initarg :bits)
   (%sequence :reader %buffer-sequence
              :initarg :sequence)
   (%stream :reader %buffer-stream
            :initarg :stream)))

(defun buffer-bytes ()
  (%buffer-bytes *buffer*))

(defun buffer-bits ()
  (%buffer-bits *buffer*))

(defun buffer-sequence ()
  (%buffer-sequence *buffer*))

(defun buffer-stream ()
  (%buffer-stream *buffer*))

(defun buffer-position ()
  (fast-io:buffer-position (buffer-bytes)))

(defun bits-read-sequence (sequence buffer &key (start 0) end)
  (fast-io:fast-read-sequence sequence buffer start end))

(defun make-buffer (sequence stream)
  (let* ((bytes (fast-io:make-input-buffer :vector sequence :stream stream))
         (bits (bitio:make-bitio bytes
                                 #'fast-io:fast-read-byte
                                 #'bits-read-sequence)))
    (make-instance 'buffer
                   :bytes bytes
                   :bits bits
                   :sequence (fast-io:input-buffer-vector bytes)
                   :stream (fast-io:input-buffer-stream bytes))))

(defmacro with-buffer-read ((&key sequence stream) &body body)
  `(let ((*buffer* (make-buffer ,sequence ,stream)))
     ,@body))

(defun %string-length (bytes null-terminated-p)
  (with-slots (%sequence) *buffer*
    (let* ((max-length (or bytes (length %sequence)))
           (start (buffer-position))
           (end (min (length %sequence) (+ start max-length)))
           (index (if null-terminated-p
                      (position 0 %sequence :start start :end end)
                      end)))
      (- index start))))

(defun split-string (string delimiter)
  (let ((pos (position delimiter string)))
    (list (subseq string 0 pos)
          (subseq string (1+ pos)))))

(defun octets= (octet-vector octet-list)
  (equalp octet-vector (fast-io:octets-from octet-list)))

(defun read-bits (count &key (processor #'identity))
  (funcall processor (bitio:read-bits (buffer-bits) count)))

(defun read-bytes (count &key (bits-per-byte 8) (processor #'identity))
  (let ((octet-vector (fast-io:make-octet-vector count)))
    (bitio:read-bytes (buffer-bits) octet-vector :bits-per-byte bits-per-byte)
    (funcall processor octet-vector)))

(defun read-uint-be (byte-count &key (bits-per-byte 8) (processor #'identity))
  (let ((value (bitio:read-integer (buffer-bits)
                                   :byte-endian :be
                                   :num-bytes byte-count
                                   :bits-per-byte bits-per-byte
                                   :unsignedp t)))
    (funcall processor value)))

(defun read-uint-le (byte-count &key (bits-per-byte 8) (processor #'identity))
  (let ((value (bitio:read-integer (buffer-bits)
                                   :byte-endian :le
                                   :num-bytes byte-count
                                   :bits-per-byte bits-per-byte
                                   :unsignedp t)))
    (funcall processor value)))

(defun read-int-be (byte-count &key (bits-per-byte 8) (processor #'identity))
  (let ((value (bitio:read-integer (buffer-bits)
                                   :byte-endian :be
                                   :num-bytes byte-count
                                   :bits-per-byte bits-per-byte
                                   :unsignedp nil)))
    (funcall processor value)))

(defun read-int-le (byte-count &key (bits-per-byte 8) (processor #'identity))
  (let ((value (bitio:read-integer (buffer-bits)
                                   :byte-endian :le
                                   :num-bytes byte-count
                                   :bits-per-byte bits-per-byte
                                   :unsignedp nil)))
    (funcall processor value)))

(defun read-string (&key bytes (encoding :ascii) (processor #'identity)
                      null-terminated-p)
  (with-slots (%bytes) *buffer*
    (let ((octet-vector (fast-io:make-octet-vector
                         (%string-length bytes null-terminated-p))))
      (fast-io:fast-read-sequence octet-vector %bytes)
      (when null-terminated-p
        (fast-io:fast-read-byte %bytes))
      (babel:octets-to-string
       (funcall processor octet-vector)
       :encoding encoding))))
