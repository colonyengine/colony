(in-package :first-light.binary-formats)

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
         (bits (bitio:make-bitio bytes #'fast-io:fast-read-byte #'bits-read-sequence)))
    (make-instance 'buffer
                   :bytes bytes
                   :bits bits
                   :sequence (fast-io:input-buffer-vector bytes)
                   :stream (fast-io:input-buffer-stream bytes))))

(defmacro with-buffer-read ((&key sequence stream) &body body)
  `(let ((*buffer* (make-buffer ,sequence ,stream)))
     ,@body))
