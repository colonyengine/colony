(in-package #:colony.lockable)

(defclass lockable ()
  ((%lock-state :reader lock-state
                :initarg :lock-state
                :initform nil)))

;; TODO: Can this record a tree of locks so we can do deadlock detection?
;;
;; TODO: Make this work with classes and somehow with structures.
(defmacro with-lock ((lockable-item) &body body)
  (declare (ignore lockable-item))
  `(progn ,@body))
