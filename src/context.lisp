(in-package :gear)

;;;; A context class. Currently this only stores the window size, which the
;;;; camera component uses.

;;; TODO: Think about adding the timing variables in here, or making use of
;;; box.fm:tick

(defclass context ()
  ((%width :accessor width
           :initarg :width
           :initform 800)
   (%height :accessor height
            :initarg :height
            :initform 450)))

(defun make-context (&rest initargs)
  (apply #'make-instance 'context initargs))
