(in-package :gear)

(defmethod extension-file-types ((owner (eql 'context)))
  (list "cfg"))

(defun cfg (context key)
  (gethash key context))

(defun (setf cfg) (value context key)
  (setf (gethash key context) value))

(defmacro settings (&body body)
  `(let ()
     (declare (special *temp-context*))
     (loop :for (key value) :on ',body :by #'cddr
           :do (setf (gethash key *temp-context*) value))))
