(in-package :gear)

(defmethod extension-file-types ((owner (eql 'settings)))
  (list "cfg"))

(defun prepare-settings (core-state path)
  (let ((*context* (make-hash-table :test #'eq)))
    (flet ((%prepare ()
             (load-extensions 'settings path)
             *context*))
      (merge-context core-state (%prepare))
      core-state)))

(defmacro settings ((&key enabled) &body body)
  (when enabled
    `(loop :for (key value) :on ',body :by #'cddr
           :do (setf (gethash key *context*) value))))

(defun cfg (context key)
  (gethash key context))

(defun (setf cfg) (value core-state key)
  (setf (gethash key context) value))
