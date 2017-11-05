(in-package :gear)

(defun parse-shader-dictionary (name spec)
  spec)

(defmethod extension-file-types ((owner (eql 'shader)))
  (list "shd"))

(defmacro shader-dictionary (name (&key enabled) &body body)
  `(let ()
     (declare (special %temp-shader))
     ,(when enabled
        `(setf (gethash ,name %temp-shader)
               (apply #'parse-shader-dictionary ,name ',body)))))
