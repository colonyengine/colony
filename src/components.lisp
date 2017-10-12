(in-package :gear)

(defclass component ()
  ((%game-object :accessor game-object
                 :initarg :game-object)))

(defgeneric make-component (type &rest args)
  (:method ((type symbol) &rest args)
    (apply #'make-instance type args)))

(defgeneric add-component (game-object component)
  ;; NOTE: Currently we don't check if we push two of the same type.
  (:method ((game-object game-object) (component component))
    (push component (components game-object))))

(defgeneric get-component (type game-object)
  (:method ((type symbol) (game-object game-object))
    (find-if
     (lambda (x) (typep x type))
     (components game-object))))

(defun add-multiple-components (game-object components)
  (dolist (component components)
    (add-component game-object component)))
