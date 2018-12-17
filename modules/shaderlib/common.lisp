(in-package :fl.shaderlib)

(defun ensure-keyword (x)
  (etypecase x
    ((or number string symbol)
     (fl.util:make-keyword (format nil "~a" x)))))

(defun parts->string (parts &optional (filter #'identity))
  (with-output-to-string (s)
    (flet ((convert (parts)
             (mapcar
              (lambda (part)
                (etypecase part
                  ((or symbol string) (funcall filter part))
                  (integer part)))
              parts)))
      (loop :for (part . rest) :on (convert parts)
            :for separator = "" :then "."
            :do (etypecase part
                  ((or symbol string) (format s "~a~a" separator part))
                  (integer (format s "[~a]" part)))))))

(defgeneric get-qualifiers (type)
  (:method ((type varjo:v-type))
    (varjo.internals:qualifiers type))
  (:method ((type varjo.internals:shader-variable))
    (varjo:qualifiers (varjo.internals:v-type-of type))))

(defun has-qualifier-p (type qualifier)
  (member qualifier (get-qualifiers type) :test #'varjo.internals:qualifier=))

(defun lisp-symbol->glsl-type (symbol)
  (when (and (boundp symbol)
             (not (constantp symbol)))
    (etypecase (symbol-value symbol)
      (boolean :bool)
      (single-float :float)
      (double-float :double)
      ((signed-byte 32) :int)
      ((unsigned-byte 32) :uint)
      ((simple-array single-float (2)) :vec2)
      ((simple-array single-float (3)) :vec3)
      ((simple-array single-float (4)) :vec4)
      ((simple-array single-float (9)) :mat3)
      ((simple-array single-float (16)) :mat4))))

(defun lisp-constant->glsl-constant (constant)
  (when (constantp constant)
    (let ((value (symbol-value constant)))
      (typecase value
        (null 0)
        (boolean 1)
        (single-float value)
        (double-float value)
        ((signed-byte 32) value)
        ((unsigned-byte 32) value)))))
