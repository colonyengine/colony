(in-package #:first-light.annotations)

;; `(fl.materials:noise-base fl.materials:perlin-noise :shader fl.gpu:?????)


;; I designed the system such that the direction the annotation goes is the
;; same for reading and writing.
(defun %material-annotator (mat-val component)
  (with-accessors ((context context)) component
    (cond
      ((symbolp mat-val)
       (lookup-material mat-val context))

      ((consp mat-val)
       (destructuring-bind (base-mat-sym new-mat-sym &rest initargs) mat-val
         (let* ((base-mat (lookup-material base-mat-sym context))
                (copy-mat (copy-material base-mat new-mat-sym))
                (shader (getf initargs :shader))
                (instances (getf initargs :instances))
                (uniforms (getf initargs :uniforms))
                (blocks (getf initargs :blocks)))

           (when blocks
             (error "Material override: :blocks not implemented yet."))

           ;; First, change to the new shader
           (when shader
             (setf (shader copy-mat) shader))

           (when instances
             (setf (instances copy-mat) instances))

           ;; Then process the initargs for the new shader.
           (when uniforms
             (unless (every (lambda (entry) (= (length entry) 2)) uniforms)
               (error "Material override: :uniforms entries must have a length of 2 ~A~%" uniforms))

             (loop :for (uniform-name value) :in uniforms
                   :do (setf (mat-uniform-ref copy-mat uniform-name) value)))

           ;; and return the newly minted material with all the overrides.
           copy-mat)))
      (t
       mat-val))))


(define-annotation material
  ;; Often these are the same, but there are common cases where they won't be.
  :getter %material-annotator
  :setter %material-annotator)
