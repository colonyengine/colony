(in-package :fl.core)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; base types initially available for matvars:
;; :integer
;; :float
;; :vec2, :vec3, :vec4
;; :string
;; :var
;; :shader
;; :texture

;; Held in core-state, the material database for all materials everywhere.
;; THere is a better API to this than just raw hash tables.
(defclass materials-table ()
  ((%material-table :reader material-table
                    :initarg :material-table
                    :initform (make-hash-table))))

(defun make-materials-table (&rest init-args)
  (apply #'make-instance 'materials-table init-args))



(defclass material-value ()
  ((%value :accessor value
           :initarg :value)
   ;; The function that knows how to bind this value to a shader.
   (%binder :reader binder
            :initarg :binder)))

(defun make-material-value (&rest init-args)
  (apply #'make-instance 'material-value init-args))


(defclass material ()
  ((%id :reader id
        :initarg :id)
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms
              ;; key is a uniform keyword, value is material-value
              :initform (make-hash-table))
   (%source-form :reader source-form
                 :initarg :source-form)))

(defun make-material (id shader source-form)
  (make-instance 'material :id id :shader shader :source-form source-form))

(defmethod bind-uniforms ((mat material))
  nil)

(defun mat-ref (mat var)
  nil)

(defun (setf mat-ref) (new-val mat var)
  nil)





(defun parse-material (shader name body)
  "Return a function which creates a partially complete material instance.
It is partially complete because it does not yet have the shader binder
function available for it so BIND-UNIFORMS cannot yet be called on it."
  `(lambda ()
     (let ((mat (make-material ,name ,shader ',body)))

       (setf
        ,@(loop :for (var val) :in body :appending
                `((gethash ,var (uniforms mat))
                  ;; we don't know the binder function we need yet...
                  (make-material-value :value ,val))))
       mat)))



;; TODO: Given a material instance and a shader-dict, find the shader-program
;; for this material, then ensure the uniforms/etc are kosher, and assign a
;; binder function into the material-value.
(defun annotate-material-binders (material shader-dict)
  nil)

;; TODO: After the partial materials and shaders have been loaded, we need to
;; resolve the materials to something we can actually bind to a real shader.
(defun resolve-all-materials (core-state)
  nil)




(defmethod extension-file-type ((extension-type (eql 'materials)))
  "materials")

(defmethod prepare-extension ((extension-type (eql 'materials)) owner path)
  (let ((%temp-materials (make-hash-table)))
    (declare (special %temp-materials))
    (flet ((%prepare ()
             (load-extensions extension-type path)
             %temp-materials))
      (maphash
       (lambda (material-name gen-material-func)

         (format t "prepare-extension(materials): processing ~A~%~S~%"
                 material-name gen-material-func)

         (setf (gethash material-name (materials owner))
               ;; Create the partially resolved material.... we will fully
               ;; resolve it later by type checking the uniforms specified
               ;; and creating the binder annotations for the values.
               (funcall gen-material-func))

         )

       (%prepare)))))

(defmacro define-material (name (&body options) &body body)
  `(let* ((material-func ,(parse-material (second (member :shader options))
                                          `',name
                                          body)))
     (declare (special %temp-materials))
     ,(when (second (member :enabled options))
        `(setf (gethash ',name %temp-materials) material-func))))





















(defun mat/doit ()

  (parse-material

   'pbr-material

   '(:enabled t
     :shader :pbr-texture)

   '(
     (:pbr-metallic-roughness.base-color.factor (vec4 .1 .2 .3 1))
     (:pbr-metallic-roughness.base-color.texture "file")
     (:pbr-metallic-roughness.metallic-factor .4)
     (:pbr-metallic-roughness.roughness-factor .7)
     (:pbr-metallic-roughness.metallic-roughness-texture.factor (vec4 0 0 0 0))
     (:pbr-metallic-roughness.metallic-roughness-texture.texture "file")

     (:normal.factor (vec4 1 1 1 1))
     (:normal.texture "file3")

     (:occlusion.factor (vec4 1 1 1 1))
     (:occlusion.texture "file4")

     (:emissive (vec3 0 0 0))
     (:alpha-mode 0)
     (:alpha-cutoff .3))

   )

  )
