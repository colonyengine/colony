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





(defclass material ()
  ((%id :reader id
        :initarg :id)
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms
              ;; key is a uniform keyword, value is suitable for uniform.
              :initform (make-hash-table))
   (%source-form :reader source-form
                 :initarg :source-form)))

(defun make-material (id shader source-form)
  (make-instance 'material :id id :shader shader :source-form source-form))


(defmethod bind-uniforms ((mat material))
  nil)







;; NOTE: This eval when is here just until this works, then I move the
;; define-materials into an extension for loading.
;; what should this return? A function that takes a core-state? Then I
;; can finish the construction at that time?
(defun parse-material (shader name body)
  (make-material name shader body))








(defmethod extension-file-type ((extension-type (eql 'materials)))
  "materials")

(defmethod prepare-extension ((extension-type (eql 'materials)) owner path)
  (let ((%temp-materials (make-hash-table)))
    (declare (special %temp-materials))
    (flet ((%prepare ()
             (load-extensions extension-type path)
             %temp-materials))
      (maphash
       (lambda (material-name material-instance)
         (declare (ignorable material-instance))

         ;; TODO: change this to use the materials-table interface.
         ;; Also, do the type checking of the materials, etc, etc.
         #++(setf (gethash material-name (materials owner)) material-instance)
         (format t "prepare-extension(materials): processing ~A~%"
                 material-name)

         )

       (%prepare)))))

(defmacro define-material (name (&body options) &body body)
  `(let* ((material ,(parse-material (second (member :shader options))
                                     `',name
                                     body)))
     (declare (special %temp-materials))
     ,(when (second (member :enabled options))
        `(setf (gethash ',name %temp-materials) material))))





















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
