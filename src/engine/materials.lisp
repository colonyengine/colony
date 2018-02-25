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

(defparameter %temp-material (make-hash-table))

(defclass material ()
  ((%id :reader id
        :initarg :id)
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms
              ;; key is a uniform keyword, value is suitable for uniform.
              :initform (make-hash-table))))

;; Held in core-state, the material database for all materials everywhere.
(defclass materials-table ()
  ((%material-table :reader material-table
                    :initarg :material-table
                    :initform (make-hash-table))))

(defun make-materials-table (&rest init-args)
  (apply #'make-instance 'materials-table init-args))

;; NOTE: This eval when is here just until this works, then I move the
;; define-materials into an extension for loading.
(defun parse-material (shader name body)
  (format t "parse-material: shader: ~A, name: ~A, body: ~A~%"
          shader name body)
  nil)


;; standins until I write the code
(defmacro define-material (name (&body options) &body body)
  `(let* ((material ,(parse-material (second (member :shader options))
                                     `',name
                                     body)))
     (declare (special %temp-material))
     ,(when (second (member :enabled options))
        `(setf (gethash ',name %temp-material) material))))





(defun mat/doit ()

  (parse-material
   'pbr-material
   '(:enabled t
     :shader :pbr-texture)

   '(
     (pbr-metallic-roughness
      (base-color
       (factor (vec4 .1 .2 .3 1))
       (texture "file"))
      (metallic-factor .4)
      (roughness-factor .7)
      (metallic-roughness-texture
       (factor (vec4 0 0 0 0))
       (texture "file3")))

     (normal
      (factor (vec4 1 1 1 1))
      (texture "file4"))

     (occlusion
      (factor (vec4 1 1 1 1))
      (texture "file6"))

     (emissive (vec3 0 0 0))

     (alpha-mode 0)
     (alpha-cutoff .3))
   ))


;; The materials DSL.

#|
;; This form happens in the user codes.
;; FirstLight/example/data/extensions/materials.matdict
(define-material unlit-color
    (:enabled t
     :shader :unlit-color)
  ;; This shader uses no uniforms.
  ;; The color is derived from the vertex attributes.
  )

(define-material unlit-texture
    (:enabled t
     :shader :unlit-texture)
  (tex
   (sampler1 "file")
   (sampler2 "file")))

;; And now, define a real named instance of a template.
(define-material pbr-material
    (:enabled t
     :shader :pbr-texture)

  (pbr-metallic-roughness
   (base-color
    (factor (vec4 .1 .2 .3 1))
    (texture "file"))
   (metallic-factor .4)
   (roughness-factor .7)
   (metallic-roughness-texture
    (factor (vec4 0 0 0 0))
    (texture "file3")))

  (normal
   (factor (vec4 1 1 1 1))
   (texture "file4"))

  (occlusion
   (factor (vec4 1 1 1 1))
   (texture "file6"))

  (emissive (vec3 0 0 0))

  (alpha-mode 0)
  (alpha-cutoff .3))


;; :pbr-metallic-roughness.base-color.factor -> (vec4 .1 .2 .3 1)
|#
