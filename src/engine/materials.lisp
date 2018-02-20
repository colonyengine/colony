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

(defclass material ()
  ((%id :reader id
        :initarg :id)
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms
              ;; key is a uniform keyword, value is suitable for uniform.
              :initform (make-hash-table))))

;; Held in core-state, the material database.
(defclass materials-table ()
  ((%material-table :reader material-table
                    :initarg :material-table
                    :initform (make-hash-table))))

(defun make-materials-table (&rest init-args)
  (apply #'make-instance 'materials-table init-args))


;; standins until I write the code
(defmacro define-material-dictionary (name (&body options) &body body)
  `(list ',name ',options ',body))

(defmacro define-material (name (&body options) &body init-forms)
  `(list ',name ',options ',init-forms))


;; The materials DSL.

;; This form happens in the user codes.
;; FirstLight/example/data/extensions/materials.matdict
(define-material-dictionary :example-project
    (:enabled t)

  (define-material :default-unlit-color-material
      (:shader :color)
    ;; This shader uses no uniforms.
    ;; The color is derived from the vertex attributes.
    )

  (define-material :default-unlit-texture-material
      (:shader :texture)
    (tex
     (sampler1 "file")
     (sampler2 "file")))

  ;; And now, define a real named instance of a template.
  (define-material :default-pbr-material
      (:shader :pbr-lit-shader)

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
  )


;; :pbr-metallic-roughness.base-color.factor -> (vec4 .1 .2 .3 1)
