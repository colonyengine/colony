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


(defclass matvar ()
  ((%name :reader name
          :initarg :name)
   (%slot-type :reader slot-type
               :initarg :slot-type)
   (%value :accessor value
           :initarg :value)
   (%default-thunk :reader default-thunk
                   :initarg :default-thunk)))

(defclass matvar-type ()
  ((%name :reader name
          :initarg :name)
   (%matvars :reader matvars
             :initarg :matvars
             :initform (make-hash-table))))

(defclass material-type ()
  ((%name :reader name
          :initarg :name)
   (%matvar-types :reader matvar-types
                  :initarg :matvar-types
                  :initform (make-hash-table))))

(defclass material-template ()
  ((%id :reader id
        :initarg :id)
   (%material-types :reader material-types
                    :initarg :material-types
                    :initform (make-hash-table))))

;; Held in core-state
(defclass materials-table ()
  ((%matvar-table :reader matvar-table
                  :initarg :matvar-table
                  :initform (make-hash-table))
   (%matvar-type-table :reader matvar-type-table
                       :initarg :matvar-type-table
                       :initform (make-hash-table))
   (%material-type-table :reader material-type-table
                         :initarg :material-type-table
                         :initform (make-hash-table))
   (%material-table :reader material-table
                    :initarg :material-table
                    :initform (make-hash-table))))



;; standins until I write the code
(defmacro with-material-accessors (accessors material &body body)
  `(list ',accessors ',material ',body))

(defmacro define-matvar-type (name &body slots)
  `(list ',name ',slots))

(defmacro define-material-type (name &body slots)
  `(list ',name ',slots))

(defmacro define-material (name (mat-type) &body init-forms)
  `(list ',name ',mat-type ',init-forms))





;; The materials DSL.

(define-matvar-type :textfact-scalar
  (factor :float 1)
  (texture :texture))

(define-matvar-type :textfact-vec3
  (factor :vec3 (vec3 1 1 1))
  (texture :texture))

(define-matvar-type :textfact-vec4
  (factor :vec4 (vec4 1 1 1 1))
  (texture :texture))

(define-matvar-type :pbrmr
  (base-color :textfact-vec4
              ;; set defaults for pbmrmr's use of this type.
              (factor (vec4 0 0 0 1)))
  (metallic-factor :float .4)
  (roughness-factor :float 0)
  (metallic-roughness-texture :texture)
  (alpha-mode :var :opaque)
  (alpha-cutoff :float .5))

;; This is the gltf2 material definition minus extras
(define-material-type :gltf2-material-type
  (display-name :string "Unknown Texture")
  (shader :shader)
  (pbr-metallic-roughness :pbrmr)
  (normal :textfact-vec4)
  (occlusion :textfact-vec4)
  (emissive :textfact-vec3)
  (alpha-mode :var)
  (alpha-cutoff :float))

;; And now, define a real named instance of a template.
(define-material :default-material (:gltf2-material-type)
  (display-name "Default Material")
  (shader :pbr-lit-shader)

  (pbr-metallic-roughness
   (base-color
    (factor (vec4 .1 .2 .3 1))
    (texture (location "file")))
   (metallic-factor
    (factor .4))
   (roughness-factor
    (factor .7))
   (metallic-roughness-texture
    (texture (location "file2"))))

  (normal
   (factor (vec4 1 1 1 1))
   (texture (location "file4")))

  (occlusion
   (factor (vec4 1 1 1 1))
   (texture (location "file6")))

  (emissive
   (factor 0 0 0))

  (alpha-mode :opaque)
  (alpha-cutoff
   (factor .3)))
