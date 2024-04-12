(in-package #:colony.file.gltf)

;;;; Extensions are not yet supported in this model.

;;;; I didn't try and cram the parse and typechecking codes into
;;;; generic functions because the congruence or type selection doesn't
;;;; work out well.

;;;; Additional generic functions for the glTF data types
;;;; style can be :human or :json and transmits recursively
;;;; to the children of that thing
(defgeneric emit (style gltf-instance &key stream indent))

(defgeneric typecheck (pass gltf-instance &key &allow-other-keys))

;;;; Convenience functions for glTF data representation and outputting.
(defun is-aligned-p (value alignment)
  (zerop (mod value alignment)))

(defun component-type-value->component-type-symbol (component-type-value)
  (ecase component-type-value
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun component-type-symbol->component-type-value (component-type-symbol)
  (ecase component-type-symbol
    (:byte 5120)
    (:unsigned-byte 5121)
    (:short 5122)
    (:unsigned-short 5123)
    (:unsigned-int 5125)
    (:float 5126)))

(defun component-type-size (component-type-symbol)
  "Return the size in bytes for the COMPONENT-TYPE-SYMBOL."
  (ecase component-type-symbol
    (:byte 1)
    (:unsigned-byte 1)
    (:short 2)
    (:unsigned-short 2)
    (:unsigned-int 4)
    (:float 4)))

(defun attribute-type-count (attribute-type-symbol)
  (ecase attribute-type-symbol
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun attribute-type-value->attribute-type-symbol (attribute-type-value)
  (let* ((db `(("SCALAR" . :scalar)
               ("VEC2" . :vec2)
               ("VEC3" . :vec3)
               ("VEC4" . :vec4)
               ("MAT2" . :mat2)
               ("MAT3" . :mat3)
               ("MAT4" . :mat4))))
    (cdr (assoc attribute-type-value db :test #'string=))))

(defun attribute-type-symbol->attribute-type-value (attribute-type-symbol)
  (let* ((db `((:scalar . "SCALAR")
               (:vec2 . "VEC2")
               (:vec3 . "VEC3")
               (:vec4 . "VEC4")
               (:mat2 . "MAT2")
               (:mat3 . "MAT3")
               (:mat4 . "MAT4"))))
    (cdr (assoc attribute-type-symbol db :test #'eq))))

(defun target-path-value->target-path-symbol (target-path-value)
  (let* ((db `(("translation" . :translation)
               ("rotation" . :rotation)
               ("scale" . :scale)
               ("weights" . :weights))))
    (cdr (assoc target-path-value db :test #'string=))))

(defun target-path-symbol->target-path-value (target-path-symbol)
  (let* ((db `((:translation . "translation")
               (:rotation . "rotation")
               (:scale . "scale")
               (:weights . "weights"))))
    (cdr (assoc target-path-symbol db :test #'eq))))

(defun animsampler-interp-value->animsampler-interp-symbol
    (animsampler-interp-value)
  (let* ((db `(("LINEAR" . :linear)
               ("STEP" . :step)
               ("CUBICSPLINE" . :cubic-spline))))
    (cdr (assoc animsampler-interp-value db :test #'string=))))

(defun animsampler-interp-symbol->animsampler-interp-value
    (animsampler-interp-symbol)
  (let* ((db `((:linear . "LINEAR")
               (:step . "STEP")
               (:cubic-spline . "CUBICSPLINE"))))
    (cdr (assoc animsampler-interp-symbol db :test #'eq))))

(defun buffer-view-target-value->buffer-view-target-symbol
    (buffer-view-target-value)
  (ecase buffer-view-target-value
    (34962 :array-buffer)
    (34963 :element-array-buffer)))

(defun buffer-view-target-symbol->buffer-view-target-interp-value
    (buffer-view-target-symbol)
  (ecase buffer-view-target-symbol
    (:array-buffer 34962)
    (:element-array-buffer 34963)))

(defun camera-type-value->camera-type-symbol (camera-type-value)
  (let* ((db `(("perspective" . :perspective)
               ("orthographic" . :orthographic))))
    (cdr (assoc camera-type-value db :test #'string=))))

(defun camera-type-symbol->camera-type-value (camera-type-symbol)
  (let* ((db `((:perspective . "perspective")
               (:orthographic . "orthographic"))))
    (cdr (assoc camera-type-symbol db :test #'eq))))

(defun image-mime-type-value->image-mime-type-symbol (image-mime-type-value)
  (let* ((db `(("image/jpeg" . :image/jpeg)
               ("image/png" . :image/png))))
    (cdr (assoc image-mime-type-value db :test #'string=))))

(defun image-mime-type-symbol->image-mime-type-value (image-mime-type-symbol)
  (let* ((db `((:image/jpeg . "image/jpeg")
               (:image/png . "image/png"))))
    (cdr (assoc image-mime-type-symbol db :test #'eq))))

(defun alpha-mode-value->alpha-mode-symbol (alpha-mode-value)
  (let* ((db `(("OPAQUE" . :opaque)
               ("MASK" . :mask)
               ("BLEND" . :blend))))
    (cdr (assoc alpha-mode-value db :test #'string=))))

(defun alpha-mode-symbol->alpha-mode-value (alpha-mode-symbol)
  (let* ((db `((:opaque . "OPAQUE")
               (:mask . "MASK")
               (:blend . "BLEND"))))
    (cdr (assoc alpha-mode-symbol db :test #'eq))))

;; TODO: This might be a more useful utility function for general use
(defun attribute-value->attribute-name (str)
  (flet ((keywordify (s) (intern (string-upcase s) "KEYWORD")))
    (cl-ppcre:register-groups-bind
        ((#'keywordify name) (#'parse-integer index))
        ("^((?:(?:[A-Za-z]+)|(?:[_][A-Za-z]+))+)(?:[_]([0-9]+))?$" str)
      (if index
          (cons name index)
          name))))

;; TODO: This might be a more useful utility function for general use
(defun attribute-name->attribute-value (val)
  (etypecase val
    (symbol (symbol-name val))
    (cons (destructuring-bind (sym . index) val
            (concatenate 'string
                         (symbol-name sym) "_" (write-to-string index))))))

(defun primitive-attribute-value->primitive-attribute-name
    (primitive-attribute-value)
  "A PRIMITIVE-ATTRIBUTE-VALUE can be in one of a few formats, we describe the
allowable inputs below and what is returned.

  \"POSITION\"       -> :position
  \"TEXCOORD_0\"     -> (:texcoord . 0)
  \"FUNNY_THING\"    -> :funny_thing
  \"FUNNY_THING_0\"  -> (:funny_thing . 0)
  \"_TEMPERATURE\"   -> :_temperature
  \"_TEMPERATURE_0\" -> (:_temperature . 0)
  \"_FUNNY_THING\"   -> :_funny_thing
  \"_FUNNY_THING_0\" -> (:_funny_thing . 0)

  All of the below result in an ERROR being signaled.
  \"\"
  \"[_A-Za-z]*_\"
  \".*__.*\"
"
  (unless (stringp primitive-attribute-value)
    (error "primitive-attribute-value->primitive-attribute-name: PRIMITIVE-ATTRIBUTE-VALUE must be a string!"))

  (let* ((primitive-attribute-value (string-trim " " primitive-attribute-value))
         (pav-length (length primitive-attribute-value)))
    (unless (> pav-length 0)
      (error "primitive-attribute-value->primitive-attribute-name: PRIMITIVE-ATTRIBUTE-VALUE must be a string of greater than zero length"))

    (when (eql (aref primitive-attribute-value (1- pav-length)) #\_)
      (error "primitive-attribute-value->primitive-attribute-name: missing index number on priitive value: ~A. It should have integer characters at the end."
             primitive-attribute-value))

    (attribute-value->attribute-name primitive-attribute-value)))

(defun primitive-attribute-name->primitive-attribute-value
    (primitive-attribute-name)

  ;; TODO: Do some type checks.

  (attribute-name->attribute-value primitive-attribute-name))

(defun primitive-mode-value->primitive-mode-symbol (primitive-mode-value)
  (ecase primitive-mode-value
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)))

(defun primitive-mode-symbol->primitive-mode-value (primitive-mode-symbol)
  (ecase primitive-mode-symbol
    (:points 0)
    (:lines 1)
    (:line-loop 2)
    (:line-strip 3)
    (:triangles 4)
    (:triangle-strip 5)
    (:triangle-fan 6)))

(defun primitive-target-value-valid-p (primitive-target-value)
  (let ((default-values '("POSITION" "NORMAL" "TANGENT")))
    (and (stringp primitive-target-value)
         (or (some (lambda (s) (string= s primitive-target-value))
                   default-values)
             (and (> (length primitive-target-value) 0)
                  (eql (aref primitive-target-value 0) #\_))))))

(defun primitive-target-value->primitive-target-symbol (primitive-target-value)
  (unless (primitive-target-value-valid-p primitive-target-value)
    (error "primitive-target-value->primitive-target-symbol: The only valid values are NORMAL, POSITION, TANGENT, and any string of all capital letters starting with _. The value ~S is illegal: " primitive-target-value))

  (intern (string-upcase primitive-target-value) "KEYWORD"))

(defun primitive-target-symbol->primitive-target-value (primitive-target-symbol)
  (let ((primitive-target-value (symbol-name primitive-target-symbol)))
    (unless (primitive-target-value-valid-p primitive-target-value)
      (error "primitive-target-symbol->primitive-target-value: The only valid symbols are :NORMAL, :POSITION, :TANGENT, and any keyword symbol starting with _. The symbol ~S is illegal: " primitive-target-symbol))
    primitive-target-value))


(defun sampler-mag-filter-value->sampler-mag-filter-symbol (mag-filter-value)
  (ecase mag-filter-value
    (9728 :nearest)
    (9729 :linear)))

(defun sampler-mag-filter-symbol->sampler-mag-filter-value (mag-filter-symbol)
  (ecase mag-filter-symbol
    (:nearest 9728)
    (:linear 9729)))

(defun sampler-min-filter-value->sampler-min-filter-symbol (min-filter-value)
  (ecase min-filter-value
    (9728 :nearest)
    (9729 :linear)
    (9984 :nearest-mipmap-nearest)
    (9985 :linear-mipmap-nearest)
    (9986 :nearest-mipmap-linear)
    (9987 :linear-mipmap-linear)))

(defun sampler-min-filter-symbol->sampler-min-filter-value (min-filter-symbol)
  (ecase min-filter-symbol
    (:nearest 9728)
    (:linear 9729)
    (:nearest-mipmap-nearest 9984)
    (:linear-mipmap-nearest 9985)
    (:nearest-mipmap-linear 9986)
    (:linear-mipmap-linear 9987)))


(defun sampler-wrap-mode-value->sampler-wrap-mode-symbol (wrap-mode-value)
  (ecase wrap-mode-value
    (33071 :clamp-to-edge)
    (33648 :mirrored-repeat)
    (10497 :repeat)))

(defun sampler-wrap-mode-symbol->sampler-wrap-mode-value (wrap-mode-symbol)
  (ecase wrap-mode-symbol
    (:clamp-to-edge 33071)
    (:mirrored-repeat 33648)
    (:repeat 10497)))



;; Begin glTF data types.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass gltf-indices ()
  (;; An integer, cannot reference a ARRAY_BUFFER or ELEMENT_ARRAY_BUFFER target
   (%buffer-view :accessor buffer-view
                 :initarg :buffer-view)
   ;; An integer
   (%byte-offset :accessor byte-offset
                 :initarg :byte-offset
                 :initform 0)
   ;; An integer
   ;; One of:
   ;;
   ;; 5121 UNSIGNED_BYTE
   ;; 5123 UNSIGNED_SHORT
   ;; 5125 UNSIGNED_INT
   ;;
   ;; NOTE: we always store the symbol equivalent of the above values in this
   ;; slot.
   (%component-type :accessor component-type
                    :initarg :component-type)))

(defclass gltf-values ()
  (;; An integer, cannot reference a ARRAY_BUFFER or ELEMENT_ARRAY_BUFFER target
   (%buffer-view :accessor buffer-view
                 :initarg :buffer-view)
   (%byte-offset :accessor byte-offset
                 :initarg :byte-offset
                 :initform 0)))

(defclass gltf-sparse ()
  (;; An integer
   (%count :accessor sparse-count
           :initarg :sparse-count
           :initform 1)
   ;; An instance of gltf-indices (which point to a buffer of indices)
   (%indices :accessor indices
             :initarg :indices)
   ;; An instance of gltf-values (which point to a buffer of values)
   (%values :accessor sparse-values
            :initarg :sparse-values)))

(defclass gltf-accessor ()
  (;; An integer
   (%buffer-view :accessor buffer-view
                 :initarg :buffer-view)
   ;; An integer
   (%byte-offset :accessor byte-offset
                 :initarg :byte-offset
                 :initform 0)
   ;; One of:
   ;;
   ;; 5120 BYTE
   ;; 5121 UNSIGNED_BYTE
   ;; 5122 SHORT
   ;; 5123 UNSIGNED_SHORT
   ;; 5125 UNSIGNED_INT
   ;; 5126 FLOAT
   (%component-type :accessor component-type
                    :initarg :component-type)
   ;; a boolean
   (%normalized :accessor normalized
                :initarg :normalized
                :initform nil)
   ;; An integer
   (%count :accessor attribute-count
           :initarg :attribute-count
           :initform 1)
   ;; One of:
   ;;
   ;; "SCALAR"
   ;; "VEC2"
   ;; "VEC3"
   ;; "VEC4"
   ;; "MAT2"
   ;; "MAT3"
   ;; "MAT4"
   (%type :accessor attribute-type
          :initarg :attribute-type)
   ;; An instance of the correct type as denoted in ATTRIBUTE-TYPE
   (%max :accessor max-value
         :initarg :max-value)
   ;; An instance of the correct type as denoted in ATTRIBUTE-TYPE
   (%min :accessor min-value
         :initarg :min-value)
   ;; A gltf-sparse instance
   (%sparse :accessor sparse
            :initarg :sparse)
   ;; A string
   (%name :accessor name
          :initarg :name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animations
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-target ()
  (;; An integer
   (%node :accessor node
          :initarg :node)
   ;; A string
   (%path :accessor path
          :initarg :path)))

(defclass gltf-channel ()
  (;; An integer
   (%sampler :accessor sampler
             :initarg :sampler
             :initform 0)
   ;; A gltf-target instance
   (%target :accessor target
            :initarg :target)))

(defclass gltf-animation-sampler ()
  (;; An integer
   (%input :accessor input
           :initarg :input
           :initform 0)
   ;; A string
   (%interpolation :accessor interpolation
                   :initarg :interpolation
                   :initform "LINEAR")
   ;; An integer
   (%output :accessor output
            :initarg :output
            :initform 0)))

(defclass gltf-animation ()
  (;; An array of gltf-channel instances
   (%channels :accessor channels
              :initarg :channels)
   ;; An array of gltf-sampler instances
   (%samplers :accessor samplers
              :initarg :samplers)
   ;; A string
   (%name :accessor name
          :initarg :name)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asset (glTF identification)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-asset ()
  (;; a string
   (%copyright :accessor copyright
               :initarg :copyright)
   ;; a string
   (%generator :accessor generator
               :initarg :generator)
   ;; a string
   (%version :accessor version
             :initarg :version)
   ;; a string
   (%min-version :accessor min-version
                 :initarg :min-version)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-buffer ()
  (;; a string
   (%uri :accessor uri
         :initarg :uri)
   ;; an integer >= 1
   (%byte-length :accessor byte-length
                 :initarg :byte-length)
   ;; a string
   (%name :accessor name
          :initarg :name
          :initform "")))

(defclass gltf-buffer-view ()
  (;; an integer
   (%buffer :accessor buffer
            :initarg :buffer)
   ;; an integer
   (%byte-offset :accessor byte-offset
                 :initarg :byte-offset)
   ;; an integer
   (%byte-length :accessor byte-length
                 :initarg :byte-length)
   ;; an integer
   (%byte-stride :accessor byte-stride
                 :initarg :byte-stride)
   ;; an integer
   (%target :accessor target
            :initarg :target)
   ;; a string
   (%name :accessor name
          :initarg :name
          :initform "")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cameras
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-orthographic ()
  (;; a number
   (%x-mag :accessor x-mag
           :initarg :x-mag)
   ;; a number
   (%y-mag :accessor y-mag
           :initarg :y-mag)
   ;; a number
   (%z-far :accessor z-far
           :initarg :z-far)
   ;; a number
   (%z-near :accessor z-near
            :initarg :z-near)))

(defclass gltf-perspective ()
  (;; a number
   (%aspect-ratio :accessor aspect-ratio
                  :initarg :aspect-ratio)
   ;; a number
   (%y-fov :accessor y-fov
           :initarg :y-fov)
   ;; a number
   (%z-far :accessor z-far
           :initarg :z-far)
   ;; a number
   (%z-near :accessor z-near
            :initarg :z-near)))

(defclass gltf-camera ()
  (;; a gltf-orthographic instance OR null
   (%orthographic :accessor orthographic
                  :initarg :orthographic)
   ;; a gltf-perspective instance OR null
   (%perspective :accessor perspective
                 :initarg :perspective)
   ;; a string [changed from 'type' to 'camera-type']
   (%type :accessor camera-type
          :initarg :camera-type)
   ;; a string
   (%name :accessor name
          :initarg :name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root glTF object (TODO: probably move to bottom)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf ()
  (;; an array of strings
   (%extensions-used :accessor extensions-used
                     :initarg :extensions-used)
   ;; an array of strings
   (%extensions-required :accessor extensions-required
                         :initarg :extensions-required)
   ;; an array of gltf-accessor instances
   (%accessors :accessor accessors
               :initarg :accessors)
   ;; an arry of gltf-animation instances
   (%animations :accessor animations
                :initarg :animations)
   ;; a gltf-asset instance
   (%asset :accessor asset
           :initarg :asset)
   ;; an array of gltf-buffer instances
   (%buffers :accessor buffers
             :initarg :buffers)
   ;; an array of gltf-buffer-view instances
   (%buffer-views :accessor buffer-views
                  :initarg :buffer-views)
   ;; an array of gltf-camera instances
   (%cameras :accessor cameras
             :initarg :cameras)
   ;; an array of gltf-images instances
   (%images :accessor images
            :initarg :images)
   ;; an array of gltf-material instances
   (%materials :accessor materials
               :initarg :materials)
   ;; an array of gltf-mesh instances
   (%meshes :accessor meshes
            :initarg :meshes)
   ;; an array of gltf-node instances
   (%nodes :accessor nodes
           :initarg :nodes)
   ;; an array of gltf-sampler instances
   (%samplers :accessor sampleres
              :initarg :samplers)
   ;; an integer
   (%scene :accessor scene
           :initarg :scene)
   ;; an array of gltf-scene instances
   (%scenes :accessor scenes
            :initarg :scenes)
   ;; an array of gltf-skin instances
   (%skins :accessor skins
           :initarg :skins)
   ;; an array of gltf-texture instances
   (%textures :accessor textures
              :initarg :textures)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Images
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-image ()
  (;; a string
   (%uri :accessor uri
         :initarg :uri)
   ;; a string, one of
   ;; "image/jpeg"
   ;; "image/png"
   (%mime-type :accessor mime-type
               :initarg :mime-type)
   ;; an integer >= 0
   (%buffer-view :accessor buffer-view
                 :initarg :buffer-view)
   ;; a string
   (%name :accessor name
          :initarg :name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Materials
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-normal-texture-info ()
  (;; an integer >= 0
   (%index :accessor index
           :initarg :index)
   ;; an integer >= 0
   (%tex-coord :accessor tex-coord
               :initarg :tex-coord
               :initform 0)
   ;; a number
   (%scale :accessor scale
           :initarg :scale
           :initform 1f0)))

(defclass gltf-occlusion-texture-info ()
  (;; an integer >= 0
   (%index :accessor index
           :initarg :index)
   ;; an integer >= 0
   (%tex-coord :accessor tex-coord
               :initarg :tex-coord
               :initform 0)
   ;; a number >= 0 AND <= 1
   (%strength :accessor strength
              :initarg :strength
              :initform 1f0)))

(defclass gltf-pbr-metallic-roughness ()
  (;; an array of 4 numbers
   (%base-color-factor :accessor base-color-factor
                       :initarg :base-color-factor
                       :initform (vector 1f0 1f0 1f0 1f0))
   ;; a gltf-texture-info instance
   (%base-color-texture :accessor base-color-texture
                        :initarg :base-color-texture
                        :initform nil)
   ;; a number >= 0 AND <= 1
   (%metallic-factor :accessor metallic-factor
                     :initarg :metallic-factor
                     :initform 1f0)
   ;; a number >= 0 AND <= 1
   (%roughness-factor :accessor roughness-factor
                      :initarg :roughness-factor
                      :initform 1f0)
   ;; a gltf-texture-info instance
   (%metallic-roughness-texture :accessor metallic-roughness-texture
                                :initarg :metallic-roughness-texture
                                :initform nil)))

(defclass gltf-material ()
  (;; a string
   (%name :accessor name
          :initarg :name)
   ;; an instance of gltf-pbr-metallic-roughness or null
   (%pbr-metallic-roughness :accessor pbr-metallic-roughness
                            :initarg :pbr-metallic-roughness)
   ;; an instance of gltf-texture-info
   (%normal-texture :accessor normal-texture
                    :initarg :normal-texture)
   ;; an instance of gltf-texture-info
   (%occlusion-texture :accessor occlusion-texture
                       :initarg :occlusion-texture)
   ;; an instance of gltf-texture-info
   (%emissive-texture :accessor emissive-texture
                      :initarg :emissive-texture)
   ;; an array of 3 numbers
   (%emissive-factor :accessor emissive-factor
                     :initarg :emissive-factor
                     :initform (vector 1f0 1f0 1f0))
   ;; a string, one of:
   ;; "OPAQUE"
   ;; "MASK"
   ;; "BLEND"
   (%alpha-mode :accessor alpha-mode
                :initarg :alpha-mode
                :initform "OPAQUE")
   ;; a number >= 0 AND <= 1
   (%alpha-cutoff :accessor alpha-cutoff
                  :initarg :alpha-cutoff
                  :initform .5f0)
   ;; a boolean
   (%double-sided :accessor double-sided
                  :initarg :double-sided
                  :initform nil)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meshes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-primitive ()
  (;; a hash table:
   ;; key: (string) mesh attribute semantic name
   ;; value: (integer) index to accessor containing data of associated attribute
   (%attributes :accessor attributes
                :initarg :attributes
                :initform (u:dict #'equal))
   ;; an integer
   (%indices :accessor indices
             :initarg :indices)
   ;; an integer
   (%material :accessor material
              :initarg :material)
   ;; an integer, one of
   ;;
   ;; 0 POINTS
   ;; 1 LINES
   ;; 2 LINE_LOOP
   ;; 3 LINE_STRIP
   ;; 4 TRIANGLES
   ;; 5 TRIANGLE_STRIP
   ;; 6 TRIANGLE_FAN
   (%mode :accessor mode
          :initarg :mode
          :initform 4)
   ;; an array of hash tables:
   ;; the key is one of:
   ;; :position,
   ;; :normal,
   ;; :tangent"
   ;; The value is: (integer) an accessor index to the vertex displacement data
   (%targets :accessor targets
             :initarg :targets)))

(defclass gltf-mesh ()
  (;; an array of gltf-primitive instances
   (%primitives :accessor primitives
                :initarg :primitives)
   ;; an array of numbers
   (%weights :accessor weights
             :initarg :weights)
   ;; a string
   (%name :accessor name
          :initarg :name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nodes in the spatial hierarchy
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-node ()
  (;; an integer
   (%camera :accessor camera
            :initarg :camera)
   ;; an array of integers
   (%children :accessor children
              :initarg :children)
   ;; an integer
   (%skin :accessor skin
          :initarg :skin)
   ;; an array of 16 numbers, a 4x4 matrix stored in column order
   ;; NOTE: Only used if "rotation", or "translation", or or "scale" are
   ;; not default values.
   (%matrix :accessor matrix
            :initarg :matrix
            :initform (vector 1f0 0f0 0f0 0f0
                              0f0 1f0 0f0 0f0
                              0f0 0f0 1f0 0f0
                              0f0 0f0 0f0 1f0))
   ;; an integer
   (%mesh :accessor mesh
          :initarg :mesh)
   ;; an array of 4 numbers
   (%rotation :accessor rotation
              :initarg :rotation
              :initform (vector 0f0 0f0 0f0 1f0))
   ;; an array of 3 numbers
   (%scale :accessor scale
           :initarg :scale
           :initform (vector 1f0 1f0 1f0))
   ;; an array of 3 numbers
   (%translation :accessor translation
                 :initarg :translation
                 :initform (vector 0f0 0f0 0f0))
   ;; an array of number
   (%weights :accessor weights
             :initarg :weights)
   ;; a string
   (%name :accessor name
          :initarg :name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Samplers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-sampler ()
  (;; an integer, one of:
   ;; 9728 NEAREST
   ;; 9729 LINEAR
   (%mag-filter :accessor mag-filter
                :initarg :mag-filter)
   ;; an integer, one of:
   ;; 9728 NEAREST
   ;; 9729 LINEAR
   ;; 9984 NEAREST_MIPMAP_NEAREST
   ;; 9985 LINEAR_MIPMAP_NEAREST
   ;; 9986 NEAREST_MIPMAP_LINEAR
   ;; 9987 LINEAR_MIPMAP_LINEAR
   (%min-filter :accessor min-filter
                :initarg :min-filter)
   ;; an integer, one of:
   ;; 33071 CLAMP_TO_EDGE
   ;; 33648 MIRRORED_REPEAT
   ;; 10497 REPEAT
   (%wrap-s :accessor wrap-s
            :initarg :wrap-s
            :initform 10497)
   ;; an integer, one of:
   ;; 33071 CLAMP_TO_EDGE
   ;; 33648 MIRRORED_REPEAT
   ;; 10497 REPEAT
   (%wrap-t :accessor wrap-t
            :initarg :wrap-t
            :initform 10497)
   ;; a string
   (%name :accessor name
          :initarg :name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scenes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-scene ()
  (;; an array of integers
   (%nodes :accessor nodes
           :initarg :nodes)
   ;; a string
   (%name :accessor name
          :initarg :name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skin
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-skin ()
  (;; an integer
   (%inverse-bind-matricies :accessor inverse-bind-matricies
                            :initarg :inverse-bind-matricies)
   ;; an integer
   (%skeleton :accessor skeleton
              :initarg :skeleton)
   ;; an array of integers
   (%joints :accessor joints
            :initarg :joints)
   ;; a string
   (%name :accessor name
          :initarg :name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textures
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-texture ()
  (;; an integer
   (%sampler :accessor sampler
             :initarg :sampler)
   ;; an integer
   (%source :accessor source
            :initarg :source)
   ;; a string
   (%name :accessor name
          :initarg :name)))

(defclass gltf-texture-info ()
  (;; an integer
   (%index :accessor index
           :initarg :index)
   ;; an integer
   (%tex-coord :accessor tex-coord
               :initarg :tex-coord
               :initform 0)))

;; End glTF data types.

;; maker functions

(defun make-indices (&rest args)
  (apply #'make-instance 'gltf-indices args))

(defun make-values (&rest args)
  (apply #'make-instance 'gltf-values args))

(defun make-sparse (&rest args)
  (apply #'make-instance 'gltf-sparse args))

(defun make-accessor (&rest args)
  (apply #'make-instance 'gltf-accessor args))

(defun make-target (&rest args)
  (apply #'make-instance 'gltf-target args))

(defun make-channel (&rest args)
  (apply #'make-instance 'gltf-channel args))

(defun make-animation-sampler (&rest args)
  (apply #'make-instance 'gltf-animation-sampler args))

(defun make-animation (&rest args)
  (apply #'make-instance 'gltf-animation args))

(defun make-asset (&rest args)
  (apply #'make-instance 'gltf-asset args))

(defun make-buffer (&rest args)
  (apply #'make-instance 'gltf-buffer args))

(defun make-buffer-view (&rest args)
  (apply #'make-instance 'gltf-buffer-view args))

(defun make-orthographic (&rest args)
  (apply #'make-instance 'gltf-orthographic args))

(defun make-perspective (&rest args)
  (apply #'make-instance 'gltf-perspective args))

(defun make-camera (&rest args)
  (apply #'make-instance 'gltf-camera args))

(defun make-gltf (&rest args)
  (apply #'make-instance 'gltf args))

(defun make-image (&rest args)
  (apply #'make-instance 'gltf-image args))

(defun make-normal-texture-info (&rest args)
  (apply #'make-instance 'gltf-normal-texture-info args))

(defun make-occlusion-texture-info (&rest args)
  (apply #'make-instance 'gltf-occlusion-texture-info args))

(defun make-pbr-metallic-roughness (&rest args)
  (apply #'make-instance 'gltf-pbr-metallic-roughness args))

(defun make-material (&rest args)
  (apply #'make-instance 'gltf-material args))

(defun make-primitive (&rest args)
  (apply #'make-instance 'gltf-primitive args))

(defun make-mesh (&rest args)
  (apply #'make-instance 'gltf-mesh args))

(defun make-node (&rest args)
  (apply #'make-instance 'gltf-node args))

(defun make-sampler (&rest args)
  (apply #'make-instance 'gltf-sampler args))

(defun make-scene (&rest args)
  (apply #'make-instance 'gltf-scene args))

(defun make-skin (&rest args)
  (apply #'make-instance 'gltf-skin args))

(defun make-texture (&rest args)
  (apply #'make-instance 'gltf-texture args))

(defun make-texture-info (&rest args)
  (apply #'make-instance 'gltf-texture-info args))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple error handling.
;; ;;;;;;;;;;;;;;;;;;;;;;;;


(defun parse/error (gltf-type field &rest args)
  (let ((fmt (first args))
        (rest-args (rest args)))
    (error "glTF: Parse error [type: ~S json field: ~S]~A"
           gltf-type field
           (if fmt
               (apply #'format nil (concatenate 'string ": " fmt "~%")
                      rest-args)
               ": Field probably non-existent."))))

(defun parse/assert (value gltf-type field &rest args)
  (if value
      t
      (apply #'parse/error gltf-type field args)))


(defun typecheck/error (pass gltf-type fmt &rest args)
  (let ((rest-args (rest args)))
    (error "glTF: Type error [pass: ~A type: ~S]~A"
           pass gltf-type
           (apply #'format nil (concatenate 'string ": " fmt "~%")
                  rest-args))))

(defun typecheck/assert (pass gltf-obj value fmt &rest args)
  (if value
      t
      (apply #'typecheck/error pass (class-name (class-of gltf-obj)) fmt args)))

(defmacro typecheck/assert-group (pass gltf-obj &body forms)
  (a:with-gensyms (obj)
    (let ((body
            (loop :for (val fmt . args) :in forms
                  :collect `(typecheck/assert ,pass ,obj ,val ,fmt ,@args))))
      `(let ((,obj ,gltf-obj))
         ,@body))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing code to convert json objects to clos objects.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse extensions (not fully supported yet)
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-extensions-used (extensions-used)
  (when extensions-used
    (coerce extensions-used 'vector)))

(defun parse-extensions-required (extensions-required)
  (when extensions-required
    (coerce extensions-required 'vector)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; gltf-indices
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-indices (jobj)
  (u:mvlet ((buffer-view bv-p (jsown:val-safe jobj "bufferView"))
            (byte-offset bo-p (jsown:val-safe jobj "byteOffset"))
            (component-type ct-p (jsown:val-safe jobj "componentType")))

    (parse/assert bv-p 'gltf-indices "bufferView")
    (parse/assert ct-p 'gltf-indices "componentType")

    (make-indices
     :buffer-view buffer-view
     :byte-offset (if bo-p byte-offset 0)
     :component-type
     (component-type-value->component-type-symbol component-type))))

(defmethod typecheck ((pass (eql :static)) (obj gltf-indices) &key buffer-views)
  (with-accessors ((buffer-view buffer-view) (byte-offset byte-offset)
                   (component-type component-type))
      obj

    ;; TODO: check byte-offset alignment

    (symbol-macrolet ((ref-buffer-view (aref buffer-views buffer-view)))
      (typecheck/assert-group pass obj
        ((integerp buffer-view)
         "slot BUFFER-VIEW must be an integer!")
        ((>= buffer-view 0)
         "slot BUFFER-VIEW must be >= 0!")
        ((null (target ref-buffer-view))
         "slot BUFFER-VIEW references real buffer-view object with illegal TARGET value")
        ((integerp byte-offset)
         "slot BYTE-OFFSET must be an integer!")
        ((>= byte-offset 0)
         "slot BYTE-OFFSET must be >= 0!")
        ((member component-type '(:unsigned-byte :unsigned-short :unsigned-int))
         "slot COMPONENT-TYPE is [~(~S~)] but must be one of: :unsigned-byte :unsigned-short or :unsigned-int!" component-type))))

  t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; gltf-values
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-values (jobj)
  (u:mvlet ((buffer-view bv-p (jsown:val-safe jobj "bufferView"))
            (byte-offset bo-p (jsown:val-safe jobj "byteOffset")))

    (parse/assert bv-p 'gltf-values "bufferView")

    (make-values
     :buffer-view buffer-view
     :byte-offset (if bo-p byte-offset 0))))

(defmethod typecheck ((pass (eql :static)) (obj gltf-values) &key buffer-views)
  (with-accessors ((buffer-view buffer-view) (byte-offset byte-offset))
      obj

    ;; TODO: check byte-offset alignment

    (symbol-macrolet ((ref-buffer-view (aref buffer-views buffer-view)))
      (typecheck/assert-group pass obj
        ((integerp buffer-view)
         "slot BUFFER-VIEW must be an integer!")
        ((>= buffer-view 0)
         "slot BUFFER-VIEW must be >= 0!")
        ((null (target ref-buffer-view))
         "slot BUFFER-VIEW references real buffer-view object with illegal TARGET value")
        ((integerp byte-offset)
         "slot BYTE-OFFSET must be an integer!")
        ((>= byte-offset 0)
         "slot BYTE-OFFSET must be >= 0!"))))

  t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; gltf-sparse
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-sparse (jobj)
  (u:mvlet ((sparse-count sc-p (jsown:val-safe jobj "count"))
            (jobj-indices ind-p (jsown:val-safe jobj "indices"))
            (jobj-values val-p (jsown:val-safe jobj "values")))

    (parse/assert sc-p 'gltf-sparse "count")
    (parse/assert ind-p 'gltf-sparse "indices")
    (parse/assert val-p 'gltf-sparse "values")

    (make-sparse
     :sparse-count sparse-count
     :indices (parse-indices jobj-indices)
     :sparse-values (parse-values jobj-values))))

(defmethod typecheck ((pass (eql :static))
                      (obj gltf-sparse) &key accessor buffer-views)
  (declare (ignore accessor)) ;; TODO: Not implemented yet.
  (with-accessors ((sparse-count sparse-count)
                   (indices indices)
                   (sparse-values sparse-values))
      obj

    ;; TODO: requires checking of actual data to be complete.

    (typecheck/assert-group pass obj
      ((integerp sparse-count)
       "slot SPARSE-COUNT must be an integer!")
      ((>= sparse-count 1)
       "slot SPARSE-COUNT must be >= 1!"))

    (typecheck pass indices :buffer-views buffer-views)

    (typecheck pass sparse-values :buffer-views buffer-views)

    t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; gltf-accessor
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-accessor (jobj)
  (u:mvlet ((buffer-view (jsown:val-safe jobj "bufferView"))
            (byte-offset bo-p (jsown:val-safe jobj "byteOffset"))
            (component-type ct-p (jsown:val-safe jobj "componentType"))
            (normalized-p (jsown:val-safe jobj "normalized"))
            (attribute-count ac-p (jsown:val-safe jobj "count"))
            (attribute-type at-p (jsown:val-safe jobj "type"))
            (max-value max-p (jsown:val-safe jobj "max"))
            (min-value min-p (jsown:val-safe jobj "min"))
            (jobj-sparse (jsown:val-safe jobj "sparse"))
            (name (jsown:val-safe jobj "name")))

    (parse/assert ct-p 'gltf-accessor "componentType")
    (parse/assert ac-p 'gltf-accessor "count")
    (parse/assert at-p 'gltf-accessor "type")

    (make-accessor
     :buffer-view buffer-view
     :byte-offset (if bo-p byte-offset 0)
     :component-type
     (component-type-value->component-type-symbol component-type)
     :normalized normalized-p
     :attribute-count attribute-count
     :attribute-type
     (attribute-type-value->attribute-type-symbol attribute-type)
     :max-value (when max-p (coerce max-value 'vector))
     :min-value (when min-p (coerce min-value 'vector))
     :sparse (when jobj-sparse (parse-sparse jobj-sparse))
     :name name)))

(defmethod typecheck ((pass (eql :static))
                      (obj gltf-accessor) &key buffer-views)
  (with-accessors ((buffer-view buffer-view)
                   (byte-offset byte-offset)
                   (component-type component-type)
                   (normalized normalized)
                   (attribute-count attribute-count)
                   (attribute-type attribute-type)
                   (max-value max-value)
                   (min-value min-value)
                   (sparse sparse)
                   (name name))
      obj

    (unless (null buffer-view)
      (typecheck/assert-group pass obj
        ((integerp buffer-view) "slot BUFFER-VIEW is not an integer!")
        ((>= buffer-view 0) "slot BUFFER-VIEW must be >= 0!")))

    (unless (null byte-offset)
      (typecheck/assert-group pass obj
        ((integerp byte-offset) "slot BYTE-OFFSET is not an integer!")
        ((>= byte-offset 0) "slot BYTE-OFFSET must be >= 0!")
        ((is-aligned-p byte-offset (component-type-size component-type))
         "slot BYTE-OFFSET is not aligned according to component type: ~(~S~)~"
         component-type)))

    (let ((valid-domain '(:byte :unsigned-byte :short :unsigned-short
                          :unsigned-int :float)))
      (typecheck/assert-group pass obj
        ((symbolp component-type) "slot COMPONENT-TYPE is not a symbol!")
        ((member component-type valid-domain)
         "slot COMPONENT-TYPE [~(~S~)] is not a member of its valid domain: ~(~S~)!"
         component-type valid-domain)))

    ;; TODO: Must be present only when accessor contains vertex attributes, or
    ;; animation output data.
    (when normalized
      (typecheck/assert-group pass obj
        ((member normalized '(t nil))
         "slot NORMALIZED must be either T or NIL!")))

    (typecheck/assert-group pass obj
      ((integerp attribute-count)
       "slot ATTRIBUTE-COUNT is not an integer!")
      ((>= attribute-count 1)
       "slot ATTRIBUTE-COUNT must be >= 1!"))

    (let ((valid-domain '(:scalar :vec2 :vec3 :vec4 :mat2 :mat3 :mat4)))
      (typecheck/assert-group pass obj
        ((symbolp attribute-type) "slot ATTRIBUTE-TYPE is not a symbol!")
        ((member attribute-type valid-domain)
         "slot ATTRIBUTE-TYPE [~(~S~)] is not a member of its valid domain: ~(~S~)!"
         attribute-type valid-domain)))

    ;; TODO: Must check actual data to confirm it is valid--but lazy i/o.
    ;; TODO: Need to check with sparse substitution applied.
    (when max-value
      (let ((valid-lengths '(1 2 3 4 9 16)))
        (typecheck/assert-group pass obj
          ((vectorp max-value)
           "slot MAX-VALUE must be a vector!")
          ((member (length max-value) valid-lengths :test #'=)
           "slot MAX-VALUE is of length ~A, but needs to be one of length: ~A!"
           (length max-value) valid-lengths)
          ((and min-value (vectorp min-value)
                (= (length max-value) (length min-value)))
           "slot MAX-VALUE must have the same length as slow MIN-VALUE!"))))

    ;; TODO: Must check actual data to confirm it is valid--but lazy i/o.
    ;; TODO: Need to check with sparse substitution applied.
    (when min-value
      (let ((valid-lengths '(1 2 3 4 9 16)))
        (typecheck/assert-group obj
          ((vectorp min-value)
           "slot MIN-VALUE must be a vector!")
          ((member (length min-value) valid-lengths :test #'=)
           "slot MIN-VALUE is of length ~A, but needs to be one of length: ~A!"
           (length min-value) valid-lengths)
          ((and max-value (vectorp max-value)
                (= (length min-value) (length max-value)))
           "slot MIN-VALUE must have the same length as slow MAX-VALUE!"))))

    (when sparse
      (typecheck pass sparse :accessor obj :buffer-views buffer-views))

    (when name
      (typecheck/assert-group pass obj
        ((stringp name)
         "slot NAME must be a string!")))

    t))


(defun parse-accessors (accessors)
  (when accessors
    (map 'vector #'parse-accessor accessors)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-target
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-target (jobj)
  (u:mvlet ((node (jsown:val-safe jobj "node"))
            (path path-p (jsown:val-safe jobj "path")))

    (parse/assert path-p 'gltf-target "path")

    (make-target :node node
                 :path
                 (target-path-value->target-path-symbol path))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-channel
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-channel (jobj)
  (u:mvlet ((sampler sam-p (jsown:val-safe jobj "sampler"))
            (jobj-target ta-p (jsown:val-safe jobj "target")))

    (parse/assert sam-p 'gltf-channel "sampler")
    (parse/assert ta-p 'gltf-channel "target")

    (make-channel :sampler sampler
                  :target (parse-target jobj-target))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-animation-sampler
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-animation-sampler (jobj)
  (u:mvlet ((input in-p (jsown:val-safe jobj "input"))
            (interp interp-p (jsown:val-safe jobj "interpolation"))
            (output out-p (jsown:val-safe jobj "output")))

    (parse/assert in-p 'gltf-animation-sampler "input")
    (parse/assert out-p 'gltf-animation-sampler "output")

    (make-animation-sampler
     :input input
     :interpolation
     (if interp
         (animsampler-interp-value->animsampler-interp-symbol interp)
         :linear)
     :output output)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-animation
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-animation (jobj)
  (u:mvlet ((channels ch-p (jsown:val-safe jobj "channels"))
            (samplers sa-p (jsown:val-safe jobj "samplers"))
            (name (jsown:val-safe jobj "output")))

    (parse/assert ch-p 'gltf-animation "channels")
    (parse/assert sa-p 'gltf-animation "samplers")

    (make-animation
     :channels (map 'vector #'parse-channel channels)
     :samplers (map 'vector #'parse-animation-sampler samplers)
     :name name)))

(defun parse-animations (animations)
  (when animations
    (map 'vector #'parse-animation animations)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-asset
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-asset (jobj)
  (u:mvlet ((copyright (jsown:val-safe jobj "copyright"))
            (generator (jsown:val-safe jobj "generator"))
            (version vers-p (jsown:val-safe jobj "version"))
            (min-version (jsown:val-safe jobj "minVersion")))

    (parse/assert vers-p 'gltf-asset "values")

    (make-asset
     :copyright copyright
     :generator generator
     :version version
     :min-version min-version)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-buffer
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-buffer (jobj)
  (u:mvlet ((uri (jsown:val-safe jobj "uri"))
            (byte-length bl-p(jsown:val-safe jobj "byteLength"))
            (name (jsown:val-safe jobj "name")))

    (parse/assert bl-p 'gltf-buffer "byteLength")

    (make-buffer
     ;; TODO: Don't process the uri, we'll do that later when we want to
     ;; realize the buffer's actual contents into memory.
     :uri uri
     :byte-length byte-length
     :name name)))

(defun parse-buffers (buffers)
  (when buffers
    (map 'vector #'parse-buffer buffers)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-buffer;view
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-buffer-view (jobj)
  (u:mvlet ((buffer buf-p (jsown:val-safe jobj "buffer"))
            (byte-offset bo-p (jsown:val-safe jobj "byteOffset"))
            (byte-length bl-p (jsown:val-safe jobj "byteLength"))
            (byte-stride (jsown:val-safe jobj "byteStride"))
            (target target-p (jsown:val-safe jobj "target"))
            (name (jsown:val-safe jobj "name")))

    (parse/assert buf-p 'gltf-buffer-view "buffer")
    (parse/assert bl-p 'gltf-buffer-view "byteLength")

    (make-buffer-view
     :buffer buffer
     :byte-offset (if bo-p byte-offset 0)
     :byte-length byte-length
     :byte-stride byte-stride
     :target (when target-p
               (buffer-view-target-value->buffer-view-target-symbol target))
     :name name)))

(defun parse-buffer-views (buffer-views)
  (when buffer-views
    (map 'vector #'parse-buffer-view buffer-views)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-orthographic
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-orthographic (jobj)
  (u:mvlet ((x-mag x-mag-p (jsown:val-safe jobj "xmag"))
            (y-mag y-mag-p (jsown:val-safe jobj "ymag"))
            (z-far z-far-p (jsown:val-safe jobj "zfar"))
            (z-near z-near-p (jsown:val-safe jobj "znear")))

    (parse/assert x-mag-p 'gltf-orthographic "xmag")
    (parse/assert y-mag-p 'gltf-orthographic "ymag")
    (parse/assert z-far-p 'gltf-orthographic "zfar")
    (parse/assert z-near-p 'gltf-orthographic "znear")

    (make-orthographic
     :x-mag x-mag
     :y-mag y-mag
     :z-far z-far
     :z-near z-near)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-perspective
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-perspective (jobj)
  (u:mvlet ((aspect-ratio (jsown:val-safe jobj "aspectRatio"))
            (y-fov y-fov-p (jsown:val-safe jobj "yfov"))
            (z-far (jsown:val-safe jobj "zfar"))
            (z-near z-near-p (jsown:val-safe jobj "znear")))

    (parse/assert y-fov-p 'gltf-perspective "yfov")
    (parse/assert z-near-p 'gltf-perspective "znear")

    (make-perspective
     :aspect-ratio aspect-ratio
     :y-fov y-fov
     :z-far z-far
     :z-near z-near)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-camera
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-camera (jobj)
  (u:mvlet ((jobj-orthographic ortho-p (jsown:val-safe jobj "orthographic"))
            (jobj-perspective persp-p (jsown:val-safe jobj "perspective"))
            (camera-type ct-p (jsown:val-safe jobj "type"))
            (name (jsown:val-safe jobj "name")))

    (parse/assert ct-p 'gltf-camera "type")
    ;; if either ortho-p or persp-p is defined, then only one must be.
    ;; ortho or persp must be defined, but not both.
    (when (or ortho-p persp-p)
      (parse/assert (not (and ortho-p persp-p))
                    'gltf-camera "orthographic | perspective"
                    "Only one of these must be defined if any are."))

    (make-camera
     :camera-type (camera-type-value->camera-type-symbol camera-type)
     :orthographic (when ortho-p (parse-orthographic jobj-orthographic))
     :perspective (when persp-p (parse-perspective jobj-perspective))
     :name name)))

(defun parse-cameras (cameras)
  (when cameras
    (map 'vector #'parse-camera cameras)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-image
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-image (jobj)
  (u:mvlet ((uri (jsown:val-safe jobj "uri"))
            (mime-type (jsown:val-safe jobj "mimeType"))
            (buffer-view (jsown:val-safe jobj "buffer-view"))
            (name (jsown:val-safe jobj "name")))

    (make-image
     ;; TODO: We handle these URI's later for loading or converting them
     ;; the byte arrays.
     :uri uri
     :mime-type (image-mime-type-value->image-mime-type-symbol mime-type)
     :buffer-view buffer-view
     :name name)))

(defun parse-images (images)
  (when images
    (map 'vector #'parse-image images)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-normal-texture-info
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-normal-texture-info (jobj)
  (u:mvlet ((index ind-p (jsown:val-safe jobj "index"))
            (tex-coord tc-p (jsown:val-safe jobj "texCoord"))
            (scale sc-p (jsown:val-safe jobj "scale")))

    (parse/assert ind-p 'gltf-normal-texture-info "index")

    (make-normal-texture-info
     :index index
     :tex-coord (if tc-p tex-coord 0)
     :scale (if sc-p scale 1f0))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-occlusion-texture-info
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-occlusion-texture-info (jobj)
  (u:mvlet ((index ind-p (jsown:val-safe jobj "index"))
            (tex-coord tc-p (jsown:val-safe jobj "texCoord"))
            (strength st-p (jsown:val-safe jobj "strength")))

    (parse/assert ind-p 'gltf-occlusion-texture-info "index")

    (make-occlusion-texture-info
     :index index
     :tex-coord (if tc-p tex-coord 0)
     :strength (if st-p strength 1f0))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-texture-info
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-texture-info (jobj)
  (u:mvlet ((index ind-p (jsown:val-safe jobj "index"))
            (tex-coord tc-p (jsown:val-safe jobj "texCoord")))

    (parse/assert ind-p 'gltf-texture-info "index")

    (make-texture-info
     :index index
     :tex-coord (if tc-p tex-coord 0))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-pbr-metallic-roughness
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-pbr-metallic-roughness (jobj)
  (u:mvlet ((base-color-factor bcf-p (jsown:val-safe jobj "baseColorFactor"))
            (jobj-base-color-texture bct-p
                                     (jsown:val-safe jobj "baseColorTexture"))
            (metallic-factor mf-p (jsown:val-safe jobj "metallicFactor"))
            (roughness-factor rf-p (jsown:val-safe jobj "roughnessFactor"))
            (jobj-metallic-roughness-texture mrt-p
                                             (jsown:val-safe
                                              jobj
                                              "metallicRoughnessTexture")))

    (make-pbr-metallic-roughness
     :base-color-factor (if bcf-p
                            (coerce base-color-factor 'vector)
                            (vector 1 1 1 1))
     :base-color-texture (when bct-p
                           (parse-texture-info jobj-base-color-texture))
     :metallic-factor (if mf-p metallic-factor 1f0)
     :roughness-factor (if rf-p roughness-factor 1f0)
     :metallic-roughness-texture
     (if mrt-p (parse-texture-info jobj-metallic-roughness-texture)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-material
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-material (jobj)
  (u:mvlet ((name (jsown:val-safe jobj "name"))
            (jobj-pbr-metallic-roughness pbrmr-p
                                         (jsown:val-safe
                                          jobj
                                          "pbrMetallicRoughness"))
            (jobj-normal-texture nt-p
                                 (jsown:val-safe jobj "normalTexture"))
            (jobj-occlusion-texture ot-p
                                    (jsown:val-safe jobj "occlusionTexture"))
            (jobj-emissive-texture et-p
                                   (jsown:val-safe jobj "emissiveTexture"))
            (emissive-factor ef-p (jsown:val-safe jobj "emissiveFactor"))
            (alpha-mode am-p (jsown:val-safe jobj "alphaMode"))
            (alpha-cutoff ac-p (jsown:val-safe jobj "alphaCutoff"))
            (double-sided (jsown:val-safe jobj "doubleSided")))

    (make-material
     :name name
     :pbr-metallic-roughness
     (if pbrmr-p
         (parse-pbr-metallic-roughness jobj-pbr-metallic-roughness)
         (make-pbr-metallic-roughness))
     :normal-texture
     (when nt-p (parse-normal-texture-info jobj-normal-texture))
     :occlusion-texture
     (when ot-p (parse-occlusion-texture-info jobj-occlusion-texture))
     :emissive-texture
     (when et-p (parse-texture-info jobj-emissive-texture))
     :emissive-factor (if ef-p
                          (coerce emissive-factor 'vector)
                          (vector 0f0 0f0 0f0))
     :alpha-mode (if am-p
                     (alpha-mode-value->alpha-mode-symbol alpha-mode)
                     :opaque)
     :alpha-cutoff (if ac-p alpha-cutoff .5f0)
     :double-sided double-sided)))

(defun parse-materials (materials)
  (when materials
    (map 'vector #'parse-material materials)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-primitive
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-primitive (jobj)
  (u:mvlet ((jobj-attributes (jsown:val-safe jobj "attributes"))
            (indices (jsown:val-safe jobj "indices"))
            (material (jsown:val-safe jobj "material"))
            (mode mode-p (jsown:val-safe jobj "mode"))
            (jobj-targets (jsown:val-safe jobj "targets")))

    (parse/assert jobj-attributes 'gltf-primitive "attributes")

    (let ((primitive
            (make-primitive
             :indices indices
             :material material
             :mode (if mode-p
                       (primitive-mode-value->primitive-mode-symbol mode)
                       :triangles))))

      ;; Parse the attribute hash
      (loop :for (%attr-name . index) :in (rest jobj-attributes)
            :do (let ((attr-name
                        (primitive-attribute-value->primitive-attribute-name
                         %attr-name)))

                  (setf (u:href (attributes primitive) attr-name) index)))

      ;; Parse the morph target array of attribute hash tables.
      (when jobj-targets
        (flet ((morph-target->hash-table (morph-target)
                 (loop
                   :with db = (u:dict #'equal)
                   :for (%attr-name . index) :in (rest morph-target)
                   :do (let ((attr-name
                               (primitive-attribute-value->primitive-attribute-name
                                %attr-name)))
                         (parse/assert
                          (member attr-name '(:position :normal :tangent))
                          'gltf-primitive 'targets
                          "Bad attribute value for a primitive morph target: ~S"
                          attr-name)
                         (setf (u:href db attr-name) index))
                   :finally (return db))))

          (setf (targets primitive) (make-array (length jobj-targets)))
          (loop :for morph-target :in jobj-targets
                :for idx :below (length (targets primitive))
                :do (setf (aref (targets primitive) idx)
                          (morph-target->hash-table morph-target)))))

      primitive)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-mesh
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-mesh (jobj)
  (u:mvlet ((jobj-primitives (jsown:val-safe jobj "primitives"))
            (jobj-weights (jsown:val-safe jobj "weights"))
            (name (jsown:val-safe jobj "name")))

    (parse/assert jobj-primitives 'gltf-mesh "primitives")

    (make-mesh
     :primitives (map 'vector #'parse-primitive jobj-primitives)
     :weights (when jobj-weights
                (map 'vector (lambda (v) (float v 1f0)) jobj-weights))
     :name name)))

(defun parse-meshes (meshes)
  (when meshes
    (map 'vector #'parse-mesh meshes)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-node
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-node (jobj)
  (u:mvlet ((camera (jsown:val-safe jobj "camera"))
            (children children-p (jsown:val-safe jobj "children"))
            (skin (jsown:val-safe jobj "skin"))
            (matrix matrix-p (jsown:val-safe jobj "matrix"))
            (mesh (jsown:val-safe jobj "mesh"))
            (rotation rotation-p (jsown:val-safe jobj "rotation"))
            (scale scale-p (jsown:val-safe jobj "scale"))
            (translation translation-p (jsown:val-safe jobj "translation"))
            (weights weights-p (jsown:val-safe jobj "weights"))
            (name (jsown:val-safe jobj "name")))

    (parse/assert
     (or (and matrix-p (not (or rotation-p scale-p translation-p)))
         (and (or rotation-p scale-p translation-p) (not matrix-p))
         (not (or matrix-p rotation-p scale-p translation-p)))
     'gltf-node
     "<matrix | rotation | scale | translation>"
     "Failed constraints of EITHER matrix-p or TRS.")

    ;; TODO: gltf's node either defines matrix OR (rotation scale translation)
    ;; [and matrix cannot be defined when this node is the target of an
    ;; animation].  However, defaults are assigned for everything if it isn't
    ;; there (according to the docs), so, how do I know what was actually
    ;; defined?  Probably add a new slot in here to let the user know which set
    ;; was actually defined.

    (make-node
     :camera camera
     :children (when children-p (coerce children 'vector))
     :skin skin
     :matrix (if matrix-p
                 (coerce matrix 'vector)
                 ;; column-major storage.
                 ;; NOTE: But, not specfied as being column or row vectors.
                 ;; I think I can assume column vectors, however.
                 (vector 1f0 0f0 0f0 0f0
                         0f0 1f0 0f0 0f0
                         0f0 0f0 1f0 0f0
                         0f0 0f0 0f0 1f0))
     :mesh mesh
     :rotation (if rotation-p
                   (coerce rotation 'vector)
                   ;; <x, y, z, w> quaternion
                   (vector 0f0 0f0 0f0 1f0))

     :scale (if scale-p
                (coerce scale 'vector)
                (vector 1f0 1f0 1f0))
     :translation (if translation-p
                      (coerce translation 'vector)
                      (vector 0f0 0f0 0f0))
     :weights (when weights-p (coerce weights 'vector))
     :name name
     )))

(defun parse-nodes (nodes)
  (when nodes
    (map 'vector #'parse-node nodes)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-scene
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-scene (jobj)
  (u:mvlet ((nodes nodes-p (jsown:val-safe jobj "nodes"))
            (name (jsown:val-safe jobj "name")))

    (make-scene
     :nodes (when nodes-p (coerce nodes 'vector))
     :name name)))

(defun parse-scenes (scenes)
  (when scenes
    (map 'vector #'parse-scene scenes)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-sampler
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-sampler (jobj)
  (u:mvlet ((mag-filter (jsown:val-safe jobj "magFilter"))
            (min-filter (jsown:val-safe jobj "minFilter"))
            (wrap-s wrap-s-p (jsown:val-safe jobj "wrapS"))
            (wrap-t wrap-t-p (jsown:val-safe jobj "wrapT"))
            (name (jsown:val-safe jobj "name")))

    (make-sampler
     :mag-filter
     (when mag-filter
       (sampler-mag-filter-value->sampler-mag-filter-symbol mag-filter))
     :min-filter
     (when min-filter
       (sampler-min-filter-value->sampler-min-filter-symbol min-filter))
     :wrap-s
     (if wrap-s-p
         (sampler-wrap-mode-value->sampler-wrap-mode-symbol wrap-s)
         :repeat)
     :wrap-t
     (if wrap-t-p
         (sampler-wrap-mode-value->sampler-wrap-mode-symbol wrap-t)
         :repeat)
     :name name)))

(defun parse-samplers (samplers)
  (when samplers
    (map 'vector #'parse-sampler samplers)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-texture
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-texture (jobj)
  (u:mvlet ((sampler (jsown:val-safe jobj "sampler"))
            (source (jsown:val-safe jobj "source"))
            (name (jsown:val-safe jobj "name")))

    (make-texture
     :sampler sampler
     :source source
     :name name)))

(defun parse-textures (textures)
  (when textures
    (map 'vector #'parse-texture textures)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gltf-skin
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-skin (jobj)
  (u:mvlet ((inverse-bind-matrices (jsown:val-safe jobj "inverseBindMatrices"))
            (skeleton (jsown:val-safe jobj "skeleton"))
            (joints joints-p (jsown:val-safe jobj "joints"))
            (name (jsown:val-safe jobj "name")))

    (parse/assert joints-p 'gltf-skin "joints")

    (make-skin
     :inverse-bind-matricies inverse-bind-matrices
     :skeleton skeleton
     :joints (coerce joints 'vector)
     :name name)))

(defun parse-skins (skins)
  (when skins
    (map 'vector #'parse-skin skins)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse the root gltf object, finally.
;; ;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-gltf (jobj)
  (u:mvlet ((extensions-used ext-used-p
                             (jsown:val-safe jobj "extensionsUsed"))
            (extensions-required ext-req-p
                                 (jsown:val-safe jobj "extensionsRequired"))
            (accessors (jsown:val-safe jobj "accessors"))
            (animations (jsown:val-safe jobj "animations"))
            (asset asset-p (jsown:val-safe jobj "asset"))
            (buffers (jsown:val-safe jobj "buffers"))
            (buffer-views (jsown:val-safe jobj "bufferViews"))
            (cameras (jsown:val-safe jobj "cameras"))
            (images (jsown:val-safe jobj "images"))
            (materials (jsown:val-safe jobj "materials"))
            (meshes (jsown:val-safe jobj "meshes"))
            (nodes (jsown:val-safe jobj "nodes"))
            (samplers (jsown:val-safe jobj "samplers"))
            (scene (jsown:val-safe jobj "scene"))
            (scenes (jsown:val-safe jobj "scenes"))
            (skins (jsown:val-safe jobj "skins"))
            (textures (jsown:val-safe jobj "textures")))

    (parse/assert asset-p 'gltf "asset")

    (make-gltf
     :extensions-used (when ext-used-p
                        (parse-extensions-used extensions-used))
     :extensions-required (when ext-req-p
                            (parse-extensions-required
                             extensions-required))
     :accessors (parse-accessors accessors)
     :animations (parse-animations animations)
     :asset (parse-asset asset)
     :buffers (parse-buffers buffers)
     :buffer-views (parse-buffer-views buffer-views)
     :cameras (parse-cameras cameras)
     :images (parse-images images)
     :materials (parse-materials materials)
     :meshes (parse-meshes meshes)
     :nodes (parse-nodes nodes)
     :samplers (parse-samplers samplers)
     :scene scene
     :scenes (parse-scenes scenes)
     :skins (parse-skins skins)
     :textures (parse-textures textures))))


(defmethod typecheck ((pass (eql :static)) (obj gltf) &key)
  (with-accessors ((accessors accessors)
                   (buffer-views buffer-views))
      obj

    (loop :for accessor :across accessors
          :do (typecheck accessor :buffer-views buffer-views))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test code. (move to other file...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test/parse-indices (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (jsown:val
               (jsown:val
                (nth 1 (jsown:val j "accessors")) "sparse") "indices"))
         (inst (colony.file.gltf::parse-indices obj)))
    (describe inst)))

(defun test/parse-values (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (jsown:val
               (jsown:val
                (nth 1 (jsown:val j "accessors")) "sparse") "values"))
         (inst (colony.file.gltf::parse-values obj)))
    (describe inst)))

(defun test/parse-sparse (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (jsown:val
               (nth 1 (jsown:val j "accessors")) "sparse"))
         (inst (colony.file.gltf::parse-sparse obj)))
    (describe inst)))

(defun test/parse-accessor (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 1 (jsown:val j "accessors")))
         (inst (colony.file.gltf::parse-accessor obj)))
    (describe inst)))

(defun test/parse-target (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (jsown:val-safe
            (nth 0
                 (jsown:val-safe
                  (nth 0 (jsown:val j "animations"))
                  "channels"))
            "target"))
         (inst (colony.file.gltf::parse-target obj)))
    (describe inst)))

(defun test/parse-channel (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (nth 0
                (jsown:val-safe
                 (nth 0 (jsown:val j "animations"))
                 "channels")))
         (inst (colony.file.gltf::parse-channel obj)))
    (describe inst)))

(defun test/parse-animation-sampler (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (nth 0
                (jsown:val-safe
                 (nth 0 (jsown:val j "animations"))
                 "samplers")))
         (inst (colony.file.gltf::parse-animation-sampler obj)))
    (describe inst)))

(defun test/parse-animation (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "animations")))
         (inst (colony.file.gltf::parse-animation obj)))
    (describe inst)))

(defun test/parse-asset (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (jsown:val j "asset"))
         (inst (colony.file.gltf::parse-asset obj)))
    (describe inst)))

(defun test/parse-buffer (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "buffers")))
         (inst (colony.file.gltf::parse-buffer obj)))
    (describe inst)))

(defun test/parse-buffer-view (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 1 (jsown:val j "bufferViews")))
         (inst (colony.file.gltf::parse-buffer-view obj)))
    (describe inst)))

(defun test/parse-orthographic (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (jsown:val
            (nth 1 (jsown:val j "cameras"))
            "orthographic"))
         (inst (colony.file.gltf::parse-orthographic obj)))
    (describe inst)))

(defun test/parse-perspective (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (jsown:val
            (nth 0 (jsown:val j "cameras"))
            "perspective"))
         (inst (colony.file.gltf::parse-perspective obj)))
    (describe inst)))

(defun test/parse-camera (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (nth 0 (jsown:val j "cameras")))
         (inst (colony.file.gltf::parse-camera obj)))
    (describe inst)))

(defun test/parse-image (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (nth 0 (jsown:val j "images")))
         (inst (colony.file.gltf::parse-image obj)))
    (describe inst)))

(defun test/parse-normal-texture-info (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (jsown:val
            (nth 0 (jsown:val j "materials"))
            "normalTexture"))
         (inst (colony.file.gltf::parse-normal-texture-info obj)))
    (describe inst)))

(defun test/parse-occlusion-texture-info (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (jsown:val
            (nth 0 (jsown:val j "materials"))
            "occlusionTexture"))
         (inst (colony.file.gltf::parse-occlusion-texture-info obj)))
    (describe inst)))

(defun test/parse-texture-info (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (jsown:val
            (jsown:val
             (nth 0 (jsown:val j "materials"))
             "pbrMetallicRoughness")
            "baseColorTexture"))
         (inst (colony.file.gltf::parse-texture-info obj)))
    (describe inst)))

(defun test/parse-pbr-metallic-roughness (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (jsown:val
            (nth 0 (jsown:val j "materials"))
            "pbrMetallicRoughness"))
         (inst (colony.file.gltf::parse-pbr-metallic-roughness obj)))
    (describe inst)))

(defun test/parse-material (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "materials")))
         (inst (colony.file.gltf::parse-material obj)))
    (describe inst)))

(defun test/parse-primitive (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj
           (nth 0 (jsown:val (nth 0 (jsown:val j "meshes"))
                             "primitives")))
         (inst (colony.file.gltf::parse-primitive obj)))
    (describe inst)))

(defun test/parse-mesh (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "meshes")))
         (inst (colony.file.gltf::parse-mesh obj)))
    (describe inst)))

(defun test/parse-node (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "nodes")))
         (inst (colony.file.gltf::parse-node obj)))
    (describe inst)))

(defun test/parse-sampler (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "samplers")))
         (inst (colony.file.gltf::parse-sampler obj)))
    (describe inst)))

(defun test/parse-scene (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "scenes")))
         (inst (colony.file.gltf::parse-scene obj)))
    (describe inst)))

(defun test/parse-skin (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "skins")))
         (inst (colony.file.gltf::parse-skin obj)))
    (describe inst)))

(defun test/parse-texture (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (obj (nth 0 (jsown:val j "textures")))
         (inst (colony.file.gltf::parse-texture obj)))
    (describe inst)))

(defun test/parse-gltf (file)
  (let* ((j (colony.file.gltf::load-gltf-file file))
         (inst (colony.file.gltf::parse-gltf j)))
    (describe inst)))

(defun test/parse ()
  (let ((sample-sparse-accessor-file
          "/home/psilord/content/code/vendor/glTF-Sample-Models/2.0/SimpleSparseAccessor/glTF/SimpleSparseAccessor.gltf")
        (box-animated-file
          "/home/psilord/content/code/vendor/glTF-Sample-Models/2.0/BoxAnimated/glTF/BoxAnimated.gltf")
        (cameras-file "/home/psilord/content/code/vendor/glTF-Sample-Models/2.0/Cameras/glTF/Cameras.gltf")
        (images-file "/home/psilord/content/code/vendor/glTF-Sample-Models/2.0/DamagedHelmet/glTF/DamagedHelmet.gltf")
        (morph-file "/home/psilord/content/code/vendor/glTF-Sample-Models/2.0/MorphPrimitivesTest/glTF/MorphPrimitivesTest.gltf")
        (nodes-file "/home/psilord/content/code/vendor/glTF-Sample-Models/2.0/CesiumMilkTruck/glTF/CesiumMilkTruck.gltf")
        (skin-file "/home/psilord/content/code/vendor/glTF-Sample-Models/2.0/CesiumMan/glTF/CesiumMan.gltf"))


    (test/parse-indices sample-sparse-accessor-file)
    (test/parse-values sample-sparse-accessor-file)
    (test/parse-sparse sample-sparse-accessor-file)
    (test/parse-accessor sample-sparse-accessor-file)

    (test/parse-target box-animated-file)
    (test/parse-channel box-animated-file)
    (test/parse-animation-sampler box-animated-file)
    (test/parse-animation box-animated-file)

    (test/parse-asset box-animated-file)
    (test/parse-buffer box-animated-file)

    (test/parse-buffer-view box-animated-file)

    (test/parse-orthographic cameras-file)
    (test/parse-perspective cameras-file)
    (test/parse-camera cameras-file)

    (test/parse-image images-file)

    (test/parse-normal-texture-info images-file)
    (test/parse-occlusion-texture-info images-file)
    (test/parse-texture-info images-file)
    (test/parse-pbr-metallic-roughness images-file)
    (test/parse-material images-file)

    (test/parse-primitive morph-file)
    (test/parse-mesh morph-file)

    (test/parse-node nodes-file)
    (test/parse-sampler nodes-file)
    (test/parse-scene nodes-file)

    (test/parse-skin skin-file)
    (test/parse-texture skin-file)

    (test/parse-gltf skin-file)

    ))






;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data types to handle the loading of the glTF/glb file.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass glb-header ()
  ((%magic :accessor header-magic
           :initarg :header-magic
           :initform #x46546C67)
   (%version :accessor header-version
             :initarg :hreader-version
             :initform 2)
   (%length :accessor header-length
            :initarg :header-length)))

(defclass glb-chunk ()
  ((%length :accessor chunk-length
            :initarg :chunk-length)
   (%type :accessor chunk-type
          :initarg :chunk-type)
   (%data :accessor chunk-data
          :initarg :chunk-data)))

(defclass glib-container ()
  ((%header :accessor header
            :initarg :header)
   (%chunks :accessor chunks
            :initarg :chunks)))

(defun %load-gltf (path)
  (declare (optimize speed))
  (let ((json-string (u:file->string path)))
    (declare (simple-string json-string))
    ;; jsown has problems parsing strings with #\Return characters in it.  As in
    ;; it will produce WRONG answers and not crash which is incredibly hard to
    ;; debug! [Note, we once had all three #\Return #\Newline and #\Linefeed
    ;; here. I hope sometime in the future after 3 days of debugging and
    ;; cursing, we don't find we need to put the other two back again. :) ]
    (loop :for i :below (length json-string)
          :for c :of-type character = (aref json-string i)
          :when (char= c #\Return)
            :do (locally (declare (optimize (safety 0)))
                  (setf (aref json-string i) #\Space)))
    (jsown:parse json-string)))

(defun %load-glb (path)
  (declare (ignore path))
  nil)


;; not done.
(defun load-gltf-file (path &optional search-paths)
  (declare (ignore search-paths))
  (let ((file-type (pathname-type path)))
    (cond
      ((string= file-type "gltf")
       (let ((gltf (parse-gltf (%load-gltf path))))
         (typecheck gltf)
         gltf))
      ((string= file-type "glb")
       (%load-glb path))
      (t
       (error "Illegal glTF file (~A) with path extension: ~S"
              path file-type)))))




(defun test/load-gltf-files
    (&optional (examples-dir
                "/home/psilord/content/code/vendor/glTF-Sample-Models/2.0/"))

  ;; Grab all .glTF files from the examples-dir
  (let* ((gltf-files nil))
    (u:map-files (pathname examples-dir)
                 (lambda (file)
                   (push file gltf-files))
                 :test (lambda (n)
                         (string= (pathname-type n) "gltf")))
    ;; make it in the same order we found it while recursing
    (setf gltf-files (nreverse gltf-files))

    ;; Try just one
    ;;(format t "~S~%" (load-gltf-file (first gltf-files)))

    ;; Try them all
    (dolist (gltf-file gltf-files)
      (format t "Attempting to load .gltf file: ~S~%" gltf-file)
      (format t "~S~%" (load-gltf-file gltf-file)))

    (format t "Processed ~A gltf files.~%" (length gltf-files))))





;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:colony.geometry)

(defclass gltf ()
  ((%buffer :reader buffer
            :initarg :buffer)
   (%parse-tree :accessor parse-tree)
   (%json :accessor json)
   (%buffers :accessor buffers)
   (%allocated-views :accessor allocated-views
                     :initform nil)
   (%primitives :accessor primitives
                :initform nil)))

(defclass gltf-datastream ()
  ((%header :reader header)
   (%chunks :reader chunks)))

(defclass gltf-header ()
  ((%magic :reader format-magic)
   (%version :reader format-version)
   (%length :reader format-length)))

(defclass gltf-chunk ()
  ((%length :reader chunk-length)
   (%type :reader chunk-type)
   (%data :reader chunk-data)))

(u:define-printer (gltf-chunk stream :type t)
  (format stream "~s" (get-gltf-chunk-type gltf-chunk)))

(defun get-gltf-property (gltf key &optional object)
  (let ((object (or object (json gltf))))
    (when (jsown:keyp object key)
      (jsown:val (or object (json gltf)) key))))

(defun get-gltf-chunk-type (chunk)
  (case (chunk-type chunk)
    (#x4e4f534a :json-content)
    (#x004e4942 :binary-buffer)
    (otherwise :unknown)))

(defun parse-gltf-header (gltf)
  (let ((header (make-instance 'gltf-header)))
    (with-slots (%magic %version %length) header
      (let* ((buffer (fast-io:make-input-buffer
                      :vector (c::read-bytes (buffer gltf) 12)))
             (magic (c::read-string buffer :bytes 4)))
        (if (not (string= magic "glTF"))
            (error "Invalid glTF2 file.")
            (setf %magic magic
                  %version (c::read-uint-le buffer 4)
                  %length (c::read-uint-le buffer 4)))))
    header))

(defgeneric parse-gltf-chunk-data (gltf chunk-type chunk &key)
  (:method :around (gltf chunk-type chunk &key)
    (let ((buffer (fast-io:make-input-buffer
                   :vector (c::read-bytes (buffer gltf) (chunk-length chunk)))))
      (call-next-method gltf chunk-type chunk :buffer buffer))))

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :json-content)) chunk
                                  &key buffer)
  (let ((data (c::read-string buffer :encoding :utf-8)))
    (setf (json gltf) (jsown:parse data))
    data))

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :binary-buffer)) chunk
                                  &key buffer)
  (loop :with buffers = (get-gltf-property gltf "buffers")
        :with data = (make-array (length buffers))
        :for data-buffer :in buffers
        :for index :below (length buffers)
        :for size = (get-gltf-property gltf "byteLength" data-buffer)
        :do (setf (aref data index) (c::read-bytes buffer size))
        :finally (setf (buffers gltf) data))
  nil)

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :unknown)) chunk
                                  &key buffer)
  (declare (ignore buffer))
  (warn "Ignoring an unknown chunk type."))

(defun parse-gltf-chunk (gltf)
  (let ((chunk (make-instance 'gltf-chunk))
        (buffer (buffer gltf)))
    (with-slots (%length %type %data) chunk
      (setf %length (c::read-uint-le buffer 4)
            %type (c::read-uint-le buffer 4)
            %data (parse-gltf-chunk-data
                   gltf (get-gltf-chunk-type chunk) chunk)))
    chunk))

(defun parse-gltf-chunks (gltf)
  (loop :with stream = (fast-io:input-buffer-stream (buffer gltf))
        :until (= (file-position stream) (file-length stream))
        :for chunk = (parse-gltf-chunk gltf)
        :collect chunk))

(defun parse-gltf-datastream (gltf)
  (let ((datastream (make-instance 'gltf-datastream)))
    (with-slots (%header %chunks) datastream
      (setf %header (parse-gltf-header gltf)
            %chunks (parse-gltf-chunks gltf)))
    datastream))

(defun load-gltf-file (path)
  (u:with-binary-input (in path)
    (let* ((buffer (fast-io:make-input-buffer :stream in))
           (gltf (make-instance 'gltf :buffer buffer)))
      (setf (parse-tree gltf) (parse-gltf-datastream gltf))
      gltf)))

(defun get-gltf-component-type (gltf accessor)
  (ecase (get-gltf-property gltf "componentType" accessor)
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun get-gltf-component-count (data-type)
  (ecase (u:make-keyword data-type)
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun get-gltf-attribute-normalization (name component-type)
  (if (and (or (eq component-type :unsigned-byte)
               (eq component-type :unsigned-short))
           (not (string= name "JOINTS_0")))
      :true
      :false))

(defun get-gltf-primitive-mode (gltf primitive)
  (case (get-gltf-property gltf "mode" primitive)
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)
    (otherwise :triangles)))

(defun find-gltf-mesh (gltf index)
  (let ((meshes (get-gltf-property gltf "meshes")))
    (when (>= index (length meshes))
      (error "Mesh index ~d not found." index))
    (get-gltf-property gltf "primitives" (elt meshes index))))
