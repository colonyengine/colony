(in-package #:colony.model.gltf)

;;;; Extensions are not yet supported in this model. The defclass forms
;;;; don't even have the slots in them for representing extensions yet.

;;;; I didn't try and cram the parse and typechecking codes into
;;;; generic functions because the congruence or type selection doesn't
;;;; work out well.

;;;; Additional generic functions for the glTF data types
;;;; style can be :human or :json and transmits recursively
;;;; to the children of that thing
(defgeneric emit (style gltf-instance &key stream indent))
(defgeneric typecheck (pass gltf-instance &key &allow-other-keys))

;; Begin glTF data types.

(defclass gltf-sparse-indices ()
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
   ;; An instance of gltf-sparse-indices (which point to a buffer of indices)
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
   ;;
   ;; NOTE: we always store the symbol equivalent of the above values in this
   ;; slot.
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
   ;;
   ;; NOTE: we always store the symbol equivalent of the above values in this
   ;; slot.
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
   ;;
   ;; NOTE: we always store the symbol equivalent of the above values in this
   ;; slot.
   (%mag-filter :accessor mag-filter
                :initarg :mag-filter)
   ;; an integer, one of:
   ;; 9728 NEAREST
   ;; 9729 LINEAR
   ;; 9984 NEAREST_MIPMAP_NEAREST
   ;; 9985 LINEAR_MIPMAP_NEAREST
   ;; 9986 NEAREST_MIPMAP_LINEAR
   ;; 9987 LINEAR_MIPMAP_LINEAR
   ;;
   ;; NOTE: we always store the symbol equivalent of the above values in this
   ;; slot.
   (%min-filter :accessor min-filter
                :initarg :min-filter)
   ;; an integer, one of:
   ;; 33071 CLAMP_TO_EDGE
   ;; 33648 MIRRORED_REPEAT
   ;; 10497 REPEAT
   ;;
   ;; NOTE: we always store the symbol equivalent of the above values in this
   ;; slot.
   (%wrap-s :accessor wrap-s
            :initarg :wrap-s
            :initform 10497)
   ;; an integer, one of:
   ;; 33071 CLAMP_TO_EDGE
   ;; 33648 MIRRORED_REPEAT
   ;; 10497 REPEAT
   ;;
   ;; NOTE: we always store the symbol equivalent of the above values in this
   ;; slot.
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data types to handle the loading of the glb container format of the
;; gltf file.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
