(in-package #:virality.file.gltf)

;;;; Extensions are not yet supported in this model.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gltf-indicies ()
  (;; An integer, cannot reference a ARRAY_BUFFER or ELEMENT_ARRAY_BUFFER target
   (%buffer-view :accessor buffer-view
                 :initarg :buffer-view)
   ;; An integer
   (%byte-offset :accessor byte-offset
                 :initarg :byte-offset
                 :initform 0)
   ;; One of:
   ;;
   ;; 5121 UNSIGNED_BYTE
   ;; 5123 UNSIGNED_SHORT
   ;; 5125 UNSIGNED_INT
   (%component-type :accessor component-type
                    :initarg :component-type)))

(defclass gltf-values ()
  (;; An integer, cannot reference a ARRAY_BUFFER or ELEMENT_ARRAY_BUFFER target
   (%buffer-view :accessor buffer-view
                 :initarg :buffer-view)
   (%byte-offset :accessor byte-offset
                 :initarg :byte-offset
                 :initform 0)
   ;; One of:
   ;;
   ;; 5121 UNSIGNED_BYTE
   ;; 5123 UNSIGNED_SHORT
   ;; 5125 UNSIGNED_INT
   (%component-type :accessor component-type
                    :initarg :component-type)))

(defclass gltf-sparse ()
  (;; An integer
   (%entity-count :accessor entity-count
                  :initarg :entity-count
                  :initform 1)
   ;; An array of gltf-indicies instances
   (%indicies :accessor indicies
              :initarg :indicies)
   ;; An array of gltf-values instances
   (%vals :accessor vals
          :initarg :vals)))


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
   (%attribute-count :accessor attribute-count
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
   (%attribute-type :accessor attribute-type
                    :initarg :attribute-type)
   ;; An instance of the correct type as denoted in ATTRIBUTE-TYPE
   (%max-val :accessor max-val
             :initarg :max-val)
   ;; An instance of the correct type as denoted in ATTRIBUTE-TYPE
   (%min-val :accessor min-val
             :initarg :min-val)
   ;; A gltf-sparse instance
   (%sparse :accessor sparse
            :initarg :sparse)))
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
              :initarg samplers)))

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
           :initarg y-mag)
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
   (%camera-type :accessor camera-type
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
                        :initarg :base-color-texture)
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
                                :initarg :metallic-roughness-texture)))

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
                :initform (u:dict #'equalp))
   ;; an integer
   (%indicies :accessor indicies
              :initarg :indicies)
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
   ;; "POSITION",
   ;; "NORMAL",
   ;; "TANGENT"
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
                 :initarg :scale
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


(in-package #:virality.geometry)

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
                      :vector (v::read-bytes (buffer gltf) 12)))
             (magic (v::read-string buffer :bytes 4)))
        (if (not (string= magic "glTF"))
            (error "Invalid glTF2 file.")
            (setf %magic magic
                  %version (v::read-uint-le buffer 4)
                  %length (v::read-uint-le buffer 4)))))
    header))

(defgeneric parse-gltf-chunk-data (gltf chunk-type chunk &key)
  (:method :around (gltf chunk-type chunk &key)
    (let ((buffer (fast-io:make-input-buffer
                   :vector (v::read-bytes (buffer gltf) (chunk-length chunk)))))
      (call-next-method gltf chunk-type chunk :buffer buffer))))

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :json-content)) chunk
                                  &key buffer)
  (let ((data (v::read-string buffer :encoding :utf-8)))
    (setf (json gltf) (jsown:parse data))
    data))

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :binary-buffer)) chunk
                                  &key buffer)
  (loop :with buffers = (get-gltf-property gltf "buffers")
        :with data = (make-array (length buffers))
        :for data-buffer :in buffers
        :for index :below (length buffers)
        :for size = (get-gltf-property gltf "byteLength" data-buffer)
        :do (setf (aref data index) (v::read-bytes buffer size))
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
      (setf %length (v::read-uint-le buffer 4)
            %type (v::read-uint-le buffer 4)
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
