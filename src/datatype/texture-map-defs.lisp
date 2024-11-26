(in-package #:colony.texture-map)

;;;; These types exist both at macro expansion time and at engine runtime.
;;;; It is inteded they are the products of define-texture-map and also the
;;;; runtime API for generating this stuff at runtime and giving it to the
;;;; engine.


;; TODO: move to location-defs.lisp and a core-early/location.lisp
(defclass location ()
  ((%logloc :accessor logloc
            :initarg :logloc
            :initform nil
            :documentation
            "Logical (or virtualized) location of the data. Usu. asset form.")
   (%physloc :accessor physloc
             :initarg :physloc
             :initform nil
             :documentation
             "Physical location of the data (disk path, url, index, etc).")))
;; API
;; function
;; (make-location :logloc logloc :physloc physloc)

;; Generic, could be used to denote images, audio files, models, etc.
;; TODO: move to data-element-defs.lisp and a core-early/data-elements.lisp
(defclass data-element (location)
  ((%element :accessor element
             :initarg :element
             :initform nil
             :documentation
             "In-memory object of data that can be read/written.")))
;; API
;; method, factory-like
;; (make-data-element type :logloc logloc :physloc physloc :element element)
(defgeneric make-data-element (type &key logloc physloc element
                               &allow-other-keys))

(defclass image-element (data-element) ())
(defclass texture-map-element (data-element) ())
;; API handled by make-data-element

;; A base class that represents a span of something in any coordinate system.
(defclass span ()
  ((%origin :accessor origin
            :initarg :origin
            :initform nil
            :documentation "The origin of the span.")
   (%extent :accessor extent
            :initarg :extent
            :initform nil
            :documentation "Magnitude/direction/etc of the span.")))
;; API
;; method, factorylike
;; (make-span type :origin origin :extent extent)
(defgeneric make-span (type &key origin extent &allow-other-keys))

(defclass span-1d (span) ())
(defclass span-2d (span) ())
(defclass span-3d (span) ())
;; API handled by make-span

;; Inspect. The inheritance here is odd. However, it allows using the type
;; DATA-SPAN as a :type in defclass and in a method and be able to allow
;; storing of subtypes there.
(defclass data-span (span)
  ((%elidx :accessor elidx
           :initarg :elidx
           :initform nil
           :type (or null integer)
           :documentation "An index into an array of data-elements.")))
;; API
;; method, factory-like
;; (make-data-span type :elidx val)
(defgeneric make-data-span (type &key origin extent elidx &allow-other-keys))

(defclass data-span-1d (data-span span-1d) ())
(defclass data-span-2d (data-span span-2d) ())
(defclass data-span-3d (data-span span-3d) ())
;; API handled by make-data-span

(defclass mapping-span ()
  ((%to :accessor to
        :initarg :to
        :initform nil
        :type (or null data-span)
        :documentation
        "A DATA-SPAN to which the FROM is written. It can be equal to or ~
higher dimensionality than the DATA-SPAN in FROM. The coordinate system has ~
the same origin as the EXTENT in whatever storage form this MAPPING-SPAN is ~
used in -- so if the DATA-SPAN has a different origin, it is wrt to that.")
   (%from :accessor from
          :initarg :from
          :initform nil
          :type (or null data-span)
          :documentation
          "A DATA-SPAN which selects an -dim region from the indexed ~
DATA-ELEMENT. The origin of the coordinate system is whatever the coordinate ~
system is in the data-element.")))
;; API
;; method, factory-like
;; (make-mapping-span type :to to :from from)
(defgeneric make-mapping-span (type &key to from &allow-other-keys))

(defclass mapping-span-1d (mapping-span) ())
(defclass mapping-span-2d (mapping-span) ())
(defclass mapping-span-3d (mapping-span) ())
;; API handled by make-mapping-span

(defclass storage-form (abag:attribute-bag) ()
  (:documentation
   "Represents whatever means a texture map might use to store data, such as ~
a mipmap, an image, a rect specification, a cube of faces, etc."))
;; API
;; method, factory-like
;;; (make-storage-form type :inherit-attrs abag :attrs list-of-attributes)
(defgeneric make-storage-form (type &key bags attrs cattrs sattrs
                               &allow-other-keys))

(defclass mipmap (storage-form)
  ((%extent :accessor extent
            :initarg :extent
            :initform nil
            :type (or null span)
            :documentation
            "A span with zero origin indicating the size of the mipmap in ~
however many dimensions it needs to be--or nil")
   (%mapping-spans :accessor mapping-spans
                   :initarg :mapping-spans
                   :initform nil
                   :type (or null (vector mapping-span))
                   :documentation
                   "An array of MAPPING-SPAN objects which cover the extent ~
with no overlaps or holes--or nil. Mipmaps never share mapping spans.")))
(defclass mipmap-1d (mipmap) ())
(defclass mipmap-2d (mipmap) ())
(defclass mipmap-3d (mipmap) ())
;; API handled by make-storage-form

(defclass face (storage-form)
  ((%elidx :accessor elidx
           :initarg :elidx
           :initform nil
           :type (or null integer)
           :documentation "An index into an array of data-elements.")
   (%dir :accessor dir
         :initarg :dir
         :initform nil
         :type symbol ;; Make sure there is a runtime check of the validity.
         :documentation "The direction of this face in a cubmap.")))
;; API handled by make-storage-form

(defclass cube-representation ()
  ()
  (:documentation "A base class for very different representations of cubes ~
used for cube maps. Example of differences are: six individual faces, or one ~
environment map which may contain a range of different layouts."))
;; API
;; method, factory-like
;; (make-cube-representation type)
(defgeneric make-cube-representation (type &key &allow-other-keys))

;; (:cube :faces [:six | :opengl])
;;
;; :faces means each face (of which there MUST be six) is in its own
;; (named or anonymous) texture map (which may or may not itself
;; be :unique or :combined).
;; The :six or :opengl is how each face is identified and is assumed to
;; be :six if not supplied.
(defclass faces-representation (cube-representation)
  ((%faces :accessor faces
           :initarg :faces
           :initform nil
           :type (or null (vector face))
           :documentation
           "An array of exactly 6 FACE objects that describe a cube.")))
;; API handled by make-cube-representation

;; (:cube :envmap [:vcross | :equirect | etc]))
;;
;; :envmap means all faces at a certain mipmap layer encoded in a single image
;; there maybe additional mipmap images with more layers in that
;; representation. If more than one mipmap is supplied, it is treated as
;; multiple mipmaps (and the engine will verify the dimensions). The engine
;; won't be able to determine for itself how many mipmaps there are. It must be
;; explicitly told the exact number present.
(defclass envmap-representation (cube-representation)
  ((%mipmaps :accessor mipmaps
             :initarg :mipmaps
             :initform nil
             :type (or null (vector mipmap))
             :documentation
             "An adjustable array of mipmaps or nil")))
;; API handled by make-cube-representation

(defclass cube ()
  (;; What is the exact kind of store model this cube map uses: :six, :opengl,
   ;; :auto, :vcross-top, (:custom ...), etc, etc, etc.
   (%style :accessor style
           :initarg :style)
   (%store :accessor store
           :initarg :store)
   (%repr :accessor repr
          :initarg :repr
          :initform nil
          :type (or null cube-representation)
          :documentation
          "A CUBE can hold its data in one of multiple representations ~
and can translate between them at runtime. This slot holds the specific ~
CUBE-REPRESENTATION in current use.")))
;; API
;; function
;; (make-cube :style style :store store :repr repr)

(defclass texture-map (abag:attribute-bag)
  ((%name :accessor name
          :initarg :name
          :initform :unspecified
          :type (not null)
          :documentation
          "Specified by the define-texture-map form, or gensymed, or ~
:unspecified. If a texture-map with the name :unspecified is observed by the
engine, it is an error.")
   (%anonymous-p :accessor anonymous-p
                 :initarg :anonymous-p
                 :initform nil
                 :documentation
                 "This is T if the NAME was NIL originally in the ~
DEFINE-TEXTURE-MAP form. It is NIL otherwise.")
   ;; Original storage model slots, in the case of a cube map, the cube map
   ;; storage form contains the currently active representation.
   (%model :accessor model
           :initarg :model
           :initform nil
           :type symbol
           :documentation "The model of the texture: :1d, :2d, :3d, :cube...")
   (%style :accessor style
           :initarg :style
           :initform nil
           :type symbol
           :documentation "The style of the texture. Often it is :unique or ~
:combined, but for cube maps may be :envmap or :faces.")
   (%store :accessor store
           :initarg :store
           :initform nil
           :type (or symbol list)
           :documentation "The store of the texture. It may be a symbol or a ~
list and it dependant on the MODEL and STYLE of the texture.")
   (%data-elements :accessor data-elements
                   :initarg :data-elements
                   :initform nil
                   :type (or null (vector data-element))
                   :documentation
                   "An adjustable array of data-elements that represent where ~
to find the texel data for this texture-map or nil.")))
;; API
;; method, factory-like
;; (make-texture-map type
;;  :name name :anonymous-p anonymous-p
;;  :model model :style style :store store
;;  :data-elements array-of-data-elements &allow-other-keys)
(defgeneric make-texture-map (type &key name anonymous-p model style store
                                     data-elements bags attrs cattrs sattrs
                              &allow-other-keys))

(defclass texture-map-simple (texture-map)
  ((%mipmaps :accessor mipmaps
             :initarg :mipmaps
             :initform nil
             :type (or null (vector mipmap))
             :documentation
             "An adjustable array of mipmaps or nil"))
  (:documentation "A texture-map of a single image which may or may not
have hierarchical mipmaps."))
;; API handled by make-texture-map

;; user API
(defclass texture-map-1d (texture-map-simple) ())
(defclass texture-map-2d (texture-map-simple) ())
(defclass texture-map-3d (texture-map-simple) ())
;; API handled by make-texture-map factory

;; The storage forms can morph between themselves and so they are abstracted
;; into a "cube" data structure held in the texture-map which allows changing
;; the representation without having to change the texture-map-cube instance
;; itself.
;;
;; example kinds of conversions: equirectangular to six distinct faces,
;; :vcross to :hcross, combined faces or unique faces, etc, etc.
;; It is a horror show.
(defclass texture-map-complex (texture-map) ()
  (:documentation "A texture-map built from multiple other texture-maps ~
or something that is more complex than just a single image and its mipmaps. ~
Example: A cube map."))
;; API handled by make-texture-map

;; user API
(defclass texture-map-cube (texture-map-complex)
  ((%cube :accessor cube
          :initarg :cube
          :initform nil
          :type (or null cube))))
;; API handled by make-texture-map

;; --------------------------------------------------------------------------

(defclass texture-map-descriptor ()
  (;; This name is canonicalized, meaning if it was NIL, a name was generated.
   (%name :accessor name
          :initarg :name)
   ;; Was the original name nil (and therefore anonymous)?
   (%anonymous-p :accessor anonymous-p
                 :initarg :anonymous-p)
   ;; The function which constructs the in memory representation of the
   ;; texture-map data structure. The constructed in memory representation
   ;; still must pass through a gauntlet of code before it is actually a real
   ;; texture-map that can be used.
   (%constructor :accessor constructor
                 :initarg :constructor)
   ;; The original form as written by the user for debugging. Will be NIL if it
   ;; is unknown.
   (%original-form :accessor original-form
                   :initarg :original-form
                   :initform nil)))


;; -------------------------------------------------------------------------- ;

(in-package #:colony.texture-map.texture-map-table)

;; This datatype is used in the CORE type to maintain the texture-map state.
(defclass texture-map-table ()
  (;; All texture-map-descriptors from DEFINE-TEXTURE-MAP are cloned from the
   ;; metaspace and stored here at engine start time.
   ;;
   ;; Key: name supplied to toplevel define-texture-map.
   ;; Value: a texture-map-descriptor
   (%semantic-texture-map-descriptors
    :reader semantic-texture-map-descriptors
    :initarg :semantic-texture-map-descriptors
    :initform (u:dict #'equal))

   ;; Resolved texture-maps are cloned from the semantic texture maps.
   ;; Each texture-map stored in a texture-map-descriptor, including any
   ;; anonymous texture-maps for cube maps, are lifted to the same level
   ;; and stored here.
   ;;
   ;; Key: name supplied to ANY define-texture-map (or names generated from
   ;; define-texture-map with a NIL name).
   ;; Value: A texture-map instance.
   (%resolved-texture-maps :reader resolved-texture-maps
                           :initarg :resolved-texture-maps
                           :initform (u:dict #'equal))))
