(in-package #:colony.texture-map)

;;;; These types exist both at macro expansion time and at engine runtime.
;;;; It is inteded they are the products of define-texture-map and also the
;;;; runtime API for generating this stuff at runtime and giving it to the
;;;; engine.

;; A representation of a single loadable/computable data element for a
;; texture-map.
(defclass data-element ()
  (;; The original form in the define-texture-map macro, unevaluated.
   ;; The legal values may be:
   ;; An asset pool form: example: (textures blue-heron)
   ;; A function form with arguments: indicating this data should be generated.
   (%original :accessor original
              :initarg :original)
   ;; The resolved form (often a path or url gotten from the asset-pool)
   (%resolved :accessor resolved
              :initarg :resolved)
   ;; Whatever constitues a loaded, computed, or otherwise fully constructed
   ;; chunk of data, often an img:image object.
   (%computed :accessor computed
              :initarg :computed)))

;; TODO: Determine if I should derive this from ATTRIBUTE-BAG.
;; These hold a collection of data elements. This base class can be used by
;; itself and can also contain derived instances of itself.
(defclass data-store ()
  ((%data-elements :accessor data-elements
                   :initarg :data-elements)))

(defclass attributed-data-store (abag:attribute-bag data-store) ())

;; A mipmap is usually a single image (which may or may not contain additional
;; mipmaps in that image), but in the case of 3d maps, a mipmap may consist of
;; multiple unique image slices at the mipmap level.
(defclass mipmap-store (attributed-data-store) ())

;; Index 0 is the supplied buffer name as a data-element.
(defclass buffer-name-store (attributed-data-store ) ())

;; Index 0 is the image as a data-element.
(defclass image-store (mipmap-store) ())

;; Cube stores are annoying.... are properties even useful here?
;;
;; Cube maps can have more variation on how they are stored and they can move
;; between themselves, like a :combined :equirect cube map can be converted to
;; an :unique :six and we don't neccesarily want to create an entire new object
;; after the conversion but instead do the conversion in place. So, instead of
;; making lots of the usual specifc types here, we'll just keep the style
;; ontology and handle the store manually.

;; There will either be 6 entries which are face-stores in this cube store, or
;; 1 or more mipmap-stores which represent all faces at once. We tell the
;; difference by inspection of the style.
(defclass cube-store (attributed-data-store)
  (;; What is the exact kind of store model this cube map uses: :six, :opengl,
   ;; :auto, :vcross-top, (:custom ...), etc, etc, etc.
   (%style :accessor style
           :initarg :style)
   (%store :accessor store
           :initarg :store)))

;; Index 0 will be a data-element instance containing a symbol name for a
;; texture (either directly provided or as a consquence of resolving a
;; define-texture-map).  There will be a :dir attribute in here holding the
;; direction of the face.
(defclass face-store (attributed-data-store) ())


(defclass texture-map (abag:attribute-bag)
  (;; Currently must be a symbol and not NIL. If the texture-map's name was NIL
   ;; in the define-texture-map form, then this will end up being a gensym
   ;; name.
   (%name :accessor name
          :initarg :name)
   ;; This slot if T is the name started out as NIL meaning this is an
   ;; anonymous texture-map. Otherwise it is NIL.
   (%anonymous-p :accessor anonymous-p
                 :initarg :anonymous-p)
   ;; Storage model slots
   (%model :accessor model
           :initarg :model)
   (%style :accessor style
           :initarg :style)
   (%store :accessor store
           :initarg :store)
   ;; What kind of data-storage object goes into this store is dependent on the
   ;; storage model of the texture map.
   (%data-store :accessor data-store
                :initarg :data-store)
   ))

(defclass texture-map-single (texture-map) ())
(defclass texture-map-rect (texture-map) ())
(defclass texture-map-buffer (texture-map) ()) ;; has properties but ignored.
(defclass texture-map-voxel (texture-map) ())
(defclass texture-map-cube (texture-map) ())

(defgeneric enumerate-data-elements (texture-map))

;; --------------------------------------------------------------------------

;; TODO: Fill this in. These go into =meta/texture-maps= and are similar to the
;; texture-descriptor classes.
(defclass texture-map-descriptor ()
  ((%name :accessor name
          :initarg :name)
   (%ast :accessor ast
         :initarg :ast)
   ;; This descriptor might include additional texture-maps that were generated
   ;; on behalf of this texture (currently only cube maps can do this). It is
   ;; up to the reifier to ensure that the extra-asts are they are handled
   ;; properly wrt anonymity or interactive update, etc.
   (%extra-asts :accessor extra-asts
                :initarg :extra-asts)
   ;; The original form as written by the user for debugging.
   (%user-form :accessor user-form
               :initarg :user-form)))

;; --------------------------------------------------------------------------

(in-package #:colony.texture-map.texture-map-table)

;; This datatype is used in the CORE type to maintain the texture-map state.
(defclass texture-map-table ()
  (;; All texture-map-descriptors from DEFINE-TEXTURE-MAP are stored here.
   (%semantic-texture-map-descriptors
    :reader semantic-texture-map-descriptors
    :initarg :semantic-texture-map-descriptors
    :initform (u:dict #'equal))))
