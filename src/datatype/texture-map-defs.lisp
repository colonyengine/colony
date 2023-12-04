(in-package #:virality.texture-map)

;; Notable properties that a texture-map AND a data-store can have. The data
;; store can elict to override the texture-map's properties with its own.
;; TODO: In the future this convert to an attribute store.
(defclass properties ()
  ((%origin :accessor origin
            :initarg :origin)
   (%channel-layout :accessor channel-layout
                    :initarg :channel-layout)
   ;; used only for cube maps.
   (%dir :accessor dir
         :initarg :dir)))

;; A representation of a single loadable/computable data element for a
;; texture-map.
(defclass data-element ()
  (;; The original form in the define-texture-map macro, unevaluated.
   ;; The legal values may be:
   ;; An asset pool form: example: (textures blue-heron)
   (%original :accessor original
              :initarg :original)
   ;; The resolved form (often a path or url gotten from the asset-pool)
   (%resolved :accessor resolved
              :initarg :resolved)
   ;; Whatever constitues a loaded, computed, or otherwise fully constructed
   ;; chunk of data, often an img:image object.
   (%computed :accessor computed
              :initarg :computed)))


;; These hold a collection of data elements. This base class can be used by
;; itself and can also contain derived instances of itself.
(defclass data-store ()
  ((%data-elements :accessor data-elements
                   :initarg :data-elements)))


;; A mipmap is usually a single image (which may or may not contain additional
;; mipmaps in that image), but in the case of 3d maps, a mipmap may consist of
;; multiple unique image slices at the mipmap level.
(defclass mipmap-store (data-store properties) ())

;; Index 0 is the supplied buffer name as a data-element.
(defclass buffer-name-store (data-store properties) ())

;; Index 0 is the image as a data-element.
(defclass image-store (mipmap-store properties) ())

;; Cube stores are annoying.... are properties even useful here?
;;
;; Cube maps can have more variation on how they are stored and they can move
;; between themselves, like a :combined :equirect cube map can be converted to
;; an :unique :opengl and we don't neccesarily want to create an entire new
;; object after the conversion but instead do the conversion in place. So,
;; instead of making lots of the usual specifc types here, we'll just keep the
;; style ontology and handle the store manually.
(defclass cube-store (data-store properties)
  (;; What is the exact kind of store model this cube map uses: :six, :opengl,
   ;; :auto, :vcross-top, (:custom ...), etc, etc, etc.
   (%style :accessor style
           :initarg :style)
   (%store :accessor store
           :initarg :store)))

;; The data-element :original will be this object for a cube face.
(defclass cube-face () ;; should derive from attributes. dir is an attr.
  (;; Something like :+x
   (%dir :accessor dir
         :initarg :dir
         :initform :unknown)
   ;; A symbol which names a texture-map
   (%name :accessor name
          :initarg :name)))


(defclass texture-map (properties)
  (;; The name could be anonymous but always must unique
   (%name :accessor name
          :initarg :name)
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
   ;; The data model form
   (%data-model :accessor data-model
                :initarg :data-model)
   ;; Hash table.
   ;; Key(EQUAL) is policy key form.
   ;; VALUE is policy value form.
   (%policy-attributes :accessor policy-attributes
                       :initarg :policy-attributes)
   ;; an array of 1 (:image ...) form or 1+ (:mipmap ...) form or
   ;; 1 (:buffer-name ...) form or an unprocessed cube map layout form.
   (%store-attributes :accessor store-attributes
                      :initarg :store-attributes)
   ;; The body form as written by the user for debugging.
   (%user-form :accessor user-form
               :initarg :user-form)))

;; --------------------------------------------------------------------------
