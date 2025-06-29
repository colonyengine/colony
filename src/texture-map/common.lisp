(in-package #:colony.texture-map)

;; TODO: Implement the cloning code for these objects.

;;; -----------------
;; defclass LOCATION support code
;;; -----------------
(defun make-location (&key logloc physloc)
  (make-instance 'location :logloc logloc :physloc physloc))

;;; -----------------
;; defclass DATA-ELEMENT (and children) support code
;; NOTE: Types 'data-element, 'image-element, 'texture-map-element all take the
;; same arguments, but of different types.
;;; -----------------

;; Extensible API
(defmethod make-data-element ((type symbol) &key logloc physloc element)
  (make-instance type :logloc logloc :physloc physloc :element element))
;; Convenience API
(defun make-image-element (&key logloc physloc element)
  (make-data-element 'image-element
                     :logloc logloc :physloc physloc :element element))
(defun make-texture-map-element (&key logloc physloc element)
  (make-data-element 'texture-map-element
                     :logloc logloc :physloc physloc :element element))

;;; -----------------
;; defclass SPAN (and children) support code
;; NOTE: Types 'span-1d, 'span-2d, 'span-3d all take the same arguments, but of
;; different types
;;; -----------------

;; Extensible API
(defmethod make-span ((type symbol) &key origin extent)
  (make-instance type :origin origin :extent extent))
;; Convenience API
(defun make-span-1d (&key origin extent)
  (make-span 'span-1d :origin origin :extent extent))
(defun make-span-2d (&key origin extent)
  (make-span 'span-2d :origin origin :extent extent))
(defun make-span-3d (&key origin extent)
  (make-span 'span-3d :origin origin :extent extent))

;;; -----------------
;; defclass DATA-SPAN (and children) support code
;; NOTE: Types 'data-span-1d, 'data-span-2d, 'data-span-3d all take the same
;; arguments, but of different types.
;;; -----------------

;; Extensible API
(defmethod make-data-span ((type symbol) &key origin extent elidx)
  (make-instance type :origin origin :extent extent :elidx elidx))
;; Convenience API
(defun make-data-span-1d (&key origin extent elidx)
  (make-data-span 'data-span-1d :origin origin :extent extent :elidx elidx))
(defun make-data-span-2d (&key origin extent elidx)
  (make-data-span 'data-span-2d :origin origin :extent extent :elidx elidx))
(defun make-data-span-3d (&key origin extent elidx)
  (make-data-span 'data-span-3d :origin origin :extent extent :elidx elidx))

;;; -----------------
;; defclass MAPPING-SPAN (and children) support code
;; NOTE: Types 'mapping-span-1d, 'mapping-span-2d, 'mapping-span-3d all
;; take the same arguments, but of different types.
;;; -----------------

;; Extensible API
(defmethod make-mapping-span ((type symbol) &key to from)
  (make-instance type :to to :from from))
;; Convenience API
(defun make-mapping-span-1d (&key to from)
  (make-mapping-span 'mapping-span-1d :to to :from from))
(defun make-mapping-span-2d (&key to from)
  (make-mapping-span 'mapping-span-2d :to to :from from))
(defun make-mapping-span-3d (&key to from)
  (make-mapping-span 'mapping-span-3d :to to :from from))

;;; -----------------
;; defclass STORAGE-FORM (and children) support code
;;; -----------------

;;; -----------------
;; defclass MIPMAP (and children) support code
;; NOTE: Types 'mipmap-1d, 'mipmap-2d, 'mipmap-3d all take the same arguments,
;; but of different types. So this is the general matcher for those types.
;;; -----------------

;; Extensible API
(defmethod make-storage-form ((type symbol) &key extent mapping-spans
                                              bags attrs cattrs sattrs)
  (let ((mipmap (make-instance type :extent extent
                                    :mapping-spans mapping-spans)))
    (abag:absorb mipmap :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs)
    mipmap))
;; Convenience API
(defun make-mipmap-1d (&key extent mapping-spans bags attrs cattrs sattrs)
  (make-storage-form 'mipmap-1d :extent extent :mapping-spans mapping-spans
                                :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs))
(defun make-mipmap-2d (&key extent mapping-spans bags attrs cattrs sattrs)
  (make-storage-form 'mipmap-2d :extent extent :mapping-spans mapping-spans
                                :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs))
(defun make-mipmap-3d (&key extent mapping-spans bags attrs cattrs sattrs)
  (make-storage-form 'mipmap-3d :extent extent :mapping-spans mapping-spans
                                :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs))

;;; -----------------
;; defclass FACE (and children) support code
;; NOTE: Types 'mipmap-1d, 'mipmap-2d, 'mipmap-3d all take the same arguments,
;; but of different types. This is a specialized method.
;;; -----------------

;; Extensible API
(defmethod make-storage-form ((type (eql 'face))
                              &key elidx dir bags attrs cattrs sattrs)
  (let ((face (make-instance type :elidx elidx
                                  :dir dir)))
    (abag:absorb face :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs)
    face))
;; Convenience API
(defun make-face (&key elidx dir bags attrs cattrs sattrs)
  (make-storage-form 'face :elidx elidx :dir dir :bags bags :attrs attrs
                           :cattrs cattrs :sattrs sattrs))

;;; -----------------
;; defclass CUBE-REPRESENTATION (and children) support code
;; NOTE: Types 'faces and 'envmap take very different arguments and so must
;; be immediately specialized.
;;; -----------------

;;; -----------------
;; defclass FACES (and children) support code
;;; -----------------

;; Extensible API
(defmethod make-cube-representation ((type (eql 'faces-representation))
                                     &key faces)
  (unless (or (not faces)
              (subtypep (type-of faces) '(vector face)))
    (error "make-cube-representation: type: ~A The type of FACES is wrong."
           type))
  (make-instance type :faces faces))
;; Convenience API
(defun make-faces-representation (&key faces)
  (make-cube-representation 'faces-representation :faces faces))

;;; -----------------
;; defclass ENVMAP (and children) support code
;;; -----------------

;; Extensible API
(defmethod make-cube-representation ((type (eql 'envmap-representation))
                                     &key mipmaps)
  (unless (or (not mipmaps)
              (subtypep (type-of mipmaps) '(vector mipmap)))
    (error "make-cube-representation: type: ~A The type of MIPMAPS is wrong."
           type))
  (make-instance type :mipmaps mipmaps))
;; Convenience API
(defun make-envmap-representation (&key mipmaps)
  (make-cube-representation 'envmap-representaton :mipmaps mipmaps))

;;; -----------------
;; defclass CUBE (and children) support code
;;; -----------------

(defun make-cube (&key style store repr)
  (make-instance 'cube :style style :store store :repr repr))

;;; -----------------
;; defclass TEXTURE-MAP-STATE support code
;;; -----------------

(defun make-texture-map-state (&rest initargs)
  (apply #'make-instance 'texture-map-state initargs))

;;; -----------------
;; defclass TEXTURE-MAP (and children) support code
;; NOTE: Factory for 'texture-map-1d, 'texture-map-2d, 'texture-map-3d as
;; the general symbol and a specialized method for 'texture-map-cube.
;;; -----------------

;; Extensible API
(defmethod make-texture-map ((type symbol)
                             &key name anonymous-p model style store
                               data-elements state
                               mipmaps bags attrs cattrs sattrs)
  ;; TYPE ends up being all the simple texture maps.
  (let ((texture-map
          (make-instance type
                         :name name :anonymous-p anonymous-p :model model
                         :style style :store store
                         :data-elements data-elements
                         :state (u:default state (make-texture-map-state))
                         :mipmaps mipmaps)))
    (abag:absorb texture-map
                 :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs)
    texture-map))
(defmethod make-texture-map ((type (eql 'texture-map-cube))
                             &key name anonymous-p model style store
                               data-elements state
                               cube bags attrs cattrs sattrs)
  ;; TYPE is a complex texture map type.
  (let ((texture-map
          (make-instance type
                         :name name :anonymous-p anonymous-p :model model
                         :style style :store store
                         :data-elements data-elements
                         :state (u:default state (make-texture-map-state))
                         :cube cube)))
    (abag:absorb texture-map
                 :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs)
    texture-map))
;; Convenience API
(defun make-texture-map-1d (&key name anonymous-p model style store
                              data-elements mipmaps bags attrs cattrs sattrs)
  (make-texture-map 'texture-map-1d :name name :anonymous-p anonymous-p
                                    :model model :style style :store store
                                    :data-elements data-elements
                                    :mipmaps mipmaps :bags bags :attrs attrs
                                    :cattrs cattrs :sattrs sattrs))
(defun make-texture-map-2d (&key name anonymous-p model style store
                              data-elements mipmaps bags attrs cattrs sattrs)
  (make-texture-map 'texture-map-2d :name name :anonymous-p anonymous-p
                                    :model model :style style :store store
                                    :data-elements data-elements
                                    :mipmaps mipmaps :bags bags :attrs attrs
                                    :cattrs cattrs :sattrs sattrs))
(defun make-texture-map-3d (&key name anonymous-p model style store
                              data-elements mipmaps bags attrs cattrs sattrs)
  (make-texture-map 'texture-map-3d :name name :anonymous-p anonymous-p
                                    :model model :style style :store store
                                    :data-elements data-elements
                                    :mipmaps mipmaps :bags bags :attrs attrs
                                    :cattrs cattrs :sattrs sattrs))
(defun make-texture-map-cube (&key name anonymous-p model style store
                                data-elements cube bags attrs cattrs sattrs)
  (make-texture-map 'texture-map-cube :name name :anonymous-p anonymous-p
                                      :model model :style style :store store
                                      :data-elements data-elements
                                      :cube cube :bags bags :attrs attrs
                                      :cattrs cattrs :sattrs sattrs))

;;; -----------------
;; Additional convenience API for building array containers used in this API.
;;; -----------------

(defun %make-container-array (type len init-action data)
  "A helper function used to make arrays of specific types and contents."
  (ecase init-action
    (:contents
     (make-array len :element-type type
                     :adjustable t :fill-pointer t
                     :initial-contents data))
    (:init
     (unless (integerp len)
       (error "%make-container-array: supplied length is not an integer: ~A"
              len))
     (make-array len :element-type type
                     :adjustable t :fill-pointer t
                     :initial-element data))))


(defun make-data-elements (action &rest elements)
  "Return an adjustable and fillable array suitable for holding elements of
type (OR NULL DATA-ELEMENT). If ACTION is :encode, then ELEMENTS is a rest list
that contains instances of DATA-ELEMENT objects. These objects will be read in
left to right order and assigned into the array in accordance with their
ordinal location in the ELEMENTS list. If ACTION is :length, only the first
position is inspected in ELEMENTS list, which must be an integer denoting the
desired length of the array, and an array is constructed of that length with
nil entries and returned."
  (let ((array-type '(or null data-element)))
    (ecase action
      (:encode
       (%make-container-array array-type (length elements) :contents elements))
      (:length
       (assert (= (length elements) 1))
       (%make-container-array array-type (first elements) :init nil)))))

(defun make-mapping-spans (action &rest elements)
  "Return an adjustable and fillable array suitable for holding elements of
type (OR NULL MAPPING-SPAN). If ACTION is :encode, then ELEMENTS is a rest list
that contains instances of MAPPNG-SPAN objects. These objects will be read in
left to right order and assigned into the array in accordance with their
ordinal location in the ELEMENTS list. If ACTION is :length, only the first
position is inspected in ELEMENTS list, which must be an integer denoting the
desired length of the array, and an array is constructed of that length with
nil entries and returned."
  (let ((array-type '(or null mapping-span)))
    (ecase action
      (:encode
       (%make-container-array array-type (length elements) :contents elements))
      (:length
       (assert (= (length elements) 1))
       (%make-container-array array-type (first elements) :init nil)))))

(defun make-mipmaps (action &rest elements)
  "Return an adjustable and fillable array suitable for holding elements of
type (OR NULL MIPMAP). If ACTION is :encode, then ELEMENTS is a rest list
that contains instances of MIPMAP objects. These objects will be read in
left to right order and assigned into the array in accordance with their
ordinal location in the ELEMENTS list. If ACTION is :length, only the first
position is inspected in ELEMENTS list, which must be an integer denoting the
desired length of the array, and an array is constructed of that length with
nil entries and returned."
  (let ((array-type '(or null mipmap)))
    (ecase action
      (:encode
       (%make-container-array array-type (length elements) :contents elements))
      (:length
       (assert (= (length elements) 1))
       (%make-container-array array-type (first elements) :init nil)))))

(defun make-faces (action &rest elements)
  "Return an adjustable and fillable array suitable for holding elements of
type (OR NULL FACE). If ACTION is :encode, then ELEMENTS is a rest list
that contains instances of FACE objects. These objects will be read in
left to right order and assigned into the array in accordance with their
ordinal location in the ELEMENTS list. If ACTION is :length, only the first
position is inspected in ELEMENTS list, which must be an integer denoting the
desired length of the array, and an array is constructed of that length with
nil entries and returned."
  (let ((array-type '(or null face)))
    (ecase action
      (:encode
       (%make-container-array array-type (length elements) :contents elements))
      (:length
       (assert (= (length elements) 1))
       (%make-container-array array-type (first elements) :init nil)))))

;;; -----------------

(defun make-warming-info/texture-map (&rest init-args)
  (apply #'make-instance 'warming-info/texture-map init-args))

;;; -----------------

(defun make-texture-map-descriptor (name anonymous-p constructor
                                    &optional original-form)
  (make-instance 'texture-map-descriptor
                 :name name
                 :anonymous-p anonymous-p
                 :constructor constructor
                 :original-form original-form))

;;; -----------------
;;; Utility Functions
;;; -----------------

;; TODO: This is sort of an interesting function to write because a
;; texture-map DSL form can default a lot of shit or also be some expert
;; written thing. I think I have to see if all the mipmap forms specify
;; extents, and then check that those extents are internally
;; self-consistent and correct. If they aren't specified in the mipmap
;; forms, then I have to read the assumed base mipmap layer image and
;; deduce the extents and number of mipmaps from that. It is possible
;; the appdev could write a legal thing I don't know how to check yet,
;; so keep an eye out for that.
(defgeneric deduce-mipmap-structure (texmap-inst &key core &allow-other-keys)
  (:documentation
   "Given a materialized TEXMAP-INST, deduce how many mipmaps it should have
and their individual extents. Sometimes, one needs to dig around in CORE
to find the information needed--example cube texture-maps because only
symbolic names are stored in certain representations of the cube.

Return four values:
 The first value is T if the deduced mipmaps are consistent and in good form
  and NIL otherwise.
 The second value is the model of the TEXMAP-INST.
 When the model is one of: :1d, :2d, :3d,
   The third value is a list of (x y z) decreasing extents whose length is
    equal to the number of mipmaps. Unused dimensions are set to 1.
 When the model is :cube,
  If the style is :envmap,
   The third value is a list of (x y z) decreasing extents whose length is
    equal to the number of mipmaps. Unused dimensions are set to 1.
  If the style is :faces,
   The third value is a list of exactly six (face ...) forms  where the ...
     represents a decreasing list of extents whose length is equal to the
     number of mipmaps for that face.
 The fourth value is a reason form if the first value is NIL."))

;; NOTE: 1d and 2d texmap-inst.
(defmethod deduce-mipmap-structure ((texmap-inst texture-map) &key core)
  (declare (ignore core))

  ;; Deduce the mipmap structure in progressivly more complex contexts:

  ;; Context 1: simple logical form
  ;; Observation:
  ;;  N data-elements
  ;;  N mipmap forms each with NIL extent
  ;;  Each mipmap has 1 mapping-span with NIL :from pointing to unique delem
  ;; Assume: 1 mipmap per data-element, in sorted order big to little.
  ;; Rectification: Get the mipmap sizes from the base layer image.

  ;; Context 2: simple physical form
  ;; Observation:
  ;;  N data-elements
  ;;  N mipmap forms each with specified extent
  ;; Assume: 1 mipmap per data-element, in sorted order big to little.

  (unless (texmap:materialized-p texmap-inst)
    (error "deduce-mipmap-structure: Unmaterialized 1d or 2d texmap!"))

  (let* ((delems (texmap:data-elements texmap-inst))
         ;; NOTE: We assume data-element 0 is the base level mipmap layer.
         (base-elem (aref delems 0))
         (base-image (rc:value (texmap:element base-elem)))
         (base-image-height (img:height base-image))
         (base-image-width (img:width base-image))
         (base-image-depth 1))

    (ecase (texmap:style texmap-inst)
      (:unique
       (u:compute-mipmap-levels base-image-width base-image-height
                                base-image-depth))
      (:combined
       (ecase (texmap:store texmap-inst)
         (:common
          ;; TODO: We don't actually CHECK the image, we just assume the
          ;; user told us the right thing.
          (u:compute-mipmap-levels
           (- base-image-width (floor (/ base-image-width 3)))
           base-image-height
           base-image-depth)))))))

;; TODO: Implement me!
(defmethod deduce-mipmap-structure ((texmap-inst texture-map-3d) &key core)
  (declare (ignore core))

  (unless (texmap:materialized-p texmap-inst)
    (error "deduce-mipmap-structure: Unmaterialized 3d texmap!"))
  )


;; TODO: Implement me:
(defmethod deduce-mipmap-structure ((texmap-inst texture-map-cube) &key core)
  (declare (ignore core))
  nil)
