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
(defmethod make-cube-representation ((type (eql 'faces)) &key faces)
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
(defmethod make-cube-representation ((type (eql 'envmap)) &key mipmaps)
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
;; defclass TEXTURE-MAP (and children) support code
;; NOTE: Factory for 'texture-map-1d, 'texture-map-2d, 'texture-map-3d as
;; the general symbol and a specialized method for 'texture-map-cube.
;;; -----------------

;; Extensible API
(defmethod make-texture-map ((type symbol)
                             &key name anonymous-p model style store
                               data-elements mipmaps bags attrs cattrs sattrs)
  ;; TYPE ends up being all the simple texture maps.
  (let ((texture-map
          (make-instance type
                         :name name :anonymous-p anonymous-p :model model
                         :style style :store store
                         :data-elements data-elements :mipmaps mipmaps)))
    (abag:absorb texture-map
                 :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs)
    texture-map))
(defmethod make-texture-map ((type (eql 'texture-map-cube))
                             &key name anonymous-p model style store
                               data-elements cube bags attrs cattrs sattrs)
  ;; TYPE is a complex texture map type.
  (let ((texture-map
          (make-instance type
                         :name name :anonymous-p anonymous-p :model model
                         :style style :store store
                         :data-elements data-elements :cube cube)))
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

(defun make-texture-map-descriptor (name anonymous-p constructor
                                    &optional original-form)
  (make-instance 'texture-map-descriptor
                 :name name
                 :anonymous-p anonymous-p
                 :constructor constructor
                 :original-form original-form))
