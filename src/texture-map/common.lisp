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
(defun group-data-elements-by-resolution/combined (texmap-inst)
  "We get the data-elements from the materialized TEXMAP-INST for models
:1d, :2d, :3d/(:slices <any>), :cube/:envmap--but only for styles which
are :COMBINED. WE ASSUME the data-elements are in canonical form. Return
two values. The first value is a ((X Y Z) (elidxA ... elidxZ)) form that
describes the mipmap extents and participating DATA-ELEMENTS in each
extent or NIL if something went wrong. The second value is :OK if
nothing went wrong or a keyword symbol indicating the reason.

This function will put NIL for the (elidxA ... elidxZ) form if there
were no DATA-ELEMENTS to satisfy that resolution. This function will
return a complete set of mipmap resolutions meaning starting from the
biggest resolution, the final one will be (1 1 1)."

  ;; TODO

  t)


(defun group-data-elements-by-resolution/unique (texmap-inst)
  "We get the data-elements from the materialized TEXMAP-INST for models
:1d, :2d, :3d/(:slices <any>), :cube/:envmap--but only for styles which
are :UNIQUE. WE ASSUME the data-elements are in canonical form. Return
two values. The first value is a ((X Y Z) (elidxA ... elidxZ)) form that
describes the mipmap extents and participating DATA-ELEMENTS in each
extent or NIL if something went wrong. The second value is :OK if
nothing went wrong or a keyword symbol indicating the reason.

This function will put NIL for the (elidxA ... elidxZ) form if there
were no DATA-ELEMENTS to satisfy that resolution. This function will
return a complete set of mipmap resolutions meaning starting from the
biggest resolution, the final one will be (1 1 1)."

  ;; TODO: This code probably doesn't handle the case when data-elements
  ;; actually contains many (1 1 1) mipmaps!

  ;; TODO: This is clunky. Clean up later.
  (unless (or (member (texmap:model texmap-inst) '(:1d :2d))
              (and (eq (texmap:model texmap-inst) :3d)
                   (listp (texmap:store texmap-inst))
                   (eq (car (texmap:store texmap-inst)) :slices))
              (and (eq (texmap:model texmap-inst) :cube)
                   (eq (car texmap-inst) :envmap)))
    (return-from group-data-elements-by-resolution/unique
      (values NIL :unsupported-model-form)))

  ;; We're going to temporarily take advantage of the fact that all
  ;; image formats are actually 2 dimensions.
  (let ((delems (texmap:data-elements texmap-inst))
        ;; Key: (WIDTH HEIGHT)
        ;; Value: (idxA ... idxZ) [Need to reverse these when done.]
        (res-table (u:dict #'equal)))

    ;; NOTE: This loop makes an assumption that the data-elements are in
    ;; canonical form (sorted largest to smallest and in the case of :3d
    ;; (:slices <any>) the slices are in a contiguous group per mipmap
    ;; and also in the right slice order).
    (dotimes (idx (length delems))
      (let* ((delem (aref delems idx))
             (image (rc:value (texmap:element delem)))
             (width (img:width image))
             (height (img:height image)))
        (push idx (u:href res-table (list width height)))))

    ;; Get the available resolutions and sort ascendingly.
    (let* ((ascending-keys
             (sort (u:hash-table-keys res-table)
                   ;; Sort by area.
                   (lambda (left right)
                     (destructuring-bind (left-width left-height) left
                       (destructuring-bind (right-width right-height) right
                         (> (* left-width left-height)
                            (* right-width right-height)))))))

           ;; Build partial results given only what is in the data-elements.
           (delem-extents
             (mapcar
              (lambda (key)
                (let* ((participating-elems
                         (reverse (u:href res-table key)))
                       ;; TODO: This next line is a lie. For :3d models
                       ;; and :slices store forms we need to treat what
                       ;; is actually width/height/depth more
                       ;; accurately.
                       (depth (length participating-elems)))
                  (destructuring-bind (width height) key
                    `((,width ,height ,depth) ,participating-elems))))
              ascending-keys))
           (largest-delem-extent
             (caar delem-extents)))

      ;; Now, u:compute-mipmap-extents from the largest one and resolve
      ;; them together comparing the real resolutions found in the
      ;; delem-extents with the one from compute-mipmap-extents and the
      ;; adding in any addition extents down to (1 1 1) with NIL as the
      ;; elidxs as appropriate.
      (multiple-value-bind (num-levels computed-extents)
          (destructuring-bind (width height depth) largest-delem-extent
            (u:compute-mipmap-levels width height depth))

        ;; TODO: The relationship between num-levels and length
        ;; delem-extents needs meaning assigned.

        (flet ((resolve (dext cext)
                 ;; KEEP GOING: Resolve two possibly unequal in length
                 ;; lists together into one list that will represent all
                 ;; the mipmaps from largest to smallest.
                 nil))
          (u:ragged-mapcar :dne #'resolve delem-extents computed-extents))))))

(defgeneric deduce-mipmap-structure (texmap-inst &key core &allow-other-keys)
  (:documentation
   "Given a TEXMAP-INST with materialized data-elements, deduce how many
mipmaps it should have, their individual extents, and which data-elements
those mipmaps require from ONLY the information in the data-elements.

IT IS ASSUMED that this GF is only called during the synthesis phase of
rectification. This means the GF may assume that the data-elements are
ordered from largest to smallest in mipmap resolutions and that any
:STORE layouts are normalized. For :1d and :2d models, this is
intuitive, but for :3d models, depending on the :STORE form, the
function assumes whatever would have been the correct representation in
the data-elements array.

It is an error to use this GF during the validating phase of
rectification of a texture-map. This is because a texture-map in the
validation phase could have arrived there due to exact specification of
all data values which may legally break many assumptions made by this
GF. In this situation, this generic function will signal an error.

This GF will look in CORE to find information about cube texture-maps
when the :style is :faces. These face texture maps must already have
been successfully rectified before this function can use that data.

Return four values:
 The first value is T if the deduced mipmaps are consistent and in good form
 and NIL otherwise.

 The second value is the model of the TEXMAP-INST.

 The third value:
   when the model is :1d, :2d, :3d, or :cube with style :envmap,

     An EXTENTS list of ((x y z) (elidxA ... elidxZ)) entries that
     describe decreasing mipmap extents whose length is equal to the
     number of mipmaps. Unused dimensions are set to 1. (elidxA ...
     elidxZ) is a list of elidxes to the data-elements that participate
     in the extent of the mipmap at that resolution in the order
     required to union together into the extent. If there are no
     data-elements for a mipmap extent, then the list (elidxA ...
     elidxZ) is NIL. In the case of the :cube/:envmap model, these
     extents represent the ENTIRE size of the envmap at each mipmap
     level (i.e. not each face).

   when the model is :cube with style :faces,

     A list of exactly six (face DIR EXTENTS) forms where DIR represents
     the direction of the face (appropriate to the :store) and EXTENTS
     represents the same decreasing list of extents whose length is
     equal to the number of mipmaps for that face, and so forth as
     described above. This function will retrieve these values from the
     named texture-maps from CORE. Those texture maps must already have
     been successfully rectified before this call is made on the cube
     map.

 The fourth value is a reason keyword symbol if the first value is NIL."))

;; KEEP GOING: Implement below what I wrote above.

;; NOTE: 1d and 2d texmap-inst.
(defmethod deduce-mipmap-structure ((texmap-inst texture-map) &key core)
  (declare (ignore core))

  (let ((state (texmap:state texmap-inst)))
    (unless (eq (texmap:state state) :synthesize)
      (error "~A ~A ~S"
             "This GF may not be used during the validation phase of "
             "rectification for the texture-map: "
             texmap-inst)))

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
