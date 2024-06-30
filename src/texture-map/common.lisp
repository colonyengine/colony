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
(defmethod make-data-element ((type symbol) &key logloc physloc element)
  (make-instance type :logloc logloc :physloc physloc :element element))

;;; -----------------
;; defclass SPAN (and children) support code
;; NOTE: Types 'span-1d, 'span-2d, 'span-3d all take the same arguments, but of
;; different types
;;; -----------------
(defmethod make-span ((type symbol) &key origin extent)
  (make-instance type :origin origin :extent extent))

;;; -----------------
;; defclass DATA-SPAN (and children) support code
;; NOTE: Types 'data-span-1d, 'data-span-2d, 'data-span-3d all take the same
;; arguments, but of different types.
;;; -----------------
(defmethod make-data-span ((type symbol) &key origin extent elidx)
  (make-instance type :origin origin :extent extent :elidx elidx))

;;; -----------------
;; defclass MAPPING-SPAN (and children) support code
;; NOTE: Types 'mapping-span-1d, 'mapping-span-2d, 'mapping-span-3d all
;; take the same arguments, but of different types.
;;; -----------------
(defmethod make-mapping-span ((type symbol) &key to from)
  (make-instance type :to to :from from))

;;; -----------------
;; defclass STORAGE-FORM (and children) support code
;;; -----------------

;;; -----------------
;; defclass MIPMAP (and children) support code
;; NOTE: Types 'mipmap-1d, 'mipmap-2d, 'mipmap-3d all take the same arguments,
;; but of different types. So this is the general matcher for those types.
;;; -----------------
(defmethod make-storage-form ((type symbol) &key extent mapping-spans
                                              bags attrs cattrs sattrs)
  (let ((mipmap (make-instance type :extent extent
                                    :mapping-spans mapping-spans)))
    (abag:absorb mipmap :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs)
    mipmap))

;;; -----------------
;; defclass FACE (and children) support code
;; NOTE: Types 'mipmap-1d, 'mipmap-2d, 'mipmap-3d all take the same arguments,
;; but of different types. This is a specialized method.
;;; -----------------
(defmethod make-storage-form ((type (eql 'face))
                              &key elidx dir bags attrs cattrs sattrs)
  (let ((face (make-instance type :elidx elidx
                                  :dir dir)))
    (abag:absorb face :bags bags :attrs attrs :cattrs cattrs :sattrs sattrs)
    face))

;;; -----------------
;; defclass CUBE-REPRESENTATION (and children) support code
;; NOTE: Types 'faces and 'envmap take very different arguments and so must
;; be immediately specialized.
;;; -----------------

;;; -----------------
;; defclass FACE (and children) support code
;;; -----------------

(defmethod make-cube-representation ((type (eql 'faces)) &key faces)
  (unless (or (not faces)
              (subtypep (type-of faces) '(vector face)))
    (error "make-cube-representation: type: ~A The type of FACES is wrong."
           type))
  (make-instance type :faces faces))

;;; -----------------
;; defclass ENVMAP (and children) support code
;;; -----------------

(defmethod make-cube-representation ((type (eql 'envmap)) &key mipmaps)
  (unless (or (not mipmaps)
              (subtypep (type-of mipmaps) '(vector mipmap)))
    (error "make-cube-representation: type: ~A The type of MIPMAPS is wrong."
           type))
  (make-instance type :mipmaps mipmaps))

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

;;; -----------------

(defun make-texture-map-descriptor (name ast extra-asts user-form)
  (make-instance 'texture-map-descriptor
                 :name name
                 :ast ast
                 :astp (when ast)
                 :extra-asts extra-asts
                 :extra-asts-p (when extra-asts t)
                 :user-form user-form
                 :user-form-p (when user-form t)))










;;;; OLD STUFF BELOW -----------------------------------------------------

(defun make-texture-map (texture-map-type &rest init-args)
  (apply #'make-instance texture-map-type init-args))

(defun make-data-store (data-store-type num-elements &rest init-args)
  "A factory function that will produce the DATA-STORE-TYPE as tasked that
can contain NUM-ELEMENTS of elements. INIT-ARGS are the usual plist of
slot initargs and values."
  (apply #'make-instance data-store-type
         :data-elements (make-array num-elements)
         init-args))

(defun make-cube-face (&rest init-args)
  (apply #'make-instance 'cube-face init-args))

(defun make-data-element (&rest init-args)
  (apply #'make-instance 'data-element init-args))
