(in-package #:colony.texture-map)

;;;; Cloning of DATA-ELEMENT
(defmethod clone:allocatablep ((object data-element))
  t)

(defmethod clone:clone-allocate ((object data-element) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object data-element)
                                     (original-object data-element)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  (u:when-slot-boundp original-object %original
    (setf (original cloned-object)
          (clone:clone-deep (original original-object)
                            eql-map)))

  (u:when-slot-boundp original-object %resolved
    (setf (resolved cloned-object)
          (clone:clone-deep (resolved original-object)
                            eql-map)))

  ;; TODO: What if this is a reference to something in the resource cache
  ;; (especially if it is shared with other data-elements)?  Even if I pass a
  ;; &key variable here to say if I wanted a deep copy or not, what should I do
  ;; with this?  The original name (likely an asset reference form) would stay
  ;; the same but I'd need a duplicate of the computed value.....
  (u:when-slot-boundp original-object %computed
    ;; TODO: For now, just catch the case. This will allow this code to work
    ;; for reification of metaspace texture-maps descriptors into core, but if
    ;; you try to deep clone a resolved texture-map, we'll ahve to figure it
    ;; out at that time what to do here.
    (error "TODO: Implement CLONE-OBJECT for a materialized texture-map")
    (setf (computed cloned-object)
          (clone:clone-deep (computed original-object)
                            eql-map)))
  cloned-object)

;;;; Cloning of DATA-STORE
(defmethod clone:allocatablep ((object data-store))
  t)

(defmethod clone:clone-allocate ((object data-store) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object data-store)
                                     (original-object data-store)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  (u:when-slot-boundp original-object %data-elements
    (setf (data-elements cloned-object)
          (clone::clone-deep (data-elements original-object) eql-map)))
  cloned-object)

;;;; Cloning of ATTRIBUTED-DATA-STORE

(defmethod clone:allocatablep ((object attributed-data-store))
  t)

(defmethod clone:clone-allocate ((object attributed-data-store) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object attributed-data-store)
                                     (original-object attributed-data-store)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

(defun test-clone-attributed-data-store ()
  (let ((x (texmap::make-data-store 'texmap::attributed-data-store 1))
        (de (texmap::make-data-element :original '(textures xxx))))
    (setf (abag:sattr x :foo) 42
          (abag:cattr x :foo) 42
          (aref (texmap::data-elements x) 0) de)
    (list x (clone::clone-deep x))))

;;;; Cloning of MIPMAP-STORE

(defmethod clone:allocatablep ((object mipmap-store))
  t)

(defmethod clone:clone-allocate ((object mipmap-store) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object mipmap-store)
                                     (original-object mipmap-store)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)


;;;; Cloning of BUFFER-NAME-STORE

(defmethod clone:allocatablep ((object buffer-name-store))
  t)

(defmethod clone:clone-allocate ((object buffer-name-store) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object buffer-name-store)
                                     (original-object buffer-name-store)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

;;;; Cloning of IMAGE-STORE

(defmethod clone:allocatablep ((object image-store))
  t)

(defmethod clone:clone-allocate ((object image-store) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object image-store)
                                     (original-object image-store)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

;;;; Cloning of CUBE-STORE

(defmethod clone:allocatablep ((object cube-store))
  t)

(defmethod clone:clone-allocate ((object cube-store) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object cube-store)
                                     (original-object cube-store)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  (u:when-slot-boundp original-object %style
    (setf (style cloned-object)
          (clone:clone-deep (style original-object) eql-map)))
  (u:when-slot-boundp original-object %store
    (setf (store cloned-object)
          (clone:clone-deep (store original-object) eql-map)))
  cloned-object)

;;;; Cloning of FACE-STORE

(defmethod clone:allocatablep ((object face-store))
  t)

(defmethod clone:clone-allocate ((object face-store) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object face-store)
                                     (original-object face-store)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

;;;; Cloning of TEXTURE-MAP

(defmethod clone:allocatablep ((object texture-map))
  t)

(defmethod clone:clone-allocate ((object texture-map) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object texture-map)
                                     (original-object texture-map)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  (u:when-slot-boundp original-object %name
    (setf (name cloned-object)
          (clone:clone-deep (name original-object) eql-map)))
  (u:when-slot-boundp original-object %anonymous-p
    (setf (anonymous-p cloned-object)
          (clone:clone-deep (anonymous-p original-object) eql-map)))
  (u:when-slot-boundp original-object %model
    (setf (model cloned-object)
          (clone:clone-deep (model original-object) eql-map)))
  (u:when-slot-boundp original-object %style
    (setf (style cloned-object)
          (clone:clone-deep (style original-object) eql-map)))
  (u:when-slot-boundp original-object %store
    (setf (store cloned-object)
          (clone:clone-deep (store original-object) eql-map)))
  (u:when-slot-boundp original-object %data-store
    (setf (data-store cloned-object)
          (clone:clone-deep (data-store original-object) eql-map)))
  cloned-object)

;;;; Cloning of TEXTURE-MAP-SINGLE

(defmethod clone:allocatablep ((object texture-map-single))
  t)

(defmethod clone:clone-allocate ((object texture-map-single) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object texture-map-single)
                                     (original-object texture-map-single)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

;;;; Cloning of TEXTURE-MAP-RECT

(defmethod clone:allocatablep ((object texture-map-rect))
  t)

(defmethod clone:clone-allocate ((object texture-map-rect) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object texture-map-rect)
                                     (original-object texture-map-rect)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

;;;; Cloning of TEXTURE-MAP-BUFFER

(defmethod clone:allocatablep ((object texture-map-buffer))
  t)

(defmethod clone:clone-allocate ((object texture-map-buffer) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object texture-map-buffer)
                                     (original-object texture-map-buffer)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

;;;; Cloning of TEXTURE-MAP-VOXEL

(defmethod clone:allocatablep ((object texture-map-voxel))
  t)

(defmethod clone:clone-allocate ((object texture-map-voxel) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object texture-map-voxel)
                                     (original-object texture-map-voxel)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

;;;; Cloning of TEXTURE-MAP-CUBE

(defmethod clone:allocatablep ((object texture-map-cube))
  t)

(defmethod clone:clone-allocate ((object texture-map-cube) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object texture-map-cube)
                                     (original-object texture-map-cube)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  ;; NOTE: This type currently has no slots to worry about.
  cloned-object)

;;;; Cloning of TEXTURE-MAP-DESCRIPTOR

(defmethod clone:allocatablep ((desc texture-map-descriptor))
  t)

(defmethod clone:clone-allocate ((object texture-map-descriptor) eql-map)
  (clone:eql-map-record eql-map object :allocation)
  (make-instance (type-of object)))

(defmethod clone:clone-object progn ((cloned-object texture-map-descriptor)
                                     (original-object texture-map-descriptor)
                                     (policy clone:deep-clone)
                                     (intention clone:graph-intention)
                                     (last-known-intention
                                      clone:no-specific-intention)
                                     eql-map &key)

  (u:when-slot-boundp original-object %name
    (setf (name cloned-object)
          (clone:clone-deep (name original-object) eql-map)))
  (u:when-slot-boundp original-object %ast
    (setf (ast cloned-object)
          (clone:clone-deep (ast original-object) eql-map)))
  (setf (astp cloned-object)
        (clone:clone-deep (astp original-object) eql-map))
  (u:when-slot-boundp original-object %extra-asts
    (setf (extra-asts cloned-object)
          (clone:clone-deep (extra-asts original-object) eql-map)))
  (setf (extra-asts-p cloned-object)
        (clone:clone-deep (extra-asts-p original-object) eql-map))
  (u:when-slot-boundp original-object %user-form
    (setf (user-form cloned-object)
          (clone:clone-deep (user-form original-object) eql-map)))
  (setf (user-form-p cloned-object)
        (clone:clone-deep (user-form-p original-object) eql-map))

  cloned-object)
