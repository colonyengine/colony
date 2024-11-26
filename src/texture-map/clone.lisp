(in-package #:colony.texture-map)

;; If anything in the texture-map-def.lisp data types must be cloned, put it
;; into this file.

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
  (u:when-slot-boundp original-object %anonymous-p
    (setf (anonymous-p cloned-object)
          (clone:clone-deep (anonymous-p original-object) eql-map)))
  (u:when-slot-boundp original-object %constructor
    (setf (constructor cloned-object)
          (clone:clone-deep (constructor original-object) eql-map)))
  (u:when-slot-boundp original-object %original-form
    (setf (original-form cloned-object)
          (clone:clone-deep (original-form original-object) eql-map)))
  cloned-object)
