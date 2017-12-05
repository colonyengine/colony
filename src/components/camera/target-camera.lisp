(in-package :fl.comp.target-camera)

(define-component $target-camera ()
  (slave-camera nil)
  (target-actor nil)
  (target-transform nil))

(defmethod initialize-component ((component $target-camera) (context context))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor))
      component
    (setf slave (actor-component-by-type actor '$camera))
    (camera-target-actor component target)))

(defmethod camera-target-actor ((camera $target-camera) (actor actor))
  (setf (target-actor camera) actor)
  (when actor
    (setf (target-transform camera)
          (actor-component-by-type actor '$transform))))
