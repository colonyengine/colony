(in-package :fl.core)

(define-component tracking-camera ()
  (slave-camera nil)
  (target-actor nil)
  (target-transform nil))

;; exported so others can use this to change what the camera is tracking.
(defun target-actor-with-tracking-camera (tracking-camera actor)
  (setf (target-actor tracking-camera) actor)
  (when actor
    (setf (target-transform tracking-camera)
          (actor-component-by-type actor 'transform))))

(defmethod initialize-component ((component tracking-camera) (context context))
  (setf (slave-camera component)
        (actor-component-by-type (actor component) 'camera))
  ;; Set up the target-transform too!
  (target-actor-with-tracking-camera component (target-actor component)))

(defmethod update-component ((component tracking-camera) (context context))
  (with-accessors ((view view) (transform transform))
      (slave-camera component)
    (let* ((eye (mtr->v (model transform)))
           (target (mtr->v (model (target-transform component))))
           (up (vec 0 1 0)))
      ;; Reset the slave-camera's view to look at intended target instead of
      ;; where it was previously looking.
      (mkview! view eye target up))))
