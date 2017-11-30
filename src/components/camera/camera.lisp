(in-package :first-light)

(define-component camera ()
  (activep nil)
  (view (mid))
  (projection (mid))
  (mode :perspective)
  (clip-near 0)
  (clip-far 1024)
  (zoom 2)
  (transform nil))

(defmethod initialize-component ((component camera) (context context))
  (with-accessors ((mode mode ) (actor actor) (transform transform)) component
    ;; compute a projection matrix according to the mode of this camera
    (make-projection mode component context)
    ;; store a reference to the transform component of this camera's actor locally
    (setf transform (actor-component-by-type actor 'transform))
    ;; register the camera in the list of core-state cameras
    (push component (cameras (core-state context)))))

(defmethod destroy-component :around ((component camera) (context context))
  ;; when we destroy a component that is a camera, also remove its reference in
  ;; the list of core-state cameras, and the context
  (call-next-method)
  (deletef (cameras (core-state context)) component)
  (setf (camera context) nil))

(defmethod make-projection ((mode (eql :perspective)) camera (context context))
  (with-accessors ((zoom zoom) (proj projection) (near clip-near) (far clip-far))
      camera
    (with-cfg (width height) context
      (mkpersp! proj (/ pi zoom) (/ width height) near far))))

(defmethod make-projection ((mode (eql :orthographic)) camera (context context))
  (with-accessors ((zoom zoom) (proj projection) (near clip-near) (far clip-far))
      camera
    (with-cfg (width height) context
      (let ((w (/ width (zoom camera) 2))
            (h (/ height (zoom camera) 2)))
        (mkortho! proj (- w) w (- h) h near far)))))

(defgeneric compute-camera-view (camera context)
  (:method ((camera camera) (context context))
    (with-accessors ((view view) (transform transform)) camera
      (let* ((eye (mtr->v (model transform)))
             (target (v+ eye (vneg (mrot->v (model transform) :z))))
             (up (mrot->v (model transform) :y)))
        (mkview! view eye target up)))))

(defun find-active-camera (core-state)
  (dolist (camera (cameras core-state))
    (when (activep camera)
      (return-from find-active-camera camera))))
