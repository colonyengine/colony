(in-package :fl.comp.camera)

(define-component camera ()
  (activep nil)
  (view (mid))
  (projection (mid))
  (mode :perspective)
  (clip-near .1)
  (clip-far 1024)
  (fovy (* 90 (/ pi 180)))
  (zoom 1)
  (transform nil))

(defmethod initialize-component ((component camera) (context context))
  (with-accessors ((mode mode) (actor actor) (transform transform)) component
    (make-projection mode component context)
    (setf transform (actor-component-by-type actor 'transform))
    (push component (cameras (core-state context)))))

(defmethod destroy-component ((component camera) (context context))
  (deletef (cameras (core-state context)) component)
  (setf (active-camera context) nil))

(defmethod make-projection ((mode (eql :perspective)) camera (context context))
  (with-accessors ((zoom zoom) (proj projection) (near clip-near)
                   (far clip-far) (fovy fovy))
      camera
    (with-cfg (width height) context
      (mkpersp! proj (/ fovy zoom) (/ width height) near far))))

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

(defun zoom-camera (display direction)
  (let* ((core-state (core-state display))
         (camera (find-active-camera core-state)))
    (with-accessors ((zoom zoom) (mode mode)) camera
      (setf zoom (clamp (+ zoom (/ direction 2)) 1 10))
      (make-projection mode camera (context core-state)))))
