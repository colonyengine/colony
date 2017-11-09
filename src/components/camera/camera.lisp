(in-package :gear)

(define-component camera ()
  (activep nil)
  (view (mid))
  (projection (mid))
  (mode :perspective)
  (clip-near 0)
  (clip-far 1024)
  (zoom 1)
  (transform nil))

(defmethod initialize-component ((component camera) (context context))
  (make-projection (mode component) component context)
  (setf (transform component) (get-component 'transform (actor component))))

(defmethod compute-camera-view ((camera camera) (context context))
  (with-accessors ((view view) (transform transform)) camera
    (let* ((eye (mtr->v (model transform)))
           (target (v+ eye (vneg (mrot->v (model transform) :z))))
           (up (mrot->v (model transform) :y)))
      (mkview! view eye target up))))

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
