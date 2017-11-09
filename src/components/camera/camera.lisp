(in-package :gear)

(%define-core-component camera ()
  (activep nil)
  (view (mid))
  (projection (mid))
  (mode :perspective)
  (clip-near 0)
  (clip-far 1024)
  (zoom 1)
  (transform nil))

(defmethod initialize-component ((component camera) context)
  (setf (transform component)
	(get-component 'transform (actor component)))
  (make-projection (mode component) component context))

(defmethod update-component ((component camera) context))

(defmethod render-component ((component camera) context))

(defmethod compute-camera-view ((component camera) context)
  ;; Simply look down the neg z axis with y pointing up.
  (with-accessors ((view view) (transform transform)) component
    (let* ((eye (mtr->v (model transform)))
           (target-vec (vneg (mrot->v (model transform) :z)))
           (up-vec (mrot->v (model transform) :y)))
      (mkview! view eye target-vec up-vec))))

(defmethod make-projection ((mode (eql :perspective)) camera context)
  (with-accessors ((zoom zoom) (proj projection) (near clip-near) (far clip-far))
      camera
    (let ((w (cfg context :width))
          (h (cfg context :height)))
      (mkpersp! proj (/ pi zoom) (/ w h) near far))))

(defmethod make-projection ((mode (eql :orthographic)) camera context)
  (with-accessors ((zoom zoom) (proj projection) (near clip-near) (far clip-far))
      camera
    (let ((w (/ (cfg context :width) (zoom camera) 2))
          (h (/ (cfg context :height) (zoom camera) 2)))
      (mkortho! proj (- w) w (- h) h near far))))
