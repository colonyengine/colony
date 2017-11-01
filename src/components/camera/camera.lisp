(in-package :gear)

(%define-core-component camera ()
  (view (mid))
  (projection (mid))
  (mode :perspective)
  (clip-near 0)
  (clip-far 1024)
  (zoom 1)
  (target nil)
  (transform nil))

(defmethod initialize-component ((component camera) context)
  (format t "Camera component: initializing~%")
  (setf (transform component) (get-component 'transform (actor component))))

;;; TODO: Think about what belongs in UPDATE-COMPONENT and RENDER-COMPONENT for
;;; the camera. We need to call MAKE-VIEW and MAKE-PROJECTION somewhere, but
;;; currently these matrices are not computed at all.

(defmethod update-component ((component camera) context)
  (format t "Camera component: updated~%"))

(defmethod render-component ((component camera) context)
  (format t "Camera component: rendered~%"))

(defun make-view (camera)
  (with-accessors ((view view) (target target) (transform transform)) camera
    (let* ((eye (v+ (mtr->v (local transform))
                    (mtr->v (model (transform target)))))
           (target-vec (v+ eye (vneg (mrot->v (model transform) :z))))
           (up-vec (mrot->v (model transform) :y)))
      (v->mtr! (model transform) eye)
      (mkview! view eye target-vec up-vec))))

(defmethod make-projection ((mode (eql :perspective)) camera context)
  (with-accessors ((zoom zoom) (proj projection) (near clip-near) (far clip-far))
      camera
    (let ((w (width context))
          (h (height context)))
      (mkpersp! proj (/ pi zoom) (/ w h) near far))))

(defmethod make-projection ((mode (eql :orthographic)) camera context)
  (with-accessors ((zoom zoom) (proj projection) (near clip-near) (far clip-far))
      camera
    (let ((w (/ (width context) (zoom camera) 2))
          (h (/ (height context) (zoom camera) 2)))
      (mkortho! proj (- w) w (- h) h near far))))
