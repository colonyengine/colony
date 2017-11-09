(in-package :gear)

(%define-core-component camera ()
  (activep nil)
  (view (mid))
  (projection (mid))
  (mode :perspective)
  (clip-near 0)
  (clip-far 1024)
  (zoom 1)
  (look-at-actor nil)
  (look-at-transform nil)
  (transform nil))

(defmethod initialize-component ((component camera) context)
  (make-projection (mode component) component context)
  (setf (transform component) (get-component 'transform (actor component)))
  (when (look-at-actor component)
    (camera-look-at component (look-at-actor component))))

(defmethod compute-camera-view ((component camera) context)
  (with-accessors ((view view) (look-at-actor look-at-actor)
                   (look-at-transform look-at-transform)
                   (transform transform))
      component

    (if look-at-actor
        ;; LookAt the look-at regardless of the camera's actor's
        ;; transform orientation. (But the camera's actor position
        ;; matters).
        (let* ((eye (mtr->v (model transform)))
               (look-at (mtr->v (model look-at-transform)))
               (up-vec (vec 0 1 0)))
          (mkview! view eye look-at up-vec))

        ;; Simply look down the neg z axis of the camera's actor with the
        ;; actor's y pointing up.
        (let* ((eye (mtr->v (model transform)))
               (look-at (v+ eye (vneg (mrot->v (model transform) :z))))
               (up-vec (mrot->v (model transform) :y)))
          (mkview! view eye look-at up-vec)))))

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


(defun camera-look-at (camera-component look-at-actor)
  "Set LOOK-AT-ACTOR into the CAMERA-COMPONENT and make the camera
track that actor. To unset, set LOOK-AT-ACTOR to NIL when calling
function. That will restore the camera to its default configuration of
matching the associated actor's transform for its orientation."
  (setf (look-at-actor camera-component) look-at-actor

        (look-at-transform camera-component)
        (when look-at-actor
          (get-component 'transform look-at-actor))))
