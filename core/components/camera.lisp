(in-package #:first-light.components)

(define-component camera ()
  ((active-p :default nil)
   (view :default (m4:id))
   (projection :default (m4:id))
   (mode :default :perspective)
   (clip-near :default 0.1)
   (clip-far :default 1024)
   (fov-y :default 90)
   (zoom :default 1)
   (transform :default nil)))

(defun correct-camera-transform (camera)
  (with-accessors ((actor actor) (mode mode) (transform transform)) camera
    (when (v3:zero-p (current (translation transform)))
      (let ((translation (ecase mode
                           (:orthographic (v3:make 0 0 1))
                           (:perspective (v3:make 0 0 50)))))
        (translate transform translation)
        (v:warn :fl.comp.camera
                "Camera ~a was attached to an actor without a translation ~
                 transform.~%~
                 Using a sane default value for ~(~a~): ~s."
                (id actor) mode translation)))))

(defmethod make-projection (camera (mode (eql :perspective)))
  (with-accessors ((context context) (zoom zoom) (proj projection)
                   (near clip-near) (far clip-far) (fov-y fov-y))
      camera
    (m4:set-projection/perspective! proj
                                    (/ fov-y zoom)
                                    (/ (option context :window-width)
                                       (option context :window-height))
                                    near
                                    far)))

(defmethod make-projection (camera (mode (eql :orthographic)))
  (with-accessors ((context context) (zoom zoom) (proj projection)
                   (near clip-near) (far clip-far))
      camera
    (let ((w (/ (option context :window-width) zoom 2))
          (h (/ (option context :window-height) zoom 2)))
      (m4:set-projection/orthographic! proj (- w) w (- h) h near far))))

(defun compute-camera-view (camera)
  (when (active-p camera)
    (let* ((model (model (transform camera)))
           (eye (m4:get-translation model))
           (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 model :z))))
           (up (m4:rotation-axis-to-vec3 model :y)))
      (m4:set-view! (view camera) eye target up))))

(defun find-active-camera (context)
  (dolist (camera (cameras (core context)))
    (when (active-p camera)
      (return-from find-active-camera camera))))

(defun zoom-camera (display direction)
  (let* ((context (context (core display)))
         (camera (find-active-camera context)))
    (with-accessors ((zoom zoom) (mode mode)) camera
      (setf zoom (au:clamp (+ zoom (/ direction 2)) 1 10))
      (make-projection mode camera))))

;;; Component event hooks

(defmethod on-component-initialize ((self camera))
  (with-accessors ((context context) (actor actor) (mode mode)
                   (transform transform) (fov-y fov-y))
      self
    (setf transform (actor-component-by-type actor 'transform)
          fov-y (* fov-y (/ pi 180)))
    (correct-camera-transform self)
    (make-projection self mode)
    (push self (cameras (core context)))))

(defmethod on-component-destroy ((self camera))
  (with-accessors ((context context)) self
    (au:deletef (cameras (core context)) self)
    (setf (active-camera context) nil)))
