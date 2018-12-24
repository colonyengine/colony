(in-package :first-light.components)

(define-component camera ()
  ((active-p :default nil)
   (view :default (flm:mat4 1))
   (projection :default (flm:mat4 1))
   (mode :default :perspective)
   (clip-near :default 0.1)
   (clip-far :default 1024)
   (fovy :default (* 90 (/ pi 180)))
   (zoom :default 1)
   (transform :default nil)))

(defmethod on-component-initialize ((self camera))
  (with-accessors ((context context) (actor actor) (mode mode) (transform transform)) self
    (make-projection self mode)
    (setf transform (actor-component-by-type actor 'transform))
    (push self (cameras (core-state context)))))

(defmethod on-component-destroy ((self camera))
  (with-accessors ((context context)) self
    (fl.util:deletef (cameras (core-state context)) self)
    (setf (active-camera context) nil)))

(defmethod make-projection (camera (mode (eql :perspective)))
  (with-accessors ((context context) (zoom zoom) (proj projection) (near clip-near) (far clip-far)
                   (fovy fovy))
      camera
    (flm:set-projection/perspective (/ fovy zoom)
                                    (/ (option context :window-width)
                                       (option context :window-height))
                                    near
                                    far
                                    proj)))

(defmethod make-projection (camera (mode (eql :orthographic)))
  (with-accessors ((context context) (zoom zoom) (proj projection) (near clip-near) (far clip-far))
      camera
    (let ((w (/ (option context :window-width) zoom 2))
          (h (/ (option context :window-height) zoom 2)))
      (flm:set-projection/orthographic (- w) w (- h) h near far proj))))

(defun compute-camera-view (camera)
  (when (active-p camera)
    (let* ((model (model (transform camera)))
           (eye (flm:get-translation model))
           (target (flm:+ eye (flm:negate (flm:vec3 (flm:get-column model 2)))))
           (up (flm:vec3 (flm:get-column model 1))))
      (flm:set-view eye target up (view camera)))))

(defun find-active-camera (context)
  (dolist (camera (cameras (core-state context)))
    (when (active-p camera)
      (return-from find-active-camera camera))))

(defun zoom-camera (display direction)
  (let* ((context (context (core-state display)))
         (camera (find-active-camera context)))
    (with-accessors ((zoom zoom) (mode mode)) camera
      (setf zoom (fl.util:clamp (+ zoom (/ direction 2)) 1 10))
      (make-projection mode camera))))
