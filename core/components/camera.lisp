(in-package :first-light.components)

(define-component camera ()
  ((active-p :default nil)
   (view :default (m:mat4 1))
   (projection :default (m:mat4 1))
   (mode :default :perspective)
   (clip-near :default 0.1)
   (clip-far :default 1024)
   (fovy :default (* 90 (/ pi 180)))
   (zoom :default 1)
   (transform :default nil)))

(defun correct-camera-transform (camera)
  (with-accessors ((actor actor) (mode mode) (transform transform)) camera
    (when (m:zero-p (current (translation transform)))
      (let ((translation (ecase mode
                           (:orthographic (m:vec3 0 0 1))
                           (:perspective (m:vec3 0 0 50)))))
        (translate transform translation)
        (v:warn :fl.comp.camera
                "Camera ~a was attached to an actor without a translation ~
                 transform.~%~
                 Using a sane default value for ~(~a~): ~s."
                (id actor) mode (m:get-array translation))))))

(defmethod make-projection (camera (mode (eql :perspective)))
  (with-accessors ((context context) (zoom zoom) (proj projection) (near clip-near) (far clip-far)
                   (fovy fovy))
      camera
    (m:set-projection/perspective (/ fovy zoom)
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
      (m:set-projection/orthographic (- w) w (- h) h near far proj))))

(defun compute-camera-view (camera)
  (when (active-p camera)
    (let* ((model (model (transform camera)))
           (eye (m:get-translation model))
           (target (m:+ eye (m:negate (m:vec3 (m:get-column model 2)))))
           (up (m:vec3 (m:get-column model 1))))
      (m:set-view eye target up (view camera)))))

(defun find-active-camera (context)
  (dolist (camera (cameras (core-state context)))
    (when (active-p camera)
      (return-from find-active-camera camera))))

(defun zoom-camera (display direction)
  (let* ((context (context (core-state display)))
         (camera (find-active-camera context)))
    (with-accessors ((zoom zoom) (mode mode)) camera
      (setf zoom (u:clamp (+ zoom (/ direction 2)) 1 10))
      (make-projection mode camera))))

;;; Component event hooks

(defmethod on-component-initialize ((self camera))
  (with-accessors ((context context) (actor actor) (mode mode) (transform transform)) self
    (setf transform (actor-component-by-type actor 'transform))
    (correct-camera-transform self)
    (make-projection self mode)
    (push self (cameras (core-state context)))))

(defmethod on-component-destroy ((self camera))
  (with-accessors ((context context)) self
    (u:deletef (cameras (core-state context)) self)
    (setf (active-camera context) nil)))
