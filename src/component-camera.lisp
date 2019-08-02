(in-package #:first-light.components)

(v:define-component camera ()
  ((%active-p :accessor active-p
              :initarg :active-p
              :initform nil)
   (%view :reader view
          :initform (m4:id))
   (%projection :reader projection
                :initform (m4:id))
   (%mode :reader mode
          :initarg :mode
          :initform :perspective)
   (%clip-near :reader clip-near
               :initarg :clip-near
               :initform 0.1)
   (%clip-far :reader clip-far
              :initarg :clip-far
              :initform 1024)
   (%fov-y :reader fov-y
           :initarg :fov-y
           :initform 90)
   (%zoom :accessor zoom
          :initarg :zoom
          :initform 1)
   (%transform :reader transform)))

(defun correct-camera-transform (camera)
  (when (v3:zero-p (current (translation (transform camera))))
    (let ((translation (ecase (mode camera)
                         (:orthographic (v3:vec 0 0 1))
                         (:perspective (v3:vec 0 0 50)))))
      (translate (transform camera) translation)
      (log:warn :changeme
                "Camera ~a was attached to an actor without a translation ~
                 transform.~%~
                 Using a sane default value for ~(~a~): ~s."
                (v:id (v:actor camera)) (mode camera) translation))))

(defmethod make-projection (camera (mode (eql :perspective)))
  (let ((context (v:context camera)))
    (m4:set-projection/perspective! (projection camera)
                                    (/ (fov-y camera) (zoom camera))
                                    (/ (v:option context :window-width)
                                       (v:option context :window-height))
                                    (clip-near camera)
                                    (clip-far camera))))

(defmethod make-projection (camera (mode (eql :orthographic)))
  (let* ((context (v:context camera))
         (zoom (zoom camera))
         (w (/ (v:option context :window-width) zoom 2))
         (h (/ (v:option context :window-height) zoom 2)))
    (m4:set-projection/orthographic!
     (projection camera) (- w) w (- h) h (clip-near camera) (clip-far camera))))

(defun compute-camera-view (camera)
  (when (active-p camera)
    (let* ((model (model (transform camera)))
           (eye (m4:get-translation model))
           (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 model :z))))
           (up (m4:rotation-axis-to-vec3 model :y)))
      (m4:set-view! (view camera) eye target up))))

(defun find-active-camera (context)
  (dolist (camera (v::cameras (v::core context)))
    (when (active-p camera)
      (return-from find-active-camera camera))))

(defun zoom-camera (display direction)
  (let* ((context (v:context (v::core display)))
         (camera (find-active-camera context)))
    (setf (zoom camera) (a:clamp (+ (zoom camera) (/ direction 2)) 1 10))
    (make-projection (mode camera) camera)))

;;; Component event hooks

(defmethod v:on-component-initialize ((self camera))
  (with-slots (%transform %fov-y) self
    (setf %transform (v:actor-component-by-type (v:actor self) 'transform)
          %fov-y (* %fov-y (/ pi 180)))
    (correct-camera-transform self)
    (make-projection self (mode self))
    (push self (v::cameras (v::core (v:context self))))))

(defmethod v:on-component-destroy ((self camera))
  (let ((context (v:context self)))
    (a:deletef (v::cameras (v::core context)) self)
    (setf (v:active-camera context) nil)))
