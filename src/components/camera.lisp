(in-package :fl.comp.camera)

(define-component camera ()
  ((activep :default nil)
   (view :default (m4:id))
   (projection :default (m4:id))
   (mode :default :perspective)
   (clip-near :default 0.1)
   (clip-far :default 1024)
   (fovy :default (* 90 (/ pi 180)))
   (zoom :default 1)
   (transform :default nil)))

(defmethod initialize-component ((component camera) (context context))
  (with-accessors ((mode mode) (actor actor) (transform transform)) component
    (make-projection mode component context)
    (setf transform (actor-component-by-type actor 'transform))
    (push component (cameras (core-state context)))))

(defmethod destroy-component ((component camera) (context context))
  (au:deletef (cameras (core-state context)) component)
  (setf (active-camera context) nil))

(defmethod make-projection ((mode (eql :perspective)) camera (context context))
  (with-accessors ((zoom zoom) (proj projection) (near clip-near) (far clip-far) (fovy fovy)) camera
    (with-cfg (width height) context
      (m4:perspective-projection! proj (/ fovy zoom) (/ width height) near far))))

(defmethod make-projection ((mode (eql :orthographic)) camera (context context))
  (with-accessors ((zoom zoom) (proj projection) (near clip-near) (far clip-far)) camera
    (with-cfg (width height) context
      (let ((w (/ width (zoom camera) 2))
            (h (/ height (zoom camera) 2)))
        (m4:orthographic-projection! proj (- w) w (- h) h near far)))))

(defgeneric compute-camera-view (camera context)
  (:method ((camera camera) (context context))
    (with-accessors ((view view) (transform transform)) camera
      (let* ((eye (m4:translation-to-vec3 (model transform)))
             (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 (model transform) :z))))
             (up (m4:rotation-axis-to-vec3 (model transform) :y)))
        (m4:view! view eye target up)))))

(defun find-active-camera (core-state)
  (dolist (camera (cameras core-state))
    (when (activep camera)
      (return-from find-active-camera camera))))

(defun zoom-camera (display direction)
  (let* ((core-state (core-state display))
         (camera (find-active-camera core-state)))
    (with-accessors ((zoom zoom) (mode mode)) camera
      (setf zoom (au:clamp (+ zoom (/ direction 2)) 1 10))
      (make-projection mode camera (context core-state)))))
