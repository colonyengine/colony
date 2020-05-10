(in-package #:virality.component)

(v:define-component render ()
  ((%mode :reader mode
          :initarg :mode
          :initform :mesh)
   (%draw-method :reader draw-method
                 :initform (constantly nil))
   (%transform :reader transform)
   (%material :accessor material
              :initarg :material
              :annotation (v::material))
   (%slave :reader slave
           :initarg :slave
           :initform nil)))

;; TODO: Make this constant time
(defmacro with-depth-function (material &body body)
  `(destructuring-bind (&key depth) (v::attributes ,material)
     (if depth
         (let ((old-depth (v::get-gpu-parameter :depth-func)))
           (unwind-protect
                (progn
                  (gl:depth-func depth)
                  ,@body)
             (gl:depth-func old-depth)))
         (progn ,@body))))

(defmacro with-material (material (&rest bindings) &body body)
  (a:with-gensyms (material-ref)
    `(let ((,material-ref ,material))
       (shadow:with-shader (v::shader ,material-ref)
         (setf ,@(loop :for (k v) :on bindings :by #'cddr
                       :collect `(v:uniform-ref ,material-ref ,k)
                       :collect v))
         (v::bind-material ,material-ref)
         (with-depth-function ,material-ref
           ,@body)))))

(defmethod v:on-component-initialize ((self render))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'transform))))

(defmethod v:on-component-render ((self render))
  (a:when-let ((camera (v::active-camera (v:context self)))
               (slave (slave self)))
    (with-material (material self)
        (:model (v:get-model-matrix self)
         :view (view camera)
         :proj (projection camera))
      (v:on-component-slave-render self slave))))
