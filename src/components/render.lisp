(in-package #:colony.component)

(c:define-component render ()
  ((%mode :reader mode
          :initarg :mode
          :initform :mesh)
   (%draw-method :reader draw-method
                 :initform (constantly nil))
   (%transform :reader transform)
   (%material :accessor material
              :initarg :material
              :annotation (c::material))
   (%render-p :accessor render-p
              :initarg :render-p
              :initform t)
   (%slave :reader slave
           :initarg :slave
           :initform nil)))

;; TODO: Make this constant time
(defmacro with-depth-function (material &body body)
  `(destructuring-bind (&key depth) (c::attributes ,material)
     (if depth
         (let ((old-depth (c::get-gpu-parameter :depth-func)))
           (unwind-protect
                (progn
                  (gl:depth-func depth)
                  ,@body)
             (gl:depth-func old-depth)))
         (progn ,@body))))

(defmacro with-material (material (&rest bindings) &body body)
  "Bind uniforms in BINDINGS before evaluating the BODY.
If the uniform doesn't exist, silently ignore the setting of it.  NOTE: This
means if the BODY sets a uniform it will be IGNORED for this render, and if a
shared material may affect the NEXT rendering call!"
  (u:with-gensyms (material-ref)
    `(let ((,material-ref ,material))
       (shadow:with-shader (c::shader ,material-ref)
         ;; TODO: This behavior here implies a policy about uniform usage for
         ;; materials, in that, using a material that doesn't define this
         ;; uniform will silently ignore the issue if you try setting it.
         ;; The todo here is figure out how to get this better described so
         ;; we can do compile time checks on uniforms.
         (progn
           ,@(loop :for (k v) :on bindings :by #'cddr
                   :collect `(when (c:uniform-ref-p ,material-ref ,k)
                               (setf (c:uniform-ref ,material-ref ,k) ,v))))
         (c::bind-material ,material-ref)
         (with-depth-function ,material-ref
           ,@body)))))

(defmethod c:on-component-initialize ((self render))
  (with-slots (%transform) self
    (setf %transform (c:component-by-type (c:actor self) 'transform))))

(defmethod c:on-component-render ((self render))
  (when (render-p self)
    (u:when-let ((camera (c::active-camera (c:context self)))
                 (transform (c:component-by-type (c:actor self) 'transform)))
      (with-material (material self)
          (:model (model transform)
           :view (view camera)
           :proj (projection camera)
           :normal-matrix (resolve-normal-matrix transform))
        (c:on-component-slave-render self (slave self))))))
