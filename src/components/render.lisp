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
  (u:with-gensyms (material-ref)
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
  (u:when-let ((camera (v::active-camera (v:context self)))
               (slave (slave self))
               (material (material self)))
    (with-material material
        (:model (v:get-model-matrix self)
         :view (view camera)
         :proj (projection camera))

      ;; TODO: For now, if this uniform exists, set it up. We don't want to
      ;; unecessarily set this all the time, since it could compute work for a
      ;; lot of situations where we don't need it and as a general rule
      ;; materials might not have it. Another solution is that the user sets a
      ;; function for the normal-matrix and when it gets called it figures out
      ;; the actor and passes it in (along with the material). Need to think on
      ;; it a little bit more. Currently the function only knows of the context
      ;; and the material. Since materials are shared by different renderers,
      ;; materials can't have backreferences to their components/actors.
      (when (v:uniform-ref-p material :normal-matrix)
        (setf (v:uniform-ref material :normal-matrix)
              ;; TODO: Implement a m3:invert and reorganize this expressions
              ;; to reduce operations and not cons memory.
              (m4:rotation-to-mat3
               (m4:transpose (m4:invert
                              (m4:set-translation
                               (m4:* (view camera)
                                     (v:get-model-matrix self))
                               (v3:vec 0 0 0)))))))

      (v:on-component-slave-render self slave))))
