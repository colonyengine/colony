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
              :annotation (v::material))))

(defun set-draw-method (render)
  (with-slots (%draw-method) render
    (let ((actor (v:actor render))
          (instances (v::instances (material render))))
      (setf %draw-method
            (case (mode render)
              (:mesh
               (lambda ()
                 (funcall (v::draw-func
                           (primitive (v:component-by-type actor 'mesh)))
                          instances)))
              (:geometry
               (lambda ()
                 (v::draw-geometry
                  (geometry (v:component-by-type actor 'geometry))
                  instances)))
              (:sprite
               (lambda ()
                 (draw-sprite
                  (v:component-by-type (v:actor render) 'sprite)
                  instances)))
              (otherwise
               ;; TODO: Sort of a debugging interface so I can replace above
               ;; components with ones specific to a project, or do a similar
               ;; analoge with an arbitrary component defined in a game without
               ;; having to add a new type here.
               ;;
               ;; This means to abstract away what the render has to do to
               ;; render something prolly needs more thought and refactoring.
               ;; For now it is a hack to help me be able to write a different
               ;; 'sprite' component than the one in Virality. If this isn't
               ;; here, then I'll call draw-sprite instead of the correct one
               ;; for my own sprite component.
               (destructuring-bind (extended-mode component &rest args)
                   (mode render)
                 (ecase extended-mode
                   (:explicit
                    (let ((draw-function (first args)))
                      (lambda ()
                        (funcall draw-function component instances))))))))))))

(defmethod v:on-component-initialize ((self render))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'transform))
    (set-draw-method self)))

(defmethod v:on-component-render ((self render))
  (a:when-let ((camera (v::active-camera (v:context self))))
    (v:with-material (material self)
        (:model (v:get-model-matrix self)
         :view (view camera)
         :proj (projection camera))
      (funcall (draw-method self)))))
