(in-package #:virality.components)

(v:define-component render ()
  ((%mode :reader mode
          :initarg :mode
          :initform :static-mesh)
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
            (ecase (mode render)
              (:static-mesh
               (lambda ()
                 (geo::draw-static-geometry
                  (data (v:component-by-type actor 'static-mesh))
                  instances)))
              (:dynamic-mesh
               (lambda ()
                 (geo::draw-dynamic-geometry
                  (geometry (v:component-by-type actor 'dynamic-mesh))
                  instances)))
              (:sprite
               (lambda ()
                 (draw-sprite
                  (v:component-by-type (v:actor render) 'sprite)
                  instances))))))))

(defmethod v:on-component-initialize ((self render))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'transform))
    (set-draw-method self)))

(defmethod v:on-component-render ((self render))
  (a:when-let ((camera (v::active-camera (v:context self))))
    (v:with-material (material self)
        (:model (model (transform self))
         :view (view camera)
         :proj (projection camera))
      (funcall (draw-method self)))))
