(in-package #:virality.components.render)

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
          (instances (mat::instances (material render))))
      (setf %draw-method
            (ecase (mode render)
              (:static-mesh
               (lambda ()
                 (geo::draw-static-geometry
                  (c/smesh::data
                   (v:component-by-type actor 'c/smesh:static-mesh))
                  instances)))
              (:dynamic-mesh
               (lambda ()
                 (geo::draw-dynamic-geometry
                  (c/dmesh::geometry
                   (v:component-by-type actor 'c/dmesh:dynamic-mesh))
                  instances)))
              (:sprite
               (lambda ()
                 (c/sprite::draw-sprite
                  (v:component-by-type (v:actor render) 'c/sprite:sprite)
                  instances))))))))

(defmethod v:on-component-initialize ((self render))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'c/xform:transform))
    (set-draw-method self)))

(defmethod v:on-component-render ((self render))
  (a:when-let ((camera (v::active-camera (v:context self))))
    (mat:with-material (material self)
        (:model (c/xform:model (transform self))
         :view (c/cam:view camera)
         :proj (c/cam:projection camera))
      (funcall (draw-method self)))))
