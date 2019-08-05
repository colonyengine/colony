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
                  (comp.mesh.static::data
                   (v:component-by-type actor 'comp.mesh.static:static-mesh))
                  instances)))
              (:dynamic-mesh
               (lambda ()
                 (geo::draw-dynamic-geometry
                  (comp.mesh.dynamic::geometry
                   (v:component-by-type actor 'comp.mesh.dynamic:dynamic-mesh))
                  instances)))
              (:sprite
               (lambda ()
                 (comp.sprite::draw-sprite
                  (v:component-by-type (v:actor render) 'comp.sprite:sprite)
                  instances))))))))

(defmethod v:on-component-initialize ((self render))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'comp.transform:transform))
    (set-draw-method self)))

(defmethod v:on-component-render ((self render))
  (a:when-let ((camera (v::active-camera (v:context self))))
    (mat:with-material (material self)
        (:model (comp.transform:model (transform self))
         :view (comp.camera:view camera)
         :proj (comp.camera:projection camera))
      (funcall (draw-method self)))))
