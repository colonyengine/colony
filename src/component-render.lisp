(in-package #:first-light.components)

(define-component render ()
  ((mesh :default nil)
   (mode :default :static-mesh)
   (draw-method :default (constantly nil))
   (transform :default nil)
   (material :default nil
             :annotation (fl.annotations:material))))

(defun set-draw-method (render)
  (with-accessors ((actor actor) (draw-method draw-method) (mode mode)
                   (material material))
      render
    (let ((instances (instances material)))
      (setf draw-method
            (ecase mode
              (:static-mesh
               (lambda ()
                 (%fl:draw-static-geometry
                  (data (actor-component-by-type actor 'static-mesh))
                  instances)))
              (:dynamic-mesh
               (lambda ()
                 (%fl:draw-dynamic-geometry
                  (geometry (actor-component-by-type actor 'dynamic-mesh))
                  instances)))
              (:sprite
               (lambda ()
                 (draw-sprite
                  (actor-component-by-type actor 'sprite)
                  instances))))))))

(defmethod on-component-initialize ((self render))
  (with-accessors ((actor actor) (transform transform)) self
    (setf transform (actor-component-by-type actor 'transform))
    (set-draw-method self)))

(defmethod on-component-render ((self render))
  (with-accessors ((context context) (transform transform)
                   (draw-method draw-method) (material material))
      self
    (a:when-let ((camera (active-camera context)))
      (using-material material
          (:model (fl.comp:model transform)
           :view (fl.comp:view camera)
           :proj (fl.comp:projection camera))
        (funcall draw-method)))))
