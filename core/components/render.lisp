(in-package :first-light.components)

(define-component render ()
  ((mesh :default nil)
   (mode :default :mesh)
   (draw-method :default (constantly nil))
   (transform :default nil)
   (material :default nil
             :annotation (fl.annotations:material))))

(defun set-draw-method (render)
  (with-accessors ((actor actor) (draw-method draw-method) (mode mode) (material material)) render
    (let ((instances (instances material))
          (mesh (actor-component-by-type actor 'mesh))
          (sprite (actor-component-by-type actor 'sprite)))
      (setf draw-method
            (ecase mode
              (:mesh (lambda () (draw-mesh mesh instances)))
              (:sprite (lambda () (draw-sprite sprite instances))))))))

(defmethod on-component-initialize ((self render))
  (with-accessors ((actor actor) (transform transform)) self
    (setf transform (actor-component-by-type actor 'transform))
    (set-draw-method self)))

(defmethod on-component-render ((self render))
  (with-accessors ((context context) (transform transform) (draw-method draw-method) (material material)) self
    (u:when-let ((camera (active-camera context)))
      (using-material material
          (:model (fl.comp:model transform)
           :view (fl.comp:view camera)
           :proj (fl.comp:projection camera))
        (funcall draw-method)))))
