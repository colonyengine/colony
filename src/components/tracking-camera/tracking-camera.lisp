(in-package :fl.comp.tracking-camera)

(define-component $tracking-camera ($target-camera))

(defmethod update-component ((component $tracking-camera) (context context))
  (with-accessors ((view view) (transform transform)) (slave-camera component)
    (let* ((eye (mtr->v (model transform)))
           (target (mtr->v (model (target-transform component))))
           (up (vec 0 1 0)))
      (mkview! view eye target up))))
