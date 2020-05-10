(in-package #:virality.component)

(v:define-component geometry ()
  ((%geometry :reader geometry
              :initarg :geometry
              :initform nil)))

(defmethod v:on-component-initialize ((self geometry))
  (with-slots (%geometry) self
    (unless %geometry
      (error "A geometry component must have geometry defined."))
    (setf %geometry (v::make-geometry %geometry))))

(defmethod v:on-component-slave-render ((master render) (self geometry))
  (let ((instance-count (v::instances (comp:material master))))
    (v::draw-geometry (geometry self) instance-count)))
