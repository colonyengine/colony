(in-package #:colony.component)

(c:define-component geometry ()
  ((%name :reader name
          :initarg :name
          :initform nil)
   (%geometry :reader geometry
              :initarg :geometry
              :initform nil)))

(defmethod c:on-component-initialize ((self geometry))
  (with-slots (%name %geometry) self
    (unless %name
      (error "A geometry component must have name specified."))
    (setf %geometry (c::make-geometry %name))))

(defmethod c:on-component-slave-render ((master render) (self geometry))
  (let ((instance-count (c::instances (comp:material master))))
    (c::draw-geometry (geometry self) instance-count)))
