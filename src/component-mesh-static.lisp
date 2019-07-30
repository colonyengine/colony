(in-package #:first-light.components)

(define-component static-mesh ()
  ((%location :reader location
              :initarg :location
              :initform nil)
   (%index :reader index
           :initarg :index
           :initform 0)
   (%data :reader data))
  ((:cached-mesh-data equalp eql)))

(defmethod on-component-initialize ((self static-mesh))
  (with-slots (%data) self
    (let ((location (a:ensure-list (location self)))
          (index (index self)))
      (unless location
        (error "A mesh component must have a location set."))
      (let ((path (apply #'find-resource (context self) location)))
        (with-shared-storage
            (context (context self))
            ((cached-mesh mesh-present-p
                          ('static-mesh :cached-mesh-data location index)
                          (%fl:load-static-geometry path index)))
          (setf %data cached-mesh))))))
