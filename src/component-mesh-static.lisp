(in-package #:virality.components)

(v:define-component static-mesh ()
  ((%location :reader location
              :initarg :location
              :initform nil)
   (%index :reader index
           :initarg :index
           :initform 0)
   (%data :reader data))
  ((:cached-mesh-data equalp eql)))

(defmethod v:on-component-initialize ((self static-mesh))
  (with-slots (%data %index) self
    (let ((context (v:context self))
          (location (a:ensure-list (location self))))
      (unless location
        (error "A mesh component must have a location set."))
      (let ((path (apply #'v::find-resource context location)))
        (v:with-shared-storage
            (context context)
            ((cached-mesh mesh-present-p
                          ('static-mesh :cached-mesh-data location %index)
                          (v::load-static-geometry path %index)))
          (setf %data cached-mesh))))))
