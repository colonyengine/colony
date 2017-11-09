(in-package :gear)

(defclass context ()
  ((%settings-table :reader settings-table
                    :initform (make-hash-table))
   (%shaders :accessor shaders
             :initform nil)
   (%camera :accessor camera
            :initform nil)))
