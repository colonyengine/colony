(in-package :gear)

(defclass context ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%settings-table :reader settings-table
                    :initform (make-hash-table))
   (%shaders :accessor shaders
             :initform nil)
   (%camera :accessor camera
            :initform nil)))
