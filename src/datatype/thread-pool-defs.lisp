(in-package #:virality.thread-pool)

(defclass thread-pool ()
  ((%kernel :accessor kernel
            :initarg :kernel)
   (%worker-count :reader worker-count
                  :initarg :worker-count)
   (%channels :reader channels
              :initform (u:dict #'eq))
   (%queues :reader queues
            :initform (u:dict #'eq))))
