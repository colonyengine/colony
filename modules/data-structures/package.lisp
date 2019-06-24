(in-package #:defpackage+-user-1)

(defpackage+ #:first-light.data-structures
  (:nicknames #:fl.dst)
  (:use #:cl)
  ;; queues
  (:inherit-from
   #:queues
   #:make-queue
   #:qpush
   #:qpop))
