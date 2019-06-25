(in-package #:cl-user)

(defpackage #:first-light.prefab
  (:nicknames #:fl.prefab)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils))
  (:use #:cl #:%first-light)
  (:export
   #:define-prefab
   #:define-prefab-descriptor
   #:find-prefab
   #:find-prefab-descriptor
   #:make-prefab-instance
   #:print-prefab
   #:ref))
