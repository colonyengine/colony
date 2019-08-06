(in-package #:cl-user)

(defpackage #:virality.prefabs
  (:use #:cl)
  (:export
   #:define-prefab
   #:define-prefab-descriptor
   #:find-prefab
   #:find-prefab-descriptor
   #:make-prefab-instance
   #:prefab-descriptor
   #:ref))
