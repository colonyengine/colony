(in-package :defpackage+-user-1)

(defpackage+ #:first-light.prefab
  (:nicknames #:fl.prefab)
  (:local-nicknames (#:m #:game-math))
  (:use #:cl #:%first-light)
  (:export
   #:define-prefab
   #:define-prefab-descriptor
   #:find-prefab
   #:find-prefab-descriptor
   #:make-prefab-instance
   #:print-prefab
   #:ref))
