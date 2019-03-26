(in-package :defpackage+-user-1)

(defpackage+ #:first-light.prefab
  (:nicknames #:fl.prefab)
  (:local-nicknames (#:m #:game-math))
  (:use #:cl #:%first-light)
  (:export
   #:define-prefab
   #:find-prefab
   #:print-prefab
   #:load-prefabs
   #:ref))
