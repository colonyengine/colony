(in-package #:cl-user)

(defpackage #:virality.colliders
  (:use #:cl)
  (:export
   #:on-collision-enter
   #:on-collision-exit
   #:on-collision-continue
   #:collide-p))
