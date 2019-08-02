(in-package #:cl-user)

(defpackage #:virality.contrib.actions
  (:use #:cl #:%first-light)
  (:export
   #:fade-in
   #:fade-out
   #:rotate
   #:rotate/reverse
   #:sprite-animate))
