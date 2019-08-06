(in-package #:cl-user)

(defpackage #:virality.actions
  (:use #:cl)
  (:shadow #:replace
           #:step)
  (:export
   #:attrs
   #:manager
   #:on-finish
   #:on-insert
   #:on-update
   #:renderer
   #:repeat-p
   #:replace
   #:step))
