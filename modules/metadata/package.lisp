(in-package :defpackage+-user-1)

(defpackage+ #:first-light.metadata
  (:nicknames #:fl.data)
  (:use #:cl)
  (:shadow
   #:cl
   #:get
   #:set)
  (:export
   #:get
   #:set))
