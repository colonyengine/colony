(in-package :defpackage+-user-1)

(defpackage+ #:first-light.prefab
  (:nicknames #:fl.prefab)
  (:local-nicknames (#:u #:fl.util))
  (:use #:cl #:%first-light)
  (:export #:define-prefab))
