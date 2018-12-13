(in-package :cl-user)

(defpackage #:first-light.metadata
  (:nicknames #:fl.data)
  (:use #:cl)
  (:shadow #:cl
           #:get
           #:set)
  (:export #:get
           #:set))
