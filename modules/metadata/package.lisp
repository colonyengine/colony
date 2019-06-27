(in-package #:cl-user)

(defpackage #:first-light.metadata
  (:nicknames #:fl.data)
  (:local-nicknames (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:cl #:get #:set)
  (:export
   #:get
   #:set))
