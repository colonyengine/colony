(in-package :defpackage+-user-1)

(defpackage+ #:first-light.example
  (:nicknames #:fl.example)
  (:local-nicknames (#:u #:fl.util)
                    (#:m #:fl.math))
  (:use #:cl))
