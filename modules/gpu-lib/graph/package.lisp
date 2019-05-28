(in-package #:cl-user)

(defpackage #:first-light.gpu.graph
  (:nicknames #:fl.gpu.graph)
  (:use #:fl.gpu.lib
        #:fl.gpu.swizzle)
  (:export
   #:graph))
