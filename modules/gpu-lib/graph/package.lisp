(in-package :defpackage+-user-1)

(defpackage+ #:first-light.gpu.graph
  (:nicknames #:fl.gpu.graph)
  (:use #:fl.gpu.lib
        #:fl.gpu.swizzle)
  (:export
   #:graph))
