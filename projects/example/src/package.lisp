(in-package :defpackage+-user-1)

(defpackage+ #:first-light-example
  (:use #:cl
        #:fl.core)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat)))
