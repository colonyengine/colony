(in-package :defpackage+-user-1)

(defpackage+ #:first-light.geometry
  (:nicknames #:fl.geom)
  (:local-nicknames (#:u #:fl.util))
  (:use #:cl)
  (:export #:load-gltf
           #:draw-func))
