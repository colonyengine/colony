(in-package #:cl-user)

(defpackage #:first-light.geometry
  (:nicknames #:fl.geom)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:load-gltf
   #:draw-func))
