(in-package :defpackage+-user-1)

(defpackage+ #:first-light.components
  (:nicknames #:fl.comp)
  (:use #:cl #:%first-light)
  ;; camera
  (:export #:camera
           #:tracking-camera
           #:following-camera
           #:transform
           #:view
           #:projection
           #:zoom-camera
           #:find-active-camera
           #:compute-camera-view)
  ;; mesh
  (:export #:mesh)
  ;; mesh-renderer
  (:export #:mesh-renderer
           #:material
           #:draw-mesh)
  ;; transform
  (:export #:transform
           #:model
           #:local
           #:translate
           #:rotate
           #:scale))
