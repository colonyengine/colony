(in-package :defpackage+-1)

(defpackage+ #:fl.comp.camera
  (:inherit #:fl.core
            #:fl.comp.transform)
  (:export-only #:$camera
                #:transform
                #:view
                #:projection
                #:compute-camera-view))

(defpackage+ #:fl.comp.target-camera
  (:inherit #:fl.core
            #:fl.comp.transform)
  (:export-only #:$target-camera
                #:slave-camera
                #:target-actor
                #:target-transform
                #:camera-target-actor))
