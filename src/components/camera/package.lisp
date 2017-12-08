(in-package :defpackage+-1)

(defpackage+ #:fl.comp.camera
  (:inherit #:fl.core
            #:fl.comp.transform)
  (:export-only #:camera
                #:transform
                #:view
                #:projection
                #:zoom-camera
                #:compute-camera-view))
