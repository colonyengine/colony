(in-package :defpackage+-1)

(defpackage+ #:fl.comp.tracking-camera
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera
            #:fl.comp.target-camera)
  (:export-only #:$tracking-camera))
