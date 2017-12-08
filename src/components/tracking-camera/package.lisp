(in-package :defpackage+-1)

(defpackage+ #:fl.comp.tracking-camera
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera)
  (:export-only #:tracking-camera))
