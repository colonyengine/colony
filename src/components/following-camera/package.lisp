(in-package :defpackage+-1)

(defpackage+ #:fl.comp.following-camera
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera
            #:fl.comp.target-camera)
  (:export-only #:following-camera))
