(in-package :defpackage+-1)

(defpackage+ #:fl.comp.following-camera
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera)
  (:export-only #:following-camera))
