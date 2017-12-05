(in-package :defpackage+-1)

(defpackage+ #:fl.comp.mesh-renderer
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera
            #:fl.comp.mesh)
  (:export-only #:$mesh-renderer))
