(in-package :defpackage+-1)

(defpackage+ #:fl.comp.mesh
  (:inherit #:fl.core)
  (:export-only #:$mesh
                #:write-buffer-data
                #:update-mesh-buffer
                #:make-vao
                #:vao
                #:load-mesh))
