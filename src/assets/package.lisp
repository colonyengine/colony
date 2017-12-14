(in-package :defpackage+-1)

(defpackage+ #:fl.assets
  (:inherit #:cl
            #:alexandria
            #:parsley)
  (:export-only #:load-mesh
                #:draw-func))
