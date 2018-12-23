(in-package :defpackage+-user-1)

(defpackage+ #:first-light
  (:nicknames #:fl)
  (:use #:cl)
  (:inherit-from #:%first-light
                 #:active-camera
                 #:actor
                 #:actor-component-by-type
                 #:actor-components-by-type
                 #:attach-component
                 #:attach-multiple-components
                 #:context
                 #:copy-material
                 #:delta
                 #:deploy-binary
                 #:define-component
                 #:define-graph
                 #:define-material
                 #:define-material-profile
                 #:define-options
                 #:define-resources
                 #:define-scene
                 #:define-texture
                 #:define-texture-profile
                 #:detach-component
                 #:find-resource
                 #:frame-time
                 #:general-data-format-descriptor
                 #:id
                 #:input-data
                 #:make-actor
                 #:make-component
                 #:mat-uniform-ref
                 #:on-component-destroy
                 #:on-component-initialize
                 #:on-component-render
                 #:on-component-update
                 #:option
                 #:print-all-resources
                 #:project-data
                 #:spawn-actor
                 #:ss-href
                 #:start-engine
                 #:stop-engine
                 #:total-time
                 #:using-material
                 #:with-shared-storage))
