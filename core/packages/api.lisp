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
                 #:destroy-component
                 #:extension-file-type
                 #:find-resource
                 #:frame-time
                 #:general-data-format-descriptor
                 #:id
                 #:initialize-component
                 #:input-data
                 #:make-actor
                 #:make-component
                 #:mat-uniform-ref
                 #:option
                 #:prepare-extension
                 #:print-all-resources
                 #:project-data
                 #:render-component
                 #:spawn-actor
                 #:ss-href
                 #:start-engine
                 #:stop-engine
                 #:total-time
                 #:update-component
                 #:using-material
                 #:with-shared-storage))
