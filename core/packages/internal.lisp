(in-package :defpackage+-user-1)

(defpackage+ #:%first-light
  (:nicknames #:%fl)
  (:use #:cl)
  (:export
   #:active-camera
   #:actor
   #:actor-component-by-type
   #:actor-components-by-type
   #:attach-component
   #:attach-multiple-components
   #:cameras
   #:component
   #:context
   #:copy-material
   #:core-state
   #:define-annotation
   #:define-component
   #:define-material
   #:define-material-profile
   #:define-options
   #:define-resources
   #:define-scene
   #:define-texture
   #:define-texture-profile
   #:delta
   #:deploy-binary
   #:destroy-component
   #:detach-component
   #:extension-file-type
   #:find-resource
   #:frame-manager
   #:frame-time
   #:general-data-format-descriptor
   #:initialize-component
   #:input-data
   #:lookup-material
   #:make-actor
   #:make-component
   #:mat-uniform-ref
   #:option
   #:prepare-extension
   #:print-all-resources
   #:project-data
   #:render-component
   #:scene-tree
   #:shader
   #:spawn-actor
   #:ss-href
   #:start-engine
   #:state
   #:stop-engine
   #:total-time
   #:update-component
   #:using-material
   #:with-shared-storage))
