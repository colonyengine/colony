(in-package :defpackage+-user-1)

(defpackage+ #:%first-light
  (:nicknames #:%fl)
  (:use #:cl)
  ;; common
  (:export #:cameras
           #:core-state
           #:define-options
           #:display
           #:frame-manager
           #:id
           #:option
           #:rcache
           #:rcache-construct
           #:rcache-dispose
           #:rcache-lookup
           #:rcache-remove
           #:scene-tree
           #:shared-storage
           #:start-engine
           #:stop-engine
           #:window
           #:with-shared-storage)
  ;; resources
  (:export #:define-resources
           #:find-resource
           #:print-all-resources)
  ;; extensions
  (:export #:extension-file-type
           #:prepare-extension)
  ;; context
  (:export #:active-camera
           #:context
           #:project-data
           #:delta
           #:epilogue
           #:frame-time
           #:input-data
           #:prologue
           #:shared-storage-table
           #:ss-href
           #:total-time)
  ;; textures
  (:export #:define-texture
           #:define-texture-profile
           #:general-data-format-descriptor
           #:load-texture)
  ;; materials
  (:export #:bind-material
           #:copy-material
           #:define-material
           #:define-material-profile
           #:lookup-material
           #:mat-computed-uniform-ref
           #:mat-uniform-ref
           #:shader
           #:using-material)
  ;; shaders
  (:export #:define-shader)
  ;; transform-state
  (:export #:make-transform-state
           #:transform-state-scalar
           #:transform-state-vector
           #:transform-state-quaternion
           #:current
           #:incremental
           #:incremental-delta
           #:previous
           #:interpolated
           #:interpolate-state)
  ;; scene
  (:export #:define-scene)
  ;; actor
  (:export #:actor
           #:attach-component
           #:attach-multiple-components
           #:detach-component
           #:make-actor
           #:spawn-actor)
  ;; components
  (:export #:actor-component-by-type
           #:actor-components-by-type
           #:component
           #:define-component
           #:destroy-component
           #:initialize-component
           #:make-component
           #:physics-update-component
           #:render-component
           #:shared-storage-metadata
           #:state
           #:update-component)
  ;; annotation API
  (:export #:define-annotation)
  ;; deploying binaries
  (:export #:deploy-binary))
