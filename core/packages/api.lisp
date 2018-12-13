(in-package :defpackage+-user-1)

(defpackage+ #:first-light
  (:nicknames #:fl)
  (:use #:cl)
  ;; Deplying binaries
  (:inherit-from #:%first-light
                 #:deploy-binary)
  ;; common
  (:inherit-from #:%first-light
                 #:cfg
                 #:define-settings
                 #:id
                 #:start-engine
                 #:stop-engine
                 #:with-shared-storage)
  ;; resources
  (:inherit-from #:%first-light
                 #:define-resources
                 #:find-resource
                 #:print-all-resources)
  ;; extensions
  (:inherit-from #:%first-light
                 #:extension-file-type
                 #:prepare-extension)
  ;; context
  (:inherit-from #:%first-light
                 #:active-camera
                 #:context
                 #:project-data
                 #:delta
                 #:epilogue
                 #:frame-time
                 #:input-data
                 #:prologue
                 #:settings
                 #:ss-href
                 #:total-time)
  ;; textures
  (:inherit-from #:%first-light
                 #:define-texture
                 #:define-texture-profile
                 #:general-data-format-descriptor)
  ;; materials
  (:inherit-from #:%first-light
                 #:bind-material
                 #:copy-material
                 #:define-material
                 #:define-material-profile
                 #:lookup-material
                 #:mat-computed-uniform-ref
                 #:mat-uniform-ref
                 #:shader
                 #:using-material)
  ;; shaders
  (:inherit-from #:%first-light
                 #:define-shader)
  ;; scene
  (:inherit-from #:%first-light
                 #:define-scene)
  ;; actor
  ;; TODO: Finish user API.
  (:inherit-from #:%first-light
                 #:actor
                 #:actor-component-by-type
                 #:actor-components-by-type
                 #:attach-component
                 #:attach-multiple-components
                 #:detach-component
                 #:make-actor
                 #:spawn-actor)
  ;; components
  ;; TODO: Finish user API.
  (:inherit-from #:%first-light
                 #:define-component
                 #:destroy-component
                 #:initialize-component
                 #:make-component
                 #:physics-update-component
                 #:render-component
                 #:shared-storage-metadata
                 #:update-component))
