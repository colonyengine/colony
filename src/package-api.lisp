(in-package :defpackage+-user-1)

(defpackage+ #:first-light
  (:nicknames #:fl)
  (:use #:cl)
  ;; common
  (:inherit-from #:%fl
                 #:cfg
                 #:define-settings
                 #:find-resource
                 #:id
                 #:start-engine
                 #:stop-engine
                 #:with-shared-storage)
  ;; extensions
  (:inherit-from #:%fl
                 #:extension-file-type
                 #:prepare-extension)
  ;; context
  (:inherit-from #:%fl
                 #:active-camera
                 #:context
                 #:delta
                 #:epilogue
                 #:frame-time
                 #:prologue
                 #:settings
                 #:ss-href
                 #:total-time)
  ;; textures
  (:inherit-from #:%fl
                 #:define-texture
                 #:define-texture-profile
                 #:general-data-format-descriptor)
  ;; materials
  (:inherit-from #:%fl
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
  (:inherit-from #:%fl
                 #:define-shader)
  ;; input
  (:inherit-from #:%fl
                 #:get-gamepad-axis
                 #:get-gamepad-description
                 #:get-mouse-position
                 #:get-mouse-scroll
                 #:input-enabled-p
                 #:input-enter-p
                 #:input-exit-p)
  ;; scene
  (:inherit-from #:%fl
                 #:define-scene)
  ;; actor
  ;; TODO: Finish user API.
  (:inherit-from #:%fl
                 #:actor
                 #:actor-component-by-type
                 #:actor-components-by-type)
  ;; components
  ;; TODO: Finish user API.
  (:inherit-from #:%fl
                 #:define-component
                 #:initialize-component
                 #:render-component
                 #:update-component))
