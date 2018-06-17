(in-package :defpackage+-user-1)

(defpackage+ #:%fl.core
  (:nicknames #:%fl)
  (:use #:cl)
  (:export #:start-engine
           #:stop-engine)
  ;; common
  (:export #:cameras
           #:cfg
           #:core-state
           #:define-settings
           #:display
           #:find-resource
           #:id
           #:make-core-state
           #:rcache
           #:rcache-construct
           #:rcache-dispose
           #:rcache-lookup
           #:rcache-remove
           #:shared-storage
           #:user-package
           #:window
           #:with-cfg
           #:with-shared-storage)
  ;; extensions
  (:export #:extension-file-type
           #:load-extensions
           #:prepare-extension)
  ;; context
  (:export #:active-camera
           #:context
           #:delta
           #:epilogue
           #:frame-time
           #:prologue
           #:settings
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
           #:shader)
  ;; shaders
  (:export #:define-shader)
  ;; input
  (:export #:gamepad-attached-p
           #:get-gamepad-axis
           #:get-gamepad-description
           #:get-mouse-position
           #:get-mouse-scroll
           #:input-enabled-p
           #:input-enter-p
           #:input-exit-p)
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
  ;; assets
  (:export #:load-mesh
           #:draw-func))

(defpackage+ #:fl.comp.transform
  (:use #:cl #:%fl.core)
  (:export-only #:transform
                #:model
                #:local
                #:add-child
                #:remove-child
                #:map-nodes
                #:transform-node
                #:interpolate-transforms))

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
                 #:shader)
  ;; shaders
  (:inherit-from #:%fl
                 #:define-shader)
  ;; input
  (:inherit-from #:%fl
                 #:gamepad-attached-p
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

(defpackage+ #:fl.comp.camera
  (:use #:cl #:%fl.core #:fl.comp.transform)
  (:export-only #:camera
                #:transform
                #:view
                #:projection
                #:zoom-camera
                #:activep
                #:find-active-camera
                #:compute-camera-view))

(defpackage+ #:fl.comp.following-camera
  (:use #:cl #:%fl.core #:fl.comp.transform #:fl.comp.camera)
  (:export-only #:following-camera))

(defpackage+ #:fl.comp.tracking-camera
  (:use #:cl #:%fl.core #:fl.comp.transform #:fl.comp.camera)
  (:export-only #:tracking-camera))

(defpackage+ #:fl.comp.mesh
  (:use #:cl #:%fl.core)
  (:export-only #:mesh
                #:primitives))

(defpackage+ #:fl.comp.mesh-renderer
  (:use #:cl #:%fl.core)
  (:export-only #:mesh-renderer
                #:material
                #:draw-mesh))

(defpackage+ #:fl.shaders
  (:use #:%fl.core #:shadow.lang)
  (:export-only #:unlit-color
                #:unlit-color-decal
                #:unlit-texture
                #:unlit-texture-decal))

(defpackage+ #:fl.materials
  (:use #:cl #:%fl.core)
  (:export-only
   ;; Material Profiles
   #:u-model
   #:u-view
   #:u-proj
   #:u-time
   #:u-mvp
   #:u-vp
   #:u-mvpt
   #:u-vpt
   ;; Materials
   #:missing-material
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal
   #:unlit-texture-decal-bright
   ;; Helper functions
   #:total-time/uniform))

(defpackage+ #:fl.textures
  (:use #:cl #:%fl.core)
  (:export-only
   ;; Texture Profiles
   #:default-profile
   #:clamp-all-edges
   ;; Textures
   #:missing-texture
   #:debug-texture))
