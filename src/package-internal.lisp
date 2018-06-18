(in-package :defpackage+-user-1)

(defpackage+ #:%first-light
  (:nicknames #:%fl)
  (:use #:cl)
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
           #:start-engine
           #:stop-engine
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
  (:export #:get-gamepad-axis
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
  (:use #:cl #:%fl)
  (:export-only #:transform
                #:model
                #:local
                #:add-child
                #:remove-child
                #:map-nodes
                #:transform-node
                #:interpolate-transforms))

(defpackage+ #:fl.comp.camera
  (:use #:cl #:%fl #:fl.comp.transform)
  (:export-only #:camera
                #:transform
                #:view
                #:projection
                #:zoom-camera
                #:activep
                #:find-active-camera
                #:compute-camera-view))

(defpackage+ #:fl.comp.following-camera
  (:use #:cl #:%fl #:fl.comp.transform #:fl.comp.camera)
  (:export-only #:following-camera))

(defpackage+ #:fl.comp.tracking-camera
  (:use #:cl #:%fl #:fl.comp.transform #:fl.comp.camera)
  (:export-only #:tracking-camera))

(defpackage+ #:fl.comp.mesh
  (:use #:cl #:%fl)
  (:export-only #:mesh
                #:primitives))

(defpackage+ #:fl.comp.mesh-renderer
  (:use #:cl #:%fl)
  (:export-only #:mesh-renderer
                #:material
                #:draw-mesh))

(defpackage+ #:fl.shaders
  (:use #:%fl #:shadow.lang)
  (:export-only #:unlit-color
                #:unlit-color-decal
                #:unlit-texture
                #:unlit-texture-decal))

(defpackage+ #:fl.materials
  (:use #:cl #:%fl)
  ;; helper functions
  (:export #:total-time/uniform)
  ;; profiles
  (:export #:u-model
           #:u-mvp
           #:u-mvpt
           #:u-proj
           #:u-time
           #:u-view
           #:u-vp
           #:u-vpt)
  ;; materials
  (:export #:missing-material
           #:unlit-color
           #:unlit-color-decal
           #:unlit-texture
           #:unlit-texture-decal
           #:unlit-texture-decal-bright))

(defpackage+ #:fl.textures
  (:use #:cl #:%fl)
  ;; profiles
  (:export #:default-profile
           #:clamp-all-edges)
  ;; textures
  (:export #:missing-texture
           #:debug-texture))
