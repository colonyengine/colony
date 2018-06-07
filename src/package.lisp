(in-package :defpackage+-user-1)

(defpackage+ #:fl.core
  (:use #:cl)
  (:export #:start-engine
           #:stop-engine)
  ;; core state
  (:export #:core-state
           #:make-core-state
           #:user-package
           #:display
           #:window
           #:cameras
           #:context
           #:lookup-material
           #:shared-storage
           #:define-settings
           #:find-resource
           #:cfg
           #:with-cfg
           #:with-shared-storage
           #:rcache
           #:rcache-lookup
           #:rcache-construct
           #:rcache-remove
           #:rcache-dispose)
  ;; extensions
  (:export #:extension-file-type
           #:prepare-extension
           #:load-extensions)
  ;; context
  (:export #:context
           #:settings
           #:shaders
           #:shared-storage-table
           #:active-camera
           #:delta
           #:frame-time
           #:total-time
           #:ss-href
           #:prologue
           #:epilogue)
  ;; textures
  (:export #:load-texture
           #:define-texture-profile
           #:define-texture
           #:general-data-format-descriptor)
  ;; materials
  (:export #:define-material
           #:define-material-profile
           #:copy-material
           #:bind-material
           #:mat-uniform-ref
           #:mat-computed-uniform-ref
           #:shader
           )
  ;; shaders
  (:export #:define-shader)
  ;; input
  (:export #:key-down
           #:key-up
           #:mouse-down-left
           #:mouse-down-middle
           #:mouse-down-right
           #:mouse-up-left
           #:mouse-up-middle
           #:mouse-up-right
           #:mouse-scroll-up
           #:mouse-scroll-down)
  ;; scene
  (:export #:define-scene
           #:get-scene)
  ;; actor
  (:export #:actor
           #:id
           #:make-actor
           #:spawn-actor
           #:attach-component
           #:attach-multiple-components
           #:detach-component)
  ;; components
  (:export #:component
           #:state
           #:define-component
           #:make-component
           #:initialize-component
           #:physics-update-component
           #:update-component
           #:render-component
           #:destroy-component
           #:actor-components-by-type
           #:actor-component-by-type
           #:shared-storage-metadata)
  ;; assets
  (:export #:load-mesh
           #:draw-func))

(defpackage+ #:fl.host
  (:use #:cl #:fl.core)
  (:export #:initialize-host
           #:shutdown-host
           #:create-window
           #:create-opengl-context
           #:close-window
           #:get-refresh-rate
           #:redraw-window
           #:set-draw-mode
           #:get-window-title
           #:set-window-title
           #:get-window-size
           #:set-window-size
           #:get-window-mode
           #:set-window-mode
           #:set-window-hidden
           #:set-window-visible
           #:set-cursor-hidden
           #:set-cursor-visible
           #:handle-events
           #:get-key-name))

(defpackage+ #:fl.comp.transform
  (:use #:cl #:fl.core)
  (:export-only #:transform
                #:model
                #:local
                #:add-child
                #:remove-child
                #:map-nodes
                #:transform-node
                #:interpolate-transforms))

(defpackage+ #:fl.comp.camera
  (:use #:cl #:fl.core #:fl.comp.transform)
  (:export-only #:camera
                #:transform
                #:view
                #:projection
                #:zoom-camera
                #:activep
                #:find-active-camera
                #:compute-camera-view))

(defpackage+ #:fl.comp.following-camera
  (:use #:cl #:fl.core #:fl.comp.transform #:fl.comp.camera)
  (:export-only #:following-camera))

(defpackage+ #:fl.comp.tracking-camera
  (:use #:cl #:fl.core #:fl.comp.transform #:fl.comp.camera)
  (:export-only #:tracking-camera))

(defpackage+ #:fl.comp.mesh
  (:use #:cl #:fl.core)
  (:export-only #:mesh
                #:primitives))

(defpackage+ #:fl.comp.mesh-renderer
  (:use #:cl #:fl.core)
  (:export-only #:mesh-renderer
                #:material
                #:draw-mesh))

(defpackage+ #:fl.shaders
  (:use #:fl.core #:shadow.lang)
  (:export-only #:unlit-color
                #:unlit-color-decal
                #:unlit-texture
                #:unlit-texture-decal))

(defpackage+ #:fl.materials
  (:use #:cl #:fl.core)
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
  (:use #:cl #:fl.core)
  (:export-only
   ;; Texture Profiles
   #:default-profile
   #:clamp-all-edges
   ;; Textures
   #:missing-texture
   #:debug-texture))
