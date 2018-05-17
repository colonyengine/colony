(in-package :defpackage+-user-1)

(defpackage+ #:fl.core
  (:nicknames #:first-light)
  (:use #:cl)
  (:export #:start-engine
           #:stop-engine)
  ;; core state
  (:export #:core-state
           #:make-core-state
           #:user-package
           #:display
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
  ;; context
  (:export #:context
           #:settings
           #:shaders
           #:shared-storage-table
           #:active-camera
           #:delta
           #:frame-time
           #:ss-href)
  ;; textures
  (:export #:load-texture
           #:define-texture-profile
           #:define-texture
           #:general-data-format-descriptor)
  ;; materials
  (:export #:define-material
           #:bind-material
           #:mat-ref
           #:mat-computed-ref
           #:shader)
  ;; input
  (:export #:key-down
           #:key-up
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

(defpackage+ #:fl.comp.transform
  (:use #:cl #:fl.core)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat))
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
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4))
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
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4))
  (:export-only #:following-camera))

(defpackage+ #:fl.comp.tracking-camera
  (:use #:cl #:fl.core #:fl.comp.transform #:fl.comp.camera)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4))
  (:export-only #:tracking-camera))

(defpackage+ #:fl.comp.mesh
  (:use #:cl #:fl.core)
  (:export-only #:mesh
                #:primitives))

(defpackage+ #:fl.comp.mesh-renderer
  (:use #:cl #:fl.core)
  (:export-only #:mesh-renderer
                #:draw-mesh))

(defpackage+ #:fl.shaders
  (:use #:cl #:shadow)
  (:inherit #:box.math.vari #:vari)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:export-only #:unlit-color
                #:unlit-color-decal
                #:unlit-texture
                #:unlit-texture-decal
                #:pbr-damaged-helmet
                #:easing-graph))

(defpackage+ #:fl.materials
  (:use #:cl #:shadow)
  (:inherit #:box.math.vari #:vari)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:import-from #:fl.core
                #:define-material)
  (:export-only #:missing-material
                #:unlit-color
                #:unlit-color-decal
                #:unlit-texture
                #:unlit-texture-decal
                #:unlit-texture-decal-bright
                #:pbr-damaged-helmet
                #:easing-graph))

(defpackage+ #:fl.textures
  (:use #:cl #:shadow)
  (:inherit #:box.math.vari #:vari)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:import-from #:fl.core
                #:define-texture-profile
                #:define-texture
                #:general-data-format-descriptor)
  (:export-only #:default-profile
                #:missing-texture
                #:debug-texture
                #:damaged-helmet/metallic-roughness
                #:damaged-helmet/color
                #:damaged-helmet/normal
                #:damaged-helmet/ambient-occlusion
                #:damaged-helmet/emissive))
