(in-package :defpackage+-1)

(defpackage+ #:fl.core
  (:nicknames #:first-light)
  (:use-only #:cl
             #:alexandria)
  (:export #:start-engine
           #:quit-engine)

  ;; common
  (:export #:get-path
           #:flatten-numbers)

  ;; core state
  (:export #:core-state
           #:make-core-state
           #:user-package
           #:display
           #:cameras
           #:context
           #:camera
           #:shaders
           #:shared-storage
           #:define-settings
           #:find-resource
           #:cfg
           #:with-cfg)

  ;; vertex-data
  (:export #:primitive
           #:buffer-indices
           #:get-vertex-layout)

  ;; input
  (:export #:key-down
           #:key-up)

  ;; scene
  (:export #:define-scene
           #:get-scene)

  ;; actor
  (:export #:actor
           #:id
           #:make-actor
           #:spawn-actor)

  ;; components
  (:export #:component
           #:state
           #:define-component
           #:make-component
           #:add-component
           #:add-multiple-components
           #:initialize-component
           #:physics-update-component
           #:update-component
           #:render-component
           #:destroy-component
           #:actor-components-by-type
           #:actor-component-by-type))

(defpackage+ #:fl.shader
  (:use-only #:3bgl-glsl/cl))

;;; component types

(defpackage+ #:fl.comp.transform
  (:use-only #:cl
             #:alexandria
             #:gamebox-math)
  (:inherit #:fl.core)
  (:export-only #:transform
                #:model
                #:local
                #:add-child
                #:map-nodes))

(defpackage+ #:fl.comp.basis
  (:use-only #:cl
             #:alexandria)
  (:inherit #:fl.core)
  (:export-only #:basis))

(defpackage+ #:fl.comp.camera
  (:use-only #:cl
             #:alexandria
             #:gamebox-math)
  (:inherit #:fl.core
            #:fl.comp.transform)
  (:export-only #:camera
                #:view
                #:projection
                #:compute-camera-view))

(defpackage+ #:fl.comp.tracking-camera
  (:use-only #:cl
             #:alexandria
             #:gamebox-math)
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera)
  (:export-only #:tracking-camera
                #:target-actor-with-tracking-camera))

(defpackage+ #:fl.comp.following-camera
  (:use-only #:cl
             #:alexandria
             #:gamebox-math)
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera)
  (:export-only #:following-camera
                #:target-actor-with-following-camera))

(defpackage+ #:fl.comp.mesh
  (:use-only #:cl
             #:alexandria)
  (:inherit #:fl.core)
  (:export-only #:mesh
                #:write-buffer-data
                #:update-mesh-buffer
                #:make-vao
                #:vao
                #:load-mesh))

(defpackage+ #:fl.comp.mesh-renderer
  (:use-only #:cl
             #:alexandria)
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera
            #:fl.comp.mesh)
  (:export-only #:mesh-renderer))

(defpackage+ #:fl.comp.tags
  (:use-only #:cl
             #:alexandria)
  (:inherit #:fl.core)
  (:export-only #:tags))
