(in-package :cl-user)

(defpackage #:first-light
  (:use #:cl
        #:alexandria
        #:gamebox-math)

  ;; common
  (:export #:get-path
           #:start-engine
           #:quit-engine)

  ;; core state
  (:export #:make-core-state
           #:display
           #:context
           #:shaders
           #:shared-storage
           #:define-settings
           #:cfg
           #:with-cfg)

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

  ;; component
  (:export #:component
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
           #:actor-component-by-type)

;;; component types

  ;; basis
  (:export #:basis)

  ;; camera
  (:export #:camera
           #:view
           #:projection
           #:compute-camera-view)

  ;; tracking-camera
  (:export #:tracking-camera
           #:target-actor-with-tracking-camera)

  ;; mesh
  (:export #:mesh
           #:write-buffer-data
           #:update-mesh-buffer
           #:make-vao
           #:load-mesh)

  ;; mesh-renderer
  (:export #:mesh-renderer)

  ;; tags
  (:export #:tags)

  ;; transform
  (:export #:transform
           #:model
           #:local
           #:add-child
           #:map-nodes))

(defpackage #:first-light-shaders
  (:use #:3bgl-glsl/cl))
