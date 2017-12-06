(in-package :defpackage+-1)

(defpackage+ #:fl.core
  (:nicknames #:first-light)
  (:inherit #:cl #:alexandria #:gamebox-math)
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
           #:active-camera
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
