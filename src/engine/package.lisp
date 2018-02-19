(in-package :defpackage+-1)

(defpackage+ #:fl.core
    (:nicknames #:first-light)
  (:use #:cl)
  (:export #:start-engine
           #:quit-engine)

  ;; common
  (:export #:get-path
           #:destroy)

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

  ;; textures
  (:export #:load-texture)

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
           #:remove-component
           #:add-multiple-components
           #:initialize-component
           #:physics-update-component
           #:update-component
           #:render-component
           #:destroy-component
           #:actor-components-by-type
           #:actor-component-by-type))
