(in-package :defpackage+-user-1)

(defpackage+ #:fl.core
  (:nicknames #:first-light)
  (:use #:cl)
  (:inherit #:au)
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
           #:lookup-material
           #:active-camera
           #:shaders
           #:shared-storage
           #:define-settings
           #:find-resource
           #:cfg
           #:with-cfg

           ;; and rcache API
           #:rcache
           #:rcache-lookup
           #:rcache-load
           #:rcahe-remove
           #:rcache-unload)

  ;; textures
  (:export #:load-texture)

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
           #:actor-component-by-type))
