(in-package :defpackage+-1)

(defpackage+ #:fl.core
  (:nicknames #:first-light)
  (:use #:cl)
  (:use #:shadow #:vari)
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


(defpackage+ #:fl.shaders-new
  (:use #:cl #:shadow)
  (:inherit #:fl.core #:box.math.vari #:vari)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat)))
