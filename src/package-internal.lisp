(in-package #:cl-user)

(defpackage #:virality.engine
  (:use #:cl)
  ;; definitions
  (:import-from #:virality.prefabs
                #:define-prefab
                #:define-prefab-descriptor
                #:prefab-descriptor
                #:make-prefab-instance
                #:ref)
  (:export
   #:define-annotation
   #:define-call-flow
   #:define-component
   #:define-geometry
   #:define-geometry-layout
   #:define-graph
   #:define-material
   #:define-material-profile
   #:define-options
   #:define-prefab
   #:define-prefab-descriptor
   #:define-resources
   #:define-texture
   #:define-texture-profile)

  ;; prefab
  (:export #:ref)

  (:export
   #:prefab-descriptor
   #:make-prefab-instance
   #:delta
   #:copy-material
   #:attrs
   #:replace-action
   #:repeat-p
   #:action-step
   #:manager
   #:renderer
   #:on-action-insert
   #:on-action-finish
   #:on-action-update
   #:spawn-actor
   #:make-actor
   #:attach-component
   #:active-camera
   #:scene-tree
   #:frame-manager
   #:alpha
   #:with-material
   #:frame-count
   #:frame-time
   #:get-mouse-position
   #:destroy-after-time
   #:spawn-actor
   #:attach-components
   #:attach-multiple-components
   #:make-actor
   #:make-component
   #:input-enter-p
   #:frame-count
   #:input-data
   #:get-gamepad-analog
   #:option
   #:on-component-render
   #:on-component-attach
   #:on-component-detach
   #:on-component-destroy
   #:on-component-initialize
   #:on-component-update
   #:on-component-physics-update
   #:destroy
   #:display-id
   #:on-collision-enter
   #:on-collision-exit
   #:on-collision-continue
   #:meta
   #:total-time
   #:actor-component-by-type
   #:actor-components-by-type
   #:mat-uniform-ref
   #:frame-time
   #:context
   #:id
   #:actor
   #:with-shared-storage))

#++
(defpackage #:virality.engine
  (:use #:cl)
  (:export
   #:*core-debug*
   #:active-camera
   #:actor
   #:actor-component-by-type
   #:actor-components-by-type
   #:alpha
   #:attach-component
   #:attach-multiple-components
   #:cameras
   #:component
   #:compute-component-initargs
   #:context
   #:copy-material
   #:core
   #:define-annotation
   #:define-component
   #:define-graph
   #:define-material
   #:define-material-profile
   #:define-options
   #:define-resources
   #:define-texture
   #:define-texture-profile
   #:delta
   #:deploy-binary
   #:deregister-collider
   #:destroy
   #:destroy-after-time
   #:detach-component
   #:display-id
   #:find-actors-by-id
   #:find-by-uuid
   #:find-components-by-id
   #:find-resource
   #:frame-count
   #:frame-manager
   #:frame-time
   #:general-data-format-descriptor
   #:get-computed-component-precedence-list
   #:id
   #:input-data
   #:instances
   #:lookup-material
   #:make-actor
   #:make-component
   #:make-scene-tree
   #:mat-uniform-ref
   #:on-component-attach
   #:on-component-destroy
   #:on-component-detach
   #:on-component-initialize
   #:on-component-physics-update
   #:on-component-render
   #:on-component-update
   #:on-collision-enter
   #:on-collision-continue
   #:on-collision-exit
   #:option
   #:prefab-node
   #:print-all-resources
   #:project-data
   #:register-collider
   #:scene-tree
   #:shader
   #:shared-storage
   #:spawn-actor
   #:ss-href
   #:start-engine
   #:state
   #:stop-engine
   #:total-time
   #:ttl
   #:with-material
   #:with-shared-storage)

  ;; actions
  (:export
   #:on-action-insert
   #:on-action-update
   #:on-action-finish
   #:insert-action
   #:remove-action
   #:repeat-p
   #:replace-action
   #:manager
   #:renderer
   #:action-step
   #:attrs)

  ;; metadata
  (:export
   #:meta)

  ;; geometry
  (:export
   #:define-geometry-layout
   #:define-geometry
   #:draw-dynamic-geometry
   #:draw-static-geometry
   #:load-static-geometry
   #:make-dynamic-geometry
   #:update-dynamic-geometry)

  ;; image
  (:export
   #:channels
   #:data
   #:free-storage
   #:get-pixel-size
   #:height
   #:internal-format
   #:origin
   #:pixel-format
   #:pixel-type
   #:read-image
   #:width)

  ;; input
  (:export
   #:get-gamepad-analog
   #:get-gamepad-name
   #:get-mouse-position
   #:get-mouse-scroll
   #:handle-events
   #:input-enabled-p
   #:input-enter-p
   #:input-exit-p
   #:make-input-data
   #:prepare-gamepads
   #:shutdown-gamepads)
  )
