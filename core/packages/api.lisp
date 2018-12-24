(in-package :defpackage+-user-1)

(defpackage+ #:first-light
  (:nicknames #:fl)
  (:use #:cl)
  (:inherit-from #:%first-light

                 ;; If you're the type that wants to explore the user interface
                 ;; to the first-light game engine without reading documentation
                 ;; (that may or may not exist), try doing something like this:
                 ;;
                 ;; For SLY users:
                 ;;
                 ;; M-x sly-describe-symbol<ret>fl:on-component-initialize<ret>
                 ;;
                 ;; For SLIME users:
                 ;;
                 ;; M-x slime-describe-symbol<ret>fl:on-component-initialize<ret>

                 #:active-camera
                 #:actor
                 #:actor-component-by-type
                 #:actor-components-by-type
                 #:attach-component
                 #:attach-multiple-components
                 #:context
                 #:copy-material
                 #:delta
                 #:deploy-binary
                 #:define-component
                 #:define-graph
                 #:define-material
                 #:define-material-profile
                 #:define-options
                 #:define-resources
                 #:define-scene
                 #:define-texture
                 #:define-texture-profile
                 #:detach-component
                 #:find-resource
                 #:frame-time
                 #:general-data-format-descriptor
                 #:id
                 #:input-data
                 #:make-actor
                 #:make-component
                 #:mat-uniform-ref
                 #:on-component-attach
                 #:on-component-destroy
                 #:on-component-detach
                 #:on-component-initialize
                 #:on-component-physics-update
                 #:on-component-render
                 #:on-component-update
                 #:option
                 #:print-all-resources
                 #:project-data
                 #:shared-storage
                 #:spawn-actor
                 #:ss-href
                 #:start-engine
                 #:stop-engine
                 #:total-time
                 #:using-material
                 #:with-shared-storage))
