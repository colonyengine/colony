(in-package #:cl-user)

(defpackage #:virality.clone
  (:use #:cl)
  ;; CLONE-POLICY Classes
  (:export
   #:allocating-clone
   #:clone-policy
   #:deep-clone
   #:identity-clone
   #:shallow-clone
   )
  ;; INTENTION Classes and API
  (:export
   #:alist-intention
   #:compare-intention
   #:cons-intention
   #:graph-intention
   #:intention
   #:intention/=
   #:intention<
   #:intention<=
   #:intention=
   #:intention>
   #:intention>=
   #:list-intention
   #:make-alist-intention
   #:make-cons-intention
   #:make-graph-intention
   #:make-list-intention
   #:make-no-specific-intention
   #:no-specific-intention
   )
  ;; EQL-MAP & EQL-MAP-ENTRY Classes and API
  ;; TODO: Maybe we can export less around the slot symbols?
  (:export
   #:entry-table
   #:eql-map
   #:eql-map-dump
   #:eql-map-dump-stats
   #:eql-map-entry
   #:eql-map-get-stats-domain
   #:eql-map-map-stats-match-p
   #:eql-map-mark-target
   #:eql-map-mark-visited
   #:eql-map-record
   #:eql-map-ref
   #:eql-map-visited-p
   #:intent
   #:make-eql-map
   #:make-eql-map-entry
   #:make-eql-map-with-stats
   #:origin
   #:stats
   #:target
   #:transition-p
   #:validate-eql-map-stats
   )
  ;; CLONE API (including shortcut API)
  (:export
   #:allocatablep
   #:clone
   #:clone-allocate
   #:clone-deep
   #:clone-identity
   #:clone-object
   #:clone-shallow
   #:clone-shallow-alist
   #:clone-shallow-cons
   #:clone-shallow-graph
   #:clone-shallow-list
   #:make-deep-clone
   #:make-identity-clone
   #:make-shallow-clone
   )
  )

(defpackage #:virality.attribute-bag
  (:use #:cl)
  ;; ATTRIBUTE-VALUE API
  (:export
   #:attribute-value
   #:computed
   #:computed-value-bound-p
   #:copy-attribute-value
   #:dirty
   #:make-attribute-value
   #:semantic
   #:semantic-value-bound-p
   )
  ;; ATTRIBUTE-BAG API
  (:export
   #:attr
   #:attribute-bag
   #:cattr
   #:clear-attrs
   #:do-attr
   #:do-cattr
   #:do-sattr
   #:dump-attribute-bag
   #:make-attribute-bag
   #:overlay
   #:sattr
   ))

(defpackage #:virality.image
  (:use #:cl)
  ;; IMAGE API
  (:export
   #:data
   #:height
   #:internal-format
   #:load-image
   #:pixel-format
   #:pixel-type
   #:width
   ))

(defpackage #:virality.resource-cache
  (:use #:cl)
  ;; CACHE-ITEM API
  (:export
   #:cache-item
   #:location
   #:make-cache-item
   #:policy
   #:size
   #:tag
   #:value
   )
  ;; CACHE-DOMAIN API
  (:export
   #:cache
   #:cache-domain
   #:cdref
   #:cdrem
   #:did
   #:hits
   #:inserts
   #:layout
   #:make-cache-domain
   #:misses
   #:removes
   )
  ;; RESOURCE-CACHE API
  (:export
   #:domains
   #:ensure-cache-domain
   #:make-resource-cache
   #:rcref
   #:rcrefd
   #:rcrem
   #:resource-cache
   )
  ;; CACHING-TASK API
  (:export
   #:caching-task
   #:core
   #:domain-id
   #:if-exists
   #:if-not-exits
   #:key
   #:opaque-data
   #:statep
   #:value
   )
  ;; RESOURCE-CACHE-SCHEDULER API
  (:export
   #:core
   #:make-resource-cache-scheduler
   #:schedule
   #:unscheduled-tasks
   )
  ;; The Cache Warming Protocol (move to a better protocol definition place).
  (:export
   #:acquire-caching-task
   #:compute-caching-task-value
   #:finalize-caching-task
   #:init-caching-task
   #:release-caching-task
   #:reserve-or-discard-caching-task-p
   )
  ;; RESOURCE-CACHE-EXECUTOR API
  (:export
   #:concurrent-resource-cache-executor
   #:execute
   #:make-concurrent-resource-cache-executor
   #:make-resource-cache-executor
   #:make-sequential-resource-cache-executor
   #:resource-cache-executor
   #:sequential-resource-cache-executor
   ))

(defpackage #:virality.uuid
  (:use #:cl)
  ;; uuid defstruct
  (:export
   #:uuid
   #:high
   #:low
   #:variant
   #:version
   )
  ;; uuid API
  (:export
   #:make-uuid
   #:string->uuid
   #:uuid->string
   #:valid-string-p
   ))

(defpackage #:virality.thread-pool
  (:use #:cl)
  ;; thread-pool
  (:export
   #:channels
   #:dequeue
   #:destroy-thread-pool
   #:enqueue
   #:ensure-channel
   #:ensure-queue
   #:get-job-results
   #:handle-queued-event
   #:kernel
   #:kill-jobs
   #:make-thread-pool
   #:pop-queue
   #:process-queue
   #:push-queue
   #:queue-empty-p
   #:queues
   #:submit-job
   #:thread-pool
   #:worker-count
   ))

(defpackage #:virality.texture-map
  (:use #:cl)
  ;; The macro API
  (:export
   #:define-texture-map
   )
  ;; texture-map-descriptor API
  (:export
   #:ast
   #:extra-asts
   #:make-texture-map-descriptor
   #:name
   #:texture-map-descriptor
   #:user-form
   ))

(defpackage #:virality.texture-map.texture-map-table
  (:use #:cl)
  ;; texture-map-table API
  (:export
   #:add-semantic-texture-map-descriptor
   #:find-semantic-texture-map-descriptor
   #:make-texture-map-table
   #:remove-semantic-texture-map-descriptor
   #:semantic-texture-map-descriptors
   #:texture-map-table
   ))

(defpackage #:virality.texture
  (:use #:cl)
  ;; texture-descriptor
  (:export
   #:applied-attributes
   #:attributes
   #:copy-texture-descriptor
   #:make-texture-descriptor
   #:name
   #:profile-overlay-names
   #:texture-descriptor
   #:texture-type)
  ;; texture-profile
  (:export
   #:attributes
   #:define-texture-profile
   #:make-texture-profile
   #:name
   #:parse-texture-profile
   #:texture-profile)
  ;; texture
  (:export
   #:computed-texdesc
   #:define-texture
   #:name
   #:semantic-texdesc
   #:texid
   #:texture
   #:reify-texture-profiles
   #:reify-texture-descriptors))

(defpackage #:virality.texture.texture-table
  (:use #:cl)
  (:export
   #:add-semantic-texture-descriptor
   #:add-texture-profile
   #:add-unrealized-texture
   #:find-semantic-texture-descriptor
   #:get-procedural-texture-descriptors
   #:get-unrealized-textures
   #:make-texture-table
   #:profiles
   #:remove-semantic-texture-descriptor
   #:remove-texture-profile
   #:remove-unrealized-texture
   #:semantic-texture-descriptors
   #:texture-table
   #:unrealized-procedural-textures))

(defpackage #:virality.prefab
  (:use #:cl)
  (:export
   #:define-prefab
   #:find-prefab
   #:make-prefab-instance
   #:ref))

(uiop:define-package #:virality.shader
  (:use-reexport
   #:vumbra.common
   #:vshadow.glsl)
  ;; shaders
  (:export
   #:collider/cuboid
   #:collider/sphere
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal
   #:unlit-texture-invert
   #:unlit/vert
   #:unlit/vert-nil
   #:unlit/vert-only-uv1
   #:matcap))

(defpackage #:virality.extension
  (:use #:cl)
  ;; geometry layouts
  (:export
   #:2d)
  ;; materials
  (:export
   #:collider/cuboid
   #:collider/sphere
   #:matcap
   #:missing-material
   #:sprite
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal
   #:unlit-texture-invert
   #:unlit-texture-decal-bright)
  ;; material profiles
  (:export
   #:u-model
   #:u-mvp
   #:u-mvpt
   #:u-mvptr
   #:u-proj
   #:u-time
   #:u-view
   #:u-vp
   #:u-vpt)
  ;; textures
  (:export
   #:debug-texture
   #:matcap/basic-1
   #:matcap/basic-2
   #:matcap/basic-dark
   #:matcap/basic-side
   #:matcap/ceramic-dark
   #:matcap/ceramic-lightbulb
   #:matcap/check-normal-y
   #:matcap/check-rim-dark
   #:matcap/check-rim-light
   #:matcap/clay-brown
   #:matcap/clay-muddy
   #:matcap/clay-studio
   #:matcap/jade
   #:matcap/metal-anisotropic
   #:matcap/metal-carpaint
   #:matcap/metal-lead
   #:matcap/metal-shiny
   #:matcap/pearl
   #:matcap/reflection-check-horizontal
   #:matcap/reflection-check-vertical
   #:matcap/resin
   #:matcap/skin
   #:matcap/toon)
  ;; texture profiles
  (:export
   #:clamp-all-edges
   #:default-profile))

(defpackage #:virality.component
  (:use #:cl)
  ;; camera
  (:export
   #:active-p
   #:camera
   #:compute-camera-view
   #:find-active-camera
   #:following-camera
   #:projection
   #:tracking-camera
   #:transform
   #:view
   #:zoom-camera)
  ;; collider
  (:export
   #:collide-p
   #:on-layer
   #:referent
   #:sphere
   #:cuboid)
  ;; font
  (:export
   #:font)
  ;; geometry
  (:export
   #:geometry)
  ;; mesh
  (:export
   #:mesh)
  ;; render
  (:export
   #:material
   #:render
   #:render-p)
  ;; sprite
  (:export
   #:frames
   #:name
   #:duration
   #:repeat
   #:sprite)
  ;; transform
  (:export
   #:add-child
   #:local
   #:map-actors
   #:model
   #:remove-child
   #:transform))

(defpackage #:virality
  (:use #:cl)
  (:export
   #:actor
   #:context
   #:core-bound-p
   #:define-annotation
   #:define-call-flow
   #:define-config
   #:define-graph
   #:delta
   #:destroy
   #:display-id
   #:frame-count
   #:frame-time
   #:refresh-rate
   #:id
   #:make-actor
   #:make-project
   #:mcmnt
   #:screen-resolution
   #:spawn-actor
   #:start
   #:stop
   #:total-time
   #:with-selected-interactive-core
   #:with-storage)
  ;; asset pools
  (:export
   #:meshes
   #:textures
   #:matcaps)
  ;; config
  (:export
   #:=allow-screensaver=
   #:=anti-alias-level=
   #:=delta=
   #:=debug-interval=
   #:=initial-scene=
   #:=log-level=
   #:=opengl-version=
   #:=period-interval=
   #:=release=
   #:=threads=
   #:=vsync=
   #:=window-height=
   #:=window-width=
   #:=window-title=)
  ;; prefabs
  (:import-from
   #:virality.prefab
   #:define-prefab
   #:find-prefab
   #:make-prefab-instance
   #:ref)
  (:export
   #:virality.prefab
   #:define-prefab
   #:find-prefab
   #:make-prefab-instance
   #:ref)
  ;; texture-maps
  (:import-from
   #:virality.texture-map
   #:define-texture-map)
  (:export
   #:define-texture-map)
  ;; textures
  (:import-from
   #:virality.texture
   #:define-texture
   #:define-texture-profile)
  (:export
   #:define-texture
   #:define-texture-profile)
  ;; hardware
  (:export
   #:get-hardware-info)
  ;; input system
  (:export
   #:disable-relative-motion
   #:enable-relative-motion
   #:get-gamepad-analog
   #:get-mouse-position
   #:get-mouse-scroll
   #:get-window-mode
   #:get-window-size
   #:get-window-title
   #:mouse-motion-relative-p
   #:on-button-enabled
   #:on-button-enter
   #:on-button-exit
   #:on-gamepad-analog-move
   #:on-gamepad-attach
   #:on-gamepad-detach
   #:on-gamepad-enabled
   #:set-window-hidden
   #:set-window-mode
   #:set-window-title
   #:set-window-size
   #:set-window-visible)
  ;; transform protocol
  (:export
   #:get-model-matrix
   #:get-rotation
   #:get-scale
   #:get-translation
   #:rotate
   #:rotate/velocity
   #:scale
   #:scale-around
   #:scale/velocity
   #:transform-backward
   #:transform-direction
   #:transform-down
   #:transform-forward
   #:transform-left
   #:transform-point
   #:transform-right
   #:transform-up
   #:transform-vector
   #:translate
   #:translate/velocity)
  ;; colliders
  (:export
   #:collide-p
   #:on-collision-continue
   #:on-collision-enter
   #:on-collision-exit)
  ;; assets
  (:export
   #:define-asset-pool
   #:find-asset
   #:resolve-path
   #:with-asset-cache)
  ;; geometry
  (:export
   #:define-geometry
   #:define-geometry-layout
   #:update-geometry)
  ;; regions
  (:export
   #:region
   #:center
   #:clip-movement-vector)
  ;; regions (cubboid)
  (:export
   #:region-cuboid
   #:minx
   #:maxx
   #:miny
   #:maxy
   #:minz
   #:maxz)
  ;; regions (sphere)
  (:export
   #:make-region-cuboid
   #:region-sphere
   #:radius
   #:make-region-sphere)
  ;; regions (ellipsoid)
  (:export
   #:region-ellipsoid
   #:x
   #:y
   #:z
   #:make-region-ellipsoid)
  ;; materials
  (:export
   #:copy-material
   #:define-material
   #:define-material-profile
   #:uniform-ref-p
   #:uniform-ref
   #:with-material)
  ;; kernel
  (:export
   #:attach-component
   #:attach-components
   #:component-by-type
   #:define-component
   #:make-component
   #:on-component-attach
   #:on-component-detach
   #:on-component-destroy
   #:on-component-initialize
   #:on-component-physics-update
   #:on-component-slave-render
   #:on-component-render
   #:on-component-update)
  ;; spritesheet
  (:export
   #:find-sprite
   #:make-spritesheet
   #:spritesheet
   #:update-spritesheet-shader))

(defpackage #:virality.nicknames
  (:local-nicknames
   (#:u #:vutils))
  (:use #:cl)
  (:import-from
   #+sbcl #:sb-ext
   #+ccl #:ccl
   #+(or ecl abcl clasp) #:ext
   #+lispworks #:hcl
   #+allegro #:excl
   #:add-package-local-nickname)
  (:export #:define-nicknames))

(in-package #:virality.nicknames)

(u:eval-always
  (defvar *package-nicknames*
    '((:3b-bmfont :font)
      (:vutils :u)
      (:vorigin.constants :o)
      (:vorigin.geometry.point2d :p2)
      (:vorigin.geometry.point3d :p3)
      (:vorigin.vec2 :v2)
      (:vorigin.vec3 :v3)
      (:vorigin.vec4 :v4)
      (:vorigin.dvec2 :dv2)
      (:vorigin.dvec3 :dv3)
      (:vorigin.dvec4 :dv4)
      (:vorigin.mat2 :m2)
      (:vorigin.mat3 :m3)
      (:vorigin.mat4 :m4)
      (:vorigin.dmat2 :dm2)
      (:vorigin.dmat3 :dm3)
      (:vorigin.dmat4 :dm4)
      (:vorigin.quat :q)
      (:vumbra.color :umbra.color)
      (:vumbra.graphing :umbra.graphing)
      (:vumbra.noise :umbra.noise)
      (:vumbra.sdf :umbra.sdf)
      (:vumbra.sprite :umbra.sprite)
      (:virality.clone :clone)
      (:virality.attribute-bag :abag)
      (:virality.component :comp)
      (:virality :v)
      (:virality.extension :x)
      (:virality.prefab :prefab)
      (:virality.shader :shd)
      (:virality.resource-cache :rc)
      (:virality.thread-pool :tpool)
      (:virality.uuid :uuid)
      (:virality.texture-map.texture-map-table :texmaptab)
      (:virality.texture-map :texmap)
      (:virality.texture.texture-table :textab)
      (:virality.texture :tex)
      (:virality.image :img)
      (:vshadow :shadow))))

(macrolet ((define-nicknames/internal ()
             `(progn
                ,@(mapcan
                   (lambda (x)
                     (mapcar
                      (lambda (y)
                        `(add-package-local-nickname ,@(reverse y) ,(car x)))
                      *package-nicknames*))
                   (remove-if-not
                    (lambda (x)
                      (search "VIRALITY" x))
                    *package-nicknames*
                    :key (lambda (x) (symbol-name (car x))))))))
  (define-nicknames/internal))

(defmacro define-nicknames (&body body)
  `(progn
     ,@(mapcan
        (lambda (x)
          (mapcar
           (lambda (y)
             `(add-package-local-nickname ,@(reverse y) ,(car x)))
           (append *package-nicknames* body)))
        body)))
