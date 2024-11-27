(in-package #:cl-user)

(defpackage #:colony.clone
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

(defpackage #:colony.attribute-bag
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
   #:absorb
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

(defpackage #:colony.image
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

(defpackage #:colony.resource-cache
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

(defpackage #:colony.uuid
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

(defpackage #:colony.thread-pool
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

(defpackage #:colony.texture-map
  (:use #:cl)
  ;; The macro API
  (:export
   #:define-texture-map
   )
  ;; texture-map-descriptor API
  (:export
   #:anonymous-p
   #:constructor
   #:make-texture-map-descriptor
   #:name
   #:original-form
   #:texture-map-descriptor
   )
  ;; programmatic texture-map API types
  (:export
   #:cube
   #:cube-representaton
   #:data-element ;; TODO move to another package
   #:data-span ;; TODO move to another package
   #:data-span-1d ;; TODO move to another package
   #:data-span-2d ;; TODO move to another package
   #:data-span-3d ;; TODO move to another package
   #:envmap-representation
   #:face
   #:faces-representation
   #:image-element
   #:location ;; TODO move to another package
   #:mapping-span ;; TODO move to another package
   #:mapping-span-1d ;; TODO move to another package
   #:mapping-span-2d ;; TODO move to another package
   #:mapping-span-3d ;; TODO move to another package
   #:mipmap
   #:mipmap-1d
   #:mipmap-2d
   #:mipmap-3d
   #:name
   #:span ;; TODO move to another package
   #:span-1d ;; TODO move to another package
   #:span-2d ;; TODO move to another package
   #:span-3d ;; TODO move to another package
   #:storage-form
   #:texture-map
   #:texture-map-1d
   #:texture-map-2d
   #:texture-map-3d
   #:texture-map-complex
   #:texture-map-cube
   #:texture-map-descriptor
   #:texture-map-element
   #:texture-map-simple
   )
  ;; programmatic texture-map API methods and functions
  (:export
   #:anonymous-p ;; accessor for texture-map
   #:cube ;; accessor texture-map-cube
   #:data-elements ;; accessor for texture-map
   #:dir ;; accessor for face
   #:element ;; accessor for data-element TODO move
   #:elidx ;; accessor for data-span TODO move AND accessor for face, keep here
   #:extent ;; accessor for span TODO move AND accessor for mipmap, keep here
   #:faces ;; accessor for faces-representation
   #:from ;; accessor for mapping-span TODO move
   #:logloc ;; accessor for location TODO move
   #:make-cube
   #:make-cube-representation ;; GF for cube-representation
   #:make-data-element ;; GF for data-element TODO maybe move?
   #:make-data-elements
   #:make-data-span ;; GF for data-span TODO maybe move?
   #:make-data-span-1d ;; TODO move to another package
   #:make-data-span-2d ;; TODO move to another package
   #:make-data-span-3d ;; TODO move to another package
   #:make-envmap-representation
   #:make-face
   #:make-faces
   #:make-faces-representation
   #:make-image-element
   #:make-location ;; TODO move to another package.
   #:make-mapping-span ;; GF for mapping-span TODO maybe move?
   #:make-mapping-span-1d ;; TODO move to another package
   #:make-mapping-span-2d ;; TODO move to another package
   #:make-mapping-span-3d ;; TODO move to another package
   #:make-mapping-spans
   #:make-mipmap-1d
   #:make-mipmap-2d
   #:make-mipmap-3d
   #:make-mipmaps
   #:make-span ;; GF for spam TODO maybe move?
   #:make-span-1d ;; TODO move to another package
   #:make-span-2d ;; TODO move to another package
   #:make-span-3d ;; TODO move to another package
   #:make-storage-form ;; GF for storage-form
   #:make-texture-map ;; GF for texture-map
   #:make-texture-map-1d
   #:make-texture-map-2d
   #:make-texture-map-3d
   #:make-texture-map-cube
   #:make-texture-map-element
   #:mapping-spans ;; accessor for mipmap
   #:mipmaps ;; accessor for envmap-representaton, texture-map-simple
   #:model ;; accessor for texture-map
   #:name ;; accessor for texture-map
   #:origin ;; accessor for span TODO move
   #:physloc ;; accessor for location TODO move
   #:repr ;; accessor for cube
   #:store ;; accessor for cube, texture-map
   #:style ;; accessor for cube, texture-map
   #:to ;; accessor for mapping-span TODO move
   )
  ;; Exported define-texture-map DSL symbols
  (:export
   #:attrs
   #:cattrs
   #:cube
   #:data-elements
   #:data-span
   #:data-span-1d
   #:data-span-2d
   #:data-span-3d
   #:envmap
   #:face
   #:faces
   #:image-element
   #:mapping-span
   #:mapping-span-1d
   #:mapping-span-2d
   #:mapping-span-3d
   #:mipmap
   #:mipmap-1d
   #:mipmap-2d
   #:mipmap-3d
   #:sattrs
   #:span
   #:span-1d
   #:span-2d
   #:span-3d
   #:texture-map-element
   )
  )

(defpackage #:colony.texture-map.texture-map-table
  (:use #:cl)
  ;; texture-map-table API
  (:export
   #:add-semantic-texture-map-descriptor
   #:find-semantic-texture-map-descriptor
   #:make-texture-map-table
   #:reify-texture-map-descriptors
   #:remove-semantic-texture-map-descriptor
   #:resolve-all-semantic-texture-map-descriptors
   #:resolve-semantic-texture-map-descriptor
   #:semantic-texture-map-descriptors
   #:texture-map-table
   ))

(defpackage #:colony.texture
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

(defpackage #:colony.texture.texture-table
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

(defpackage #:colony.prefab
  (:use #:cl)
  (:export
   #:define-prefab
   #:find-prefab
   #:make-prefab-instance
   #:ref))

(uiop:define-package #:colony.shader
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

(defpackage #:colony.extension
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

(defpackage #:colony.component
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

(defpackage #:colony
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
   #:colony.prefab
   #:define-prefab
   #:find-prefab
   #:make-prefab-instance
   #:ref)
  (:export
   #:colony.prefab
   #:define-prefab
   #:find-prefab
   #:make-prefab-instance
   #:ref)
  ;; texture-maps
  (:import-from
   #:colony.texture-map
   #:define-texture-map
   ;; Import texture-map dsl tokens
   #:attrs
   #:cattrs
   #:cube
   #:data-elements
   #:data-span
   #:data-span-1d
   #:data-span-2d
   #:data-span-3d
   #:envmap
   #:face
   #:faces
   #:image-element
   #:mapping-span
   #:mapping-span-1d
   #:mapping-span-2d
   #:mapping-span-3d
   #:mipmap
   #:mipmap-1d
   #:mipmap-2d
   #:mipmap-3d
   #:sattrs
   #:span
   #:span-1d
   #:span-2d
   #:span-3d
   #:texture-map-element)
  (:export
   #:define-texture-map
   ;; texture-map DSL tokens
   #:attrs
   #:cattrs
   #:cube
   #:data-elements
   #:data-span
   #:data-span-1d
   #:data-span-2d
   #:data-span-3d
   #:envmap
   #:face
   #:faces
   #:image-element
   #:mapping-span
   #:mapping-span-1d
   #:mapping-span-2d
   #:mapping-span-3d
   #:mipmap
   #:mipmap-1d
   #:mipmap-2d
   #:mipmap-3d
   #:sattrs
   #:span
   #:span-1d
   #:span-2d
   #:span-3d
   #:texture-map-element)
  ;; textures
  (:import-from
   #:colony.texture
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

(defpackage #:colony.nicknames
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

(in-package #:colony.nicknames)

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
      (:colony.clone :clone)
      (:colony.attribute-bag :abag)
      (:colony.component :comp)
      (:colony :c)
      (:colony.extension :x)
      (:colony.prefab :prefab)
      (:colony.shader :shd)
      (:colony.resource-cache :rc)
      (:colony.thread-pool :tpool)
      (:colony.uuid :uuid)
      (:colony.texture-map.texture-map-table :texmaptab)
      (:colony.texture-map :texmap)
      (:colony.texture.texture-table :textab)
      (:colony.texture :tex)
      (:colony.image :img)
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
                      (search "COLONY" x))
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
