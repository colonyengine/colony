(in-package #:cl-user)

(defpackage #:virality.texture
  (:use #:cl)
  (:export
   #:define-texture
   #:define-texture-profile))

(defpackage #:virality.prefab
  (:use #:cl)
  (:export
   #:define-prefab
   #:find-prefab
   #:make-prefab-instance
   #:ref))

(uiop:define-package #:virality.shader
    (:use-reexport
     #:net.mfiano.lisp.shadow.glsl
     #:net.mfiano.lisp.umbra.common))

(defpackage #:virality.extension
  (:use #:cl)
  ;; geometry layouts
  (:export
   #:2d)
  ;; materials
  (:export
   #:collider/cuboid
   #:collider/sphere
   #:missing-material
   #:sprite
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal
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
   #:debug-texture)
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
   #:render)
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
   #:model
   #:remove-child
   #:transform))

(defpackage #:virality
  (:use #:cl)
  (:export
   #:actor
   #:context
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
   #:screen-resolution
   #:spawn-actor
   #:start
   #:stop
   #:total-time
   #:with-storage)
  ;; asset pools
  (:export
   #:textures)
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
   (#:u #:net.mfiano.lisp.golden-utils))
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
      (:net.mfiano.lisp.golden-utils :u)
      (:net.mfiano.lisp.origin :o)
      (:net.mfiano.lisp.origin.swizzle :~)
      (:net.mfiano.lisp.origin.vec2 :v2)
      (:net.mfiano.lisp.origin.vec3 :v3)
      (:net.mfiano.lisp.origin.vec4 :v4)
      (:net.mfiano.lisp.origin.mat2 :m2)
      (:net.mfiano.lisp.origin.mat3 :m3)
      (:net.mfiano.lisp.origin.mat4 :m4)
      (:net.mfiano.lisp.origin.quat :q)
      (:net.mfiano.lisp.shadow :shadow)
      (:net.mfiano.lisp.umbra.color :umbra.color)
      (:net.mfiano.lisp.umbra.graphing :umbra.graphing)
      (:net.mfiano.lisp.umbra.noise :umbra.noise)
      (:net.mfiano.lisp.umbra.sdf :umbra.sdf)
      (:net.mfiano.lisp.umbra.sprite :umbra.sprite)
      (:virality.component :comp)
      (:virality :v)
      (:virality.extension :x)
      (:virality.prefab :prefab)
      (:virality.shader :shd)
      (:virality.texture :tex))))

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
