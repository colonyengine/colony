(in-package #:virality.examples)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is not really a general purpose component. It is just here to help out
;; testing how destruction and colliders work together.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(v:define-component destroy-my-actor ()
  ((%time-to-destroy :accessor time-to-destroy
                     :initarg :time-to-destroy
                     :initform 5f0)))

(defmethod v:on-collision-enter ((self destroy-my-actor) other-collider)
  (log:info :virality.examples
            "DESTROY-MY-ACTOR: Actor ~a entered collision with collider ~
             ~a(on actor ~a)"
            (v:actor self) other-collider (v:actor other-collider))
  (when (string= (v:display-id other-collider) "Ground")
    (log:info :virality.examples
              "===>>> DESTROY-MY-ACTOR: It was specifically the \"Ground\" ~
               object, so destroy myself!")
    (v:destroy (v:actor self))))

(defmethod v:on-collision-exit ((self destroy-my-actor) other-collider)
  (log:info :virality.examples
            "DESTROY-MY-ACTOR: Actor ~a is exiting collision with ~
             ~a(on actor: ~a)."
            (v:actor self) other-collider (v:actor other-collider)))

(defmethod v:on-component-update ((self destroy-my-actor))
  (decf (time-to-destroy self) (v:frame-time (v:context self)))
  (when (<= (time-to-destroy self) 0)
    (v:destroy (v:actor self))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing getting the directions from a transform
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(v:define-component unit-test-transform-api ()
  ((%test-type :reader test-type
               :initarg :test-type
               :initform nil)
   (%test-performed :reader test-performed
                    :initform (u:dict))))

(defmethod v:on-component-physics-update ((self unit-test-transform-api))
  (ecase (test-type self)
    (:test-direction-vectors
     (test-axis-directions self))
    (:test-transform-api
     (test-transform-api self))))

(defun test-axis-directions (self)
  (let* ((actor (v:actor self))
         (test-type (test-type self))
         (actor-transform
           (v:component-by-type actor 'c/xform:transform)))
    (unless (u:href (test-performed self) test-type)
      (let ((forward (c/xform:transform-forward actor-transform))
            (backward (c/xform:transform-backward actor-transform))
            (up (c/xform:transform-up actor-transform))
            (down (c/xform:transform-down actor-transform))
            (right (c/xform:transform-right actor-transform))
            (left (c/xform:transform-left actor-transform)))
        (log:trace :virality.examples "FORWARD Vector -> ~a" forward)
        (log:trace :virality.examples "BACKWARD Vector -> ~a" backward)
        (log:trace :virality.examples "UP Vector -> ~a" up)
        (log:trace :virality.examples "DOWN Vector -> ~a" down)
        (log:trace :virality.examples "RIGHT Vector -> ~a" right)
        (log:trace :virality.examples "LEFT Vector -> ~a" left)
        ;; NOTE: This expects the actor to be unrotated wrt the universe.
        (unless (and (v3:~ forward (v3:vec 0f0 0f0 -1f0))
                     (v3:~ backward (v3:vec 0f0 0f0 1f0))
                     (v3:~ up (v3:vec 0f0 1f0 0f0))
                     (v3:~ down (v3:vec 0f0 -1f0 0f0))
                     (v3:~ right (v3:vec 1f0 0f0 0f0))
                     (v3:~ left (v3:vec -1f0 0f0 0f0)))
          (error "The Transform Axis Direction API didn't match expectations!"))
        (setf (u:href (test-performed self) test-type) t)))))

(defun test-transform-api (self)
  (let* ((test-type (test-type self)))
    (unless (u:href (test-performed self) test-type)
      (test-transform-point-api self)
      (test-transform-vector-api self)
      (test-transform-direction-api self)
      ;; And ensure we don't run this again.
      (setf (u:href (test-performed self) test-type) t))))

(defun test-transform-point-api (self)
  "Test if the TRANSFORM-POINT and INVERSE-TRANSFORM-POINT work."
  (let* ((actor (v:actor self))
         (actor-transform
           (v:component-by-type actor 'c/xform:transform))
         (object-space-point (v3:vec 1f0 0f0 0f0))
         (world-space-point (v3:vec 1f0 3f0 1f0))
         (local->world
           (c/xform:transform-point actor-transform
                                    object-space-point))
         (world->local
           (c/xform:inverse-transform-point actor-transform
                                            world-space-point)))

    ;; See if transform-point and inverse-transform-point work.
    (let ((result-0
            (v3:~ local->world world-space-point))
          (result-1
            (v3:~ world->local object-space-point)))

      (unless (and result-0 result-1)
        (unless result-0
          (log:error
           :virality.examples
           "FAILED: (v3:~~ local->world:~a world-space-point: ~a) -> ~a"
           local->world world-space-point result-0))

        (unless result-1
          (log:error
           :virality.examples
           "FAILED: (v3:~~ world->local:~a object-space-point: ~a) -> ~a"
           world->local object-space-point result-1))

        (error "TRANSFORM-POINT API Failed!")))))

(defun test-transform-vector-api (self)
  "Test if the TRANSFORM-VECTOR and INVERSE-TRANSFORM-VECTOR work."
  (let* ((actor (v:actor self))
         (actor-transform
           (v:component-by-type actor 'c/xform:transform))
         (object-space-vector (v3:vec 2f0 2f0 0f0))
         (world-space-vector (v3:vec -4f0 4f0 0f0))
         (local->world
           (c/xform:transform-vector actor-transform
                                     object-space-vector))
         (world->local
           (c/xform:inverse-transform-vector actor-transform
                                             world-space-vector)))

    ;; See if transform-point and inverse-transform-point work.
    (let ((result-0
            (v3:~ local->world world-space-vector))
          (result-1
            (v3:~ world->local object-space-vector)))

      (unless (and result-0 result-1)
        (unless result-0
          (log:error
           :virality.examples
           "FAILED: (v3:~~ local->world:~a world-space-vector: ~a) -> ~a"
           local->world world-space-vector result-0))

        (unless result-1
          (log:error
           :virality.examples
           "FAILED: (v3:~~ world->local:~a object-space-vector: ~a) -> ~a"
           world->local object-space-vector result-1))

        (error "TRANSFORM-VECTOR API Failed!")))))


(defun test-transform-direction-api (self)
  (let* ((actor (v:actor self))
         (actor-transform
           (v:component-by-type actor 'c/xform:transform))
         ;; NOTE: these must be normalized for the test. I specified it this way
         ;; so it would be easier to see in your mind's eye.
         (object-space-direction (v3:normalize (v3:vec 1f0 1f0 0f0)))
         (world-space-direction (v3:normalize (v3:vec -1f0 1f0 0f0)))
         (local->world
           (c/xform:transform-direction actor-transform
                                        object-space-direction))
         (world->local
           (c/xform:inverse-transform-direction actor-transform
                                                world-space-direction)))

    ;; See if transform-point and inverse-transform-point work.
    (let ((result-0
            (v3:~ local->world world-space-direction))
          (result-1
            (v3:~ world->local object-space-direction)))

      (unless (and result-0 result-1)
        (unless result-0
          (log:error
           :virality.examples
           "FAILED: (v3:~~ local->world:~a world-space-direction: ~a) -> ~a"
           local->world world-space-direction result-0))

        (unless result-1
          (log:error
           :virality.examples
           "FAILED: (v3:~~ world->local:~a object-space-direction: ~a) -> ~a"
           world->local object-space-direction result-1))

        (error "TRANSFORM-DIRECTION API Failed!")))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The test prefabs.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(v:define-prefab "collision-smoke-test" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 6f0))
  ("rot-0-center"
   (c/xform:transform :translate (v3:vec -2f0 0f0 0f0)
                      :rotate/inc (o:make-velocity v3:+forward+
                                                   (float pi 1f0)))
   ("plane-0"
    (c/xform:transform :translate (v3:vec -2f0 0f0 0f0))
    (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
    (c/col:sphere :display-id "Player"
                  :visualize t
                  :on-layer :player
                  :center (v3:vec)
                  :radius 1f0)
    (c/render:render :material '2d-wood)))
  ("rot-1-center"
   (c/xform:transform :translate (v3:vec 2f0 0f0 0f0)
                      :rotate/inc (o:make-velocity v3:+forward+
                                                   (float (- pi) 1f0)))
   ("plane-1"
    (c/xform:transform :translate (v3:vec 2f0 0f0 0f0))
    (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
    (c/col:sphere :display-id "Enemy"
                  :visualize t
                  :on-layer :enemy
                  :center (v3:vec)
                  :radius 1f0)
    (c/render:render :material '2d-wood))))

(v:define-prefab "collision-transform-test-0" (:library examples)
  "This test just prints out the directions of the actor transform. Since
the actor is at universe 0,0,0 and has no rotations, we should see the
unit world vector representations of the axis directions as:
  forward:  (0 0 -1)
  backward: (0 0 1)
  up:       (0 1 0)
  down:     (0 -1 0)
  right:    (1 0 0)
  left:     (-1 0 0)
"
  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 7f0))

  ("thingy"
   ;; NOTE: The 5 0 0 is specific to the unit-test-transform-api tests.
   (c/xform:transform :translate (v3:vec 5f0 0f0 0f0))
   (unit-test-transform-api :test-type :test-direction-vectors)
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
   (c/render:render :material '2d-wood)))

(v:define-prefab "collision-transform-test-1" (:library examples)
  "This test checks to see if we can move in and out of object space and
world space for a particular transform."

  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 7f0))

  ("right"
   (c/xform:transform :translate (v3:vec 1f0 0f0 0f0))
   ("up"
    (c/xform:transform :translate (v3:vec 0f0 1f0 0f0))
    ("back"
     (c/xform:transform :translate (v3:vec 0f0 0f0 1f0))
     ("mark"
      ;; Origin sitting at 1,1,1 wrt the universe, but +90deg rotation around
      ;; "mark" Z axis.
      (c/xform:transform :rotate (q:orient :local :z (float (/ pi 2f0) 1.0))
                         :scale 2)
      (unit-test-transform-api :test-type :test-transform-api)
      (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
      (c/render:render :material '2d-wood))))))


(v:define-prefab "collision-test-0" (:library examples)
  "In this test, you should see two actors with a narrow gap between them and
ananother actor near the bottom of the screen. These three are unmoving. The
green spiral (if VISUALIZE defaults to T in the sphere component) is a sphere
collider being rendered as a spiral wound around it (it is 3d and you're seeing
the top of it in the ortho projection). A small actor shows up and moves
downwards. When it hits the narrow gape, when the colliders touch they will
highlight and there will be output to the repl. After it leaves the narrow gap
and hits the bottom actor, it will disappear, having destroyed itself when
hitting the bottom actor. The DESTROY-MY-ACTOR component specifically checks for
the \"Ground\" collider via its display-id. This is a hack, but good enough for
a test. After it disappears, nothing else happens. TODO: Make a spawner object
that just spawns stone prefabs so they rain down onto the ground, which should
be made bigger. to accomodate it. Maybe some fragments too when it hits..."

  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 7f0))

  ("left-gate"
   (c/xform:transform :translate (v3:vec -1.15f0 2f0 -.1f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
   (c/render:render :material '2d-wood)
   (c/col:sphere :display-id "Left-Gate"
                 :visualize t
                 :on-layer :ground))

  ("right-gate"
   (c/xform:transform :translate (v3:vec 1.15f0 2f0 -.1f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
   (c/render:render :material '2d-wood)
   (c/col:sphere :display-id "Right-Gate"
                 :visualize t
                 :on-layer :ground))

  ("stone"
   (c/xform:transform :translate (v3:vec 0f0 5f0 0f0)
                      :scale 0.5f0
                      :rotate (q:orient :local :x (float (/ pi 2f0) 1f0))
                      :rotate/inc (o:make-velocity (v3:vec 1)
                                                   (float pi 1.0))
                      :translate/inc (v3:vec 0f0 -2f0 0f0))
   (c/smesh:static-mesh :asset '(:mesh "damaged-helmet.glb"))
   (destroy-my-actor :display-id "destroy-my-actor: stone")
   (c/col:sphere :display-id "Stone"
                 :visualize t
                 :on-layer :player
                 :referent (v:ref :self
                                  :component 'destroy-my-actor)
                 :center (v3:vec)
                 :radius 1f0)
   (c/render:render :material 'damaged-helmet))

  ("ground"
   (c/xform:transform :translate (v3:vec 0f0 -2f0 0.1f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
   (c/col:sphere :display-id "Ground"
                 :visualize t
                 :on-layer :ground
                 :center (v3:vec)
                 :radius 1f0)
   (c/render:render :material '2d-wood)))

(v:define-prefab "collision-test-1" (:library examples)
  "This test demonstrates that at frame 0 colliders that should be colliding
actually are. You have to view the results to see the colliders lighting up."

  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 7f0))

  ("upper-left"
   (c/xform:transform :translate (v3:vec -2f0 2f0 -0.1f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
   (c/render:render :material '2d-wood)
   (c/col:sphere :display-id "Upper-Left"
                 :visualize t
                 :on-layer :ground))
  ("upper-right"
   (c/xform:transform :translate (v3:vec 2f0 2f0 -0.1f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
   (c/render:render :material '2d-wood)
   (c/col:sphere :display-id "Upper-Right"
                 :visualize t
                 :on-layer :ground))
  ("lower-left"
   (c/xform:transform :translate (v3:vec -2f0 -2f0 -0.1f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
   (c/render:render :material '2d-wood)
   (c/col:sphere :display-id "Lower-Left"
                 :visualize t
                 :on-layer :ground))
  ("lower-right"
   (c/xform:transform :translate (v3:vec 2f0 -2f0 -0.1f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
   (c/render:render :material '2d-wood)
   (c/col:sphere :display-id "Lower-Right"
                 :visualize t
                 :on-layer :ground))
  ("stone"
   (c/xform:transform :translate (v3:vec)
                      :scale 2f0
                      :rotate (q:orient :local :x (float (/ pi 2) 1.0))
                      :rotate/inc (o:make-velocity (v3:vec 1)
                                                   (float pi 1.0))
                      :translate/inc (v3:vec))
   (c/smesh:static-mesh :asset '(:mesh "damaged-helmet.glb"))
   (destroy-my-actor :time-to-destroy 2f0)
   (c/col:sphere :display-id "Stone"
                 :visualize t
                 :on-layer :player
                 :center (v3:vec)
                 :radius 1f0)
   (c/render:render :material 'damaged-helmet)))

(v:define-prefab "collision-test-2" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 7f0))

  ("a"
   (c/xform:transform :translate (v3:vec -5f0 0f0 0f0)
                      :translate/inc (v3:vec 0f0 0f0 0f0)
                      :scale 2f0)

   ("stone-cuboid"
    (c/xform:transform :scale 2f0
                       :rotate (q:orient :local :x (float (/ pi 2) 1.0))
                       :rotate/inc (o:make-velocity (v3:vec 1) o:pi/6))
    (c/smesh:static-mesh :asset '(:mesh "damaged-helmet.glb"))
    (c/col:cuboid :display-id "Stone"
                  :visualize t
                  :on-layer :ground
                  :center (v3:vec)
                  :minx -1f0
                  :maxx 1f0
                  :miny -1f0
                  :maxy 1f0
                  :minz -1f0
                  :maxz 1f0)
    (c/render:render :material 'damaged-helmet)))

  ("b"
   (c/xform:transform :translate (v3:vec 5f0 0f0 0f0)
                      :translate/inc (v3:vec 0f0 0f0 0f0)
                      :scale 2f0)
   ("stone-sphere"
    (c/xform:transform :scale 2f0
                       :rotate (q:orient :local :x (float (/ pi 2) 1.0))
                       :rotate/inc (o:make-velocity (v3:vec 1) o:pi/6))
    (c/smesh:static-mesh :asset '(:mesh "damaged-helmet.glb"))
    (c/col:sphere :display-id "Stone"
                  :visualize t
                  :on-layer :ground
                  :center (v3:vec)
                  :radius 1.25f0)
    (c/render:render :material 'damaged-helmet))))

(v:define-prefab "collision-test-3" (:library examples)
  (("camera" :copy "/cameras/ortho")
   (c/cam:camera (:policy :new-args) :zoom 140f0))

  ("test-case-always"
   (c/xform:transform :translate (v3:vec -2f0 0f0 0f0))
   ("a"
    ;; As cuboid 1 rotates it should always be hitting (red) cuboid 2 since they
    ;; penetrate and then share a plane when both are parallel to each other.
    (c/xform:transform :translate (v3:vec -.50f0 0f0 0f0)
                       :translate/inc (v3:vec 0f0 0f0 0f0))
    ("cuboid1"
     (c/xform:transform :rotate (q:orient :local :z o:pi/4)
                        :rotate/inc (o:make-velocity (v3:vec 0 0 1) o:pi/12))
     (c/col:cuboid :visualize t
                   :on-layer :ground
                   :center (v3:vec))))
   ("cuboid2"
    (c/xform:transform :translate (v3:vec 0.5f0 0f0 0f0))
    (c/col:cuboid :visualize t
                  :on-layer :ground
                  :center (v3:vec))))

  ("test-case-gap"
   (c/xform:transform :translate (v3:vec 2f0 0f0 0f0))
   ("a"
    ;; As cuboid 1 rotates, when they become parallel, there will be a slight
    ;; gap and so both should turn green to represent no collision during that
    ;; small gap.
    (c/xform:transform :translate (v3:vec -.51f0 0f0 0f0)
                       :translate/inc (v3:vec 0f0 0f0 0f0))
    ("cuboid1"
     (c/xform:transform :rotate (q:orient :local :z o:pi/4)
                        :rotate/inc (o:make-velocity (v3:vec 0 0 1) o:pi/12))
     (c/col:cuboid :visualize t
                   :on-layer :ground
                   :center (v3:vec))))
   ("cuboid2"
    (c/xform:transform :translate (v3:vec 0.5f0 0f0 0f0))
    (c/col:cuboid :visualize t
                  :on-layer :ground
                  :center (v3:vec)))))



;;; Prefab entry points
;; '(("collision-smoke-test" examples))
;; '(("collision-test-0" examples))
;; '(("collision-test-1" examples)
;; '(("collision-test-2" examples))
;; '(("collision-test-3" examples))
