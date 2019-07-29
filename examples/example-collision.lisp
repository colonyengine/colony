(in-package #:first-light.example)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is not really a general purpose component. It is just here to help out
;; testing how destruction and colliders work together.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fl:define-component destroy-my-actor ()
  ((%time-to-destroy :accessor time-to-destroy
                     :initarg :time-to-destroy
                     :initform 5)))

(defmethod fl:on-collision-enter ((self destroy-my-actor) other-collider)
  (v:info :fl.example
          "DESTROY-MY-ACTOR: Actor ~A entered collision with collider ~
           ~A(on actor ~A)"
          (fl:actor self) other-collider (fl:actor other-collider))
  (when (string= (fl:display-id other-collider) "Ground")
    (v:info :fl.example
            "===>>> DESTROY-MY-ACTOR: It was specifically the \"Ground\" ~
             object, so destroy myself!")
    (fl:destroy (fl:actor self))))

(defmethod fl:on-collision-exit ((self destroy-my-actor) other-collider)
  (v:info :fl.example
          "DESTROY-MY-ACTOR: Actor ~A is exiting collision with ~
           ~A(on actor: ~A)."
          (fl:actor self) other-collider (fl:actor other-collider)))

(defmethod fl:on-component-update ((self destroy-my-actor))
  (decf (time-to-destroy self) (fl:frame-time (fl:context self)))
  (when (<= (time-to-destroy self) 0)
    (fl:destroy (fl:actor self))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing getting the directions from a transform
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fl:define-component unit-test-transform-api ()
  ((%test-type :reader test-type
               :initarg :test-type
               :initform nil)
   (%test-performed :reader test-performed
                    :initform (u:dict))))

(defmethod fl:on-component-physics-update ((self unit-test-transform-api))
  (ecase (test-type self)
    (:test-direction-vectors
     (test-axis-directions self))
    (:test-transform-api
     (test-transform-api self))))

(defun test-axis-directions (self)
  (let* ((actor (fl:actor self))
         (test-type (test-type self))
         (actor-transform
           (fl:actor-component-by-type actor 'fl.comp:transform)))

    (unless (u:href (test-performed self) test-type)
      (let ((forward (fl.comp:transform-forward actor-transform))
            (backward (fl.comp:transform-backward actor-transform))
            (up (fl.comp:transform-up actor-transform))
            (down (fl.comp:transform-down actor-transform))
            (right (fl.comp:transform-right actor-transform))
            (left (fl.comp:transform-left actor-transform)))

        (v:trace :fl.example "FORWARD Vector -> ~A" forward)
        (v:trace :fl.example "BACKWARD Vector -> ~A" backward)
        (v:trace :fl.example "UP Vector -> ~A" up)
        (v:trace :fl.example "DOWN Vector -> ~A" down)
        (v:trace :fl.example "RIGHT Vector -> ~A" right)
        (v:trace :fl.example "LEFT Vector -> ~A" left)

        ;; NOTE: This expects the actor to be unrotated wrt the universe.
        (unless (and (v3:~ forward (v3:vec 0 0 -1))
                     (v3:~ backward (v3:vec 0 0 1))
                     (v3:~ up (v3:vec 0 1 0))
                     (v3:~ down (v3:vec 0 -1 0))
                     (v3:~ right (v3:vec 1 0 0))
                     (v3:~ left (v3:vec -1 0 0)))
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
  (let* ((actor (fl:actor self))
         (actor-transform
           (fl:actor-component-by-type actor 'fl.comp:transform))
         (object-space-point (v3:vec 1 0 0))
         (world-space-point (v3:vec 1 3 1))
         (local->world
           (fl.comp:transform-point actor-transform
                                    object-space-point))
         (world->local
           (fl.comp:inverse-transform-point actor-transform
                                            world-space-point)))

    ;; See if transform-point and inverse-transform-point work.
    (let ((result-0
            (v3:~ local->world world-space-point))
          (result-1
            (v3:~ world->local object-space-point)))

      (unless (and result-0 result-1)
        (unless result-0
          (v:error
           :fl.example
           "FAILED: (v3:~~ local->world:~A world-space-point: ~A) -> ~A"
           local->world world-space-point result-0))

        (unless result-1
          (v:error
           :fl.example
           "FAILED: (v3:~~ world->local:~A object-space-point: ~A) -> ~A"
           world->local object-space-point result-1))

        (error "TRANSFORM-POINT API Failed!")))))

(defun test-transform-vector-api (self)
  "Test if the TRANSFORM-VECTOR and INVERSE-TRANSFORM-VECTOR work."
  (let* ((actor (fl:actor self))
         (actor-transform
           (fl:actor-component-by-type actor 'fl.comp:transform))
         (object-space-vector (v3:vec 2 2 0))
         (world-space-vector (v3:vec -4 4 0))
         (local->world
           (fl.comp:transform-vector actor-transform
                                     object-space-vector))
         (world->local
           (fl.comp:inverse-transform-vector actor-transform
                                             world-space-vector)))

    ;; See if transform-point and inverse-transform-point work.
    (let ((result-0
            (v3:~ local->world world-space-vector))
          (result-1
            (v3:~ world->local object-space-vector)))

      (unless (and result-0 result-1)
        (unless result-0
          (v:error
           :fl.example
           "FAILED: (v3:~~ local->world:~A world-space-vector: ~A) -> ~A"
           local->world world-space-vector result-0))

        (unless result-1
          (v:error
           :fl.example
           "FAILED: (v3:~~ world->local:~A object-space-vector: ~A) -> ~A"
           world->local object-space-vector result-1))

        (error "TRANSFORM-VECTOR API Failed!")))))


(defun test-transform-direction-api (self)
  (let* ((actor (fl:actor self))
         (actor-transform
           (fl:actor-component-by-type actor 'fl.comp:transform))
         ;; NOTE: these must be normalized for the test. I specified it this way
         ;; so it would be easier to see in your mind's eye.
         (object-space-direction (v3:normalize (v3:vec 1 1 0)))
         (world-space-direction (v3:normalize (v3:vec -1 1 0)))
         (local->world
           (fl.comp:transform-direction actor-transform
                                        object-space-direction))
         (world->local
           (fl.comp:inverse-transform-direction actor-transform
                                                world-space-direction)))

    ;; See if transform-point and inverse-transform-point work.
    (let ((result-0
            (v3:~ local->world world-space-direction))
          (result-1
            (v3:~ world->local object-space-direction)))

      (unless (and result-0 result-1)
        (unless result-0
          (v:error
           :fl.example
           "FAILED: (v3:~~ local->world:~A world-space-direction: ~A) -> ~A"
           local->world world-space-direction result-0))

        (unless result-1
          (v:error
           :fl.example
           "FAILED: (v3:~~ world->local:~A object-space-direction: ~A) -> ~A"
           world->local object-space-direction result-1))

        (error "TRANSFORM-DIRECTION API Failed!")))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The test prefabs.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fl:define-prefab "collision-smoke-test" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (fl.comp:camera (:policy :new-args) :zoom 6))
  ("rot-0-center"
   (fl.comp:transform :translate (v3:vec -2 0 0)
                      :rotate/inc (q:orient :local :z pi))
   ("plane-0"
    (fl.comp:transform :translate (v3:vec -2 0 0))
    (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:collider/sphere
     :display-id "Player"
     :visualize t
     :on-layer :player
     :center (v3:zero)
     :radius 1)
    (fl.comp:render :material '2d-wood)))
  ("rot-1-center"
   (fl.comp:transform :translate (v3:vec 2 0 0)
                      :rotate/inc (q:orient :local :z (- pi)))
   ("plane-1"
    (fl.comp:transform :translate (v3:vec 2 0 0))
    (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:collider/sphere
     :display-id "Enemy"
     :visualize t
     :on-layer :enemy
     :center (v3:zero)
     :radius 1)
    (fl.comp:render :material '2d-wood))))

(fl:define-prefab "collision-transform-test-0" (:library examples)
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
   (fl.comp:camera (:policy :new-args) :zoom 7))

  ("thingy"
   ;; NOTE: The 5 0 0 is specific to the unit-test-transform-api tests.
   (fl.comp:transform :translate (v3:vec 5 0 0))
   (unit-test-transform-api :test-type :test-direction-vectors)
   (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)))

(fl:define-prefab "collision-transform-test-1" (:library examples)
  "This test checks to see if we can move in and out of object space and
world space for a particular transform."

  (("camera" :copy "/cameras/perspective")
   (fl.comp:camera (:policy :new-args) :zoom 7))

  ("right"
   (fl.comp:transform :translate (v3:vec 1 0 0))
   ("up"
    (fl.comp:transform :translate (v3:vec 0 1 0))
    ("back"
     (fl.comp:transform :translate (v3:vec 0 0 1))
     ("mark"
      ;; Origin sitting at 1,1,1 wrt the universe, but +90deg rotation around
      ;; "mark" Z axis.
      (fl.comp:transform :rotate (q:orient :local :z (/ pi 2))
                         :scale 2)
      (unit-test-transform-api :test-type :test-transform-api)
      (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
      (fl.comp:render :material '2d-wood))))))


(fl:define-prefab "collision-test-0" (:library examples)
  "In this test, you should see two actors with a narrow gap between them and
ananother actor near the bottom of the screen. These three are unmoving. The
green spiral (if VISUALIZE defaults to T in the collider/sphere component) is a
sphere collider being rendered as a spiral wound around it (it is 3d and you're
seeing the top of it in the ortho projection). A small actor shows up and moves
downwards. When it hits the narrow gape, when the colliders touch they will
highlight and there will be output to the repl. After it leaves the narrow gap
and hits the bottom actor, it will disappear, having destroyed itself when
hitting the bottom actor. The DESTROY-MY-ACTOR component specifically checks for
the \"Ground\" collider via its display-id. This is a hack, but good enough for
a test. After it disappears, nothing else happens. TODO: Make a spawner object
that just spawns stone prefabs so they rain down onto the ground, which should
be made bigger. to accomodate it. Maybe some fragments too when it hits..."

  (("camera" :copy "/cameras/perspective")
   (fl.comp:camera (:policy :new-args) :zoom 7))

  ("left-gate"
   (fl.comp:transform :translate (v3:vec -1.15 2 -.1))
   (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Left-Gate"
                            :visualize t
                            :on-layer :ground))

  ("right-gate"
   (fl.comp:transform :translate (v3:vec 1.15 2 -.1))
   (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Right-Gate"
                            :visualize t
                            :on-layer :ground))

  ("stone"
   (fl.comp:transform :translate (v3:vec 0 5 0)
                      :scale 0.5
                      :rotate (q:orient :local :x (/ pi 2))
                      :rotate/inc (q:orient :local (v3:one) pi)
                      :translate/inc (v3:vec 0 -2 0))
   (fl.comp:static-mesh :location '(:mesh "damaged-helmet.glb"))
   (destroy-my-actor :display-id "destroy-my-actor: stone")
   (fl.comp:collider/sphere :display-id "Stone"
                            :visualize t
                            :on-layer :player
                            :referent (fl:ref :self
                                              :component 'destroy-my-actor)
                            :center (v3:zero)
                            :radius 1)
   (fl.comp:render :material 'damaged-helmet))

  ("ground"
   (fl.comp:transform :translate (v3:vec 0 -2 0.1))
   (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:collider/sphere :display-id "Ground"
                            :visualize t
                            :on-layer :ground
                            :center (v3:zero)
                            :radius 1)
   (fl.comp:render :material '2d-wood)))

(fl:define-prefab "collision-test-1" (:library examples)
  "This test demonstrates that at frame 0 colliders that should be colliding
actually are. You have to view the results to see the colliders lighting up."

  (("camera" :copy "/cameras/perspective")
   (fl.comp:camera (:policy :new-args) :zoom 7))

  ("upper-left"
   (fl.comp:transform :translate (v3:vec -2 2 -0.1))
   (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Upper-Left"
                            :visualize t
                            :on-layer :ground))
  ("upper-right"
   (fl.comp:transform :translate (v3:vec 2 2 -0.1))
   (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Upper-Right"
                            :visualize t
                            :on-layer :ground))
  ("lower-left"
   (fl.comp:transform :translate (v3:vec -2 -2 -0.1))
   (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Lower-Left"
                            :visualize t
                            :on-layer :ground))
  ("lower-right"
   (fl.comp:transform :translate (v3:vec 2 -2 -0.1))
   (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Lower-Right"
                            :visualize t
                            :on-layer :ground))
  ("stone"
   (fl.comp:transform :translate (v3:zero)
                      :scale 2
                      :rotate (q:orient :local :x (/ pi 2))
                      :rotate/inc (q:orient :local (v3:one) pi)
                      :translate/inc (v3:zero))
   (fl.comp:static-mesh :location '(:mesh "damaged-helmet.glb"))
   (destroy-my-actor :time-to-destroy 2)
   (fl.comp:collider/sphere :display-id "Stone"
                            :visualize t
                            :on-layer :player
                            :center (v3:zero)
                            :radius 1)
   (fl.comp:render :material 'damaged-helmet)))

;;; Prefab descriptors

(fl:define-prefab-descriptor collision-smoke-test ()
  ("collision-smoke-test" fl.example:examples))

(fl:define-prefab-descriptor collision-test-0 ()
  ("collision-test-0" fl.example:examples))

(fl:define-prefab-descriptor collision-test-1 ()
  ("collision-test-1" fl.example:examples))
