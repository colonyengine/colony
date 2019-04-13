(in-package :first-light.example)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is not really a general purpose component. It is just here to help out
;; testing how destruction and colliders work together.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fl:define-component destroy-my-actor ()
  ((time-to-destroy :default 5)))

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

(fl:define-prefab "collision-smoke-test" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (fl.comp:camera (:policy :new-args) :zoom 6))
  ("rot-0-center"
   (fl.comp:transform :translate (m:vec3 -2 0 0)
                      :rotate/inc (m:vec3 0 0 pi))
   ("plane-0"
    (fl.comp:transform :translate (m:vec3 -2 0 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:collider/sphere
     :display-id "Player"
     :on-layer :player
     :center (m:vec3)
     :radius 1)
    (fl.comp:render :material '2d-wood)))
  ("rot-1-center"
   (fl.comp:transform :translate (m:vec3 2 0 0)
                      :rotate/inc (m:vec3 0 0 (- pi)))
   ("plane-1"
    (fl.comp:transform :translate (m:vec3 2 0 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:collider/sphere
     :display-id "Enemy"
     :on-layer :enemy
     :center (m:vec3)
     :radius 1)
    (fl.comp:render :material '2d-wood))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing getting the directions from a transform
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fl:define-component debug-transform ()
  ((done-once :default nil)))

(defmethod fl:on-component-physics-update ((self debug-transform))
  (let* ((actor (fl:actor self))
         (actor-transform
           (fl:actor-component-by-type actor 'fl.comp:transform)))
    (unless (done-once self)
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
        (unless (and (m:~ forward (m:vec3 0 0 -1))
                     (m:~ backward (m:vec3 0 0 1))
                     (m:~ up (m:vec3 0 1 0))
                     (m:~ down (m:vec3 0 -1 0))
                     (m:~ right (m:vec3 1 0 0))
                     (m:~ left (m:vec3 -1 0 0)))
          (error "The Transform API didn't match expectations!"))
        (setf (done-once self) t)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The test prefabs.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fl:define-prefab "collision-transform-test" (:library examples)
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
   (fl.comp:transform :translate (m:vec3 5 0 0))
   (debug-transform)
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)))



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
   (fl.comp:transform :translate (m:vec3 -1.15 2 -.1))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Left-Gate"
                            :on-layer :ground))

  ("right-gate"
   (fl.comp:transform :translate (m:vec3 1.15 2 -.1))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Right-Gate"
                            :on-layer :ground))

  ("stone"
   (fl.comp:transform :translate (m:vec3 0 5 0)
                      :scale (m:vec3 .5)
                      :rotate (m:vec3 (/ 2 pi 2) 0 0)
                      :rotate/inc (m:vec3 (/ pi -2) (/ pi -2) (/ pi -2))
                      :translate/inc (m:vec3 0 -2 0))
   (fl.comp:mesh :location '(:mesh "damaged-helmet.glb"))
   (destroy-my-actor :display-id "destroy-my-actor: stone")
   (fl.comp:collider/sphere :display-id "Stone"
                            :on-layer :player
                            :referent (fl:ref :self
                                              :component 'destroy-my-actor)
                            :center (m:vec3)
                            :radius 1)
   (fl.comp:render :material 'damaged-helmet))

  ("ground"
   (fl.comp:transform :translate (m:vec3 0 -2 .1))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:collider/sphere :display-id "Ground"
                            :on-layer :ground
                            :center (m:vec3)
                            :radius 1)
   (fl.comp:render :material '2d-wood)))

(fl:define-prefab "collision-test-1" (:library examples)
  "This test demonstrates that at frame 0 colliders that should be colliding
actually are. You have to view the results to see the colliders lighting up."

  (("camera" :copy "/cameras/perspective")
   (fl.comp:camera (:policy :new-args) :zoom 7))

  ("upper-left"
   (fl.comp:transform :translate (m:vec3 -2 2 -.1))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Upper-Left"
                            :on-layer :ground))
  ("upper-right"
   (fl.comp:transform :translate (m:vec3 2 2 -.1))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Upper-Right"
                            :on-layer :ground))
  ("lower-left"
   (fl.comp:transform :translate (m:vec3 -2 -2 -.1))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Lower-Left"
                            :on-layer :ground))
  ("lower-right"
   (fl.comp:transform :translate (m:vec3 2 -2 -.1))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material '2d-wood)
   (fl.comp:collider/sphere :display-id "Lower-Right"
                            :on-layer :ground))
  ("stone"
   (fl.comp:transform :translate (m:vec3 0 0 0)
                      :scale (m:vec3 2)
                      :rotate (m:vec3 (/ 2 pi 2) 0 0)
                      :rotate/inc (m:vec3 (/ pi -2) (/ pi -2) (/ pi -2))
                      :translate/inc (m:vec3 0 0 0))
   (fl.comp:mesh :location '(:mesh "damaged-helmet.glb"))
   (destroy-my-actor :time-to-destroy 2)
   (fl.comp:collider/sphere :display-id "Stone"
                            :on-layer :player
                            :center (m:vec3)
                            :radius 1)
   (fl.comp:render :material 'damaged-helmet)))
