(in-package :first-light.example)


;; "Protect the Planets!" by Peter Keller (psilord@cs.wisc.edu)
;; Requirements: gamepad, preferably xbox one like, linux, gtx 660 or better.
;;
;; left stick controls movement, right-stick controls orientation
;;

;; The various categories don't have to be defined in this order or in a single
;; file.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textures
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We only use a single sprite sheet atlas that contains all of our textures.
;; This one is invluded with FL
(fl:define-texture sprite-atlas (:texture-2d)
  (:data #(:spritesheet)))

;; This background image was downloaded off the web here:
;; https://www.wikitree.com/photo/jpg/Tileable_Background_Images
;; And the url for the license is 404, but the wayback machine found it:
;; https://web.archive.org/web/20180723233810/http://webtreats.mysitemyway.com/terms-of-use/
;; Which says it can be used for any purpose.
(fl:define-texture starfield (:texture-2d)
  (:data #((:lgj-04/2019 "starfield.tiff"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Materials
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A material is pre-packaged set of values for a particular shader. Different
;; materials can be made for the same shader each providing diferent inputs to
;; that shader.
(fl:define-material sprite-sheet
  (:profiles (fl.materials:u-mvp)
   :shader fl.gpu.sprite:sprite
   :uniforms ((:sprite.sampler 'sprite-atlas) ;; refer to the above texture.
              (:opacity 1.0)
              (:alpha-cutoff 0.1))
   :blocks ((:block-name :spritesheet
             :storage-type :buffer
             :block-alias :spritesheet
             :binding-policy :manual))))

(fl:define-material starfield
  (:profiles (fl.materials:u-mvpt)
   :shader fl.gpu.user:starfield
   :uniforms ((:tex 'fl.example::starfield)
              (:mix-color (m:vec4 1 1 1 1)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random Types we need, some will go into FL properly in a future date
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass boundary-cube ()
  ((%minx :accessor minx
          :initarg :minx
          :initform 0)
   (%maxx :accessor maxx
          :initarg :maxx
          :initform 0)
   (%miny :accessor miny
          :initarg :miny
          :initform 0)
   (%maxy :accessor maxy
          :initarg :maxy
          :initform 0)
   (%minz :accessor minz
          :initarg :minz
          :initform 0)
   (%maxz :accessor maxz
          :initarg :maxz
          :initform 0)))

(defun make-boundary-cube (minx maxx miny maxy minz maxz)
  (make-instance 'boundary-cube
                 :minx minx :maxx maxx
                 :miny miny :maxy maxy
                 :minz minz :maxz maxz))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions some will go into FL in a future date.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clip-movement-vector (movement-vector current-translation boundary-cube)
  "Clip the MOVEMENT-VECTOR by an amount that will cause it to not violate the
BOUNDARY-CUBE when MOVEMENT-VECTOR is added to the CURRENT-TRANSLATION.
Return a newly allocated and adjusted MOVEMENT-VECTOR."

  (with-accessors ((minx minx) (maxx maxx) (miny miny) (maxy maxy)
                   (minz minz) (maxz maxz))
      boundary-cube
    (m:with-vec3 ((c current-translation))
      (m:with-vec3 ((m movement-vector))
        ;; add the movement-vector to the current-translation
        (let* ((nx (+ c.x m.x))
               (ny (+ c.y m.y))
               (nz (+ c.z m.z))
               ;; And the default adjustments to the movement-vector
               (adj-x 0)
               (adj-y 0)
               (adj-z 0))
          ;; Then if it violates the boundary cube, compute the adjustment we
          ;; need to the movement vector to fix it.
          (when (< nx minx)
            (setf adj-x (- minx nx)))

          (when (> nx maxx)
            (setf adj-x (- maxx nx)))

          (when (< ny miny)
            (setf adj-y (- miny ny)))

          (when (> ny maxy)
            (setf adj-y (- maxy ny)))

          (when (< nz minz)
            (setf adj-z (- minz nz)))

          (when (> nz maxz)
            (setf adj-z (- maxz nz)))

          ;; NOTE: Allocates memory.
          (m:vec3 (+ m.x adj-x) (+ m.y adj-y) (+ m.z adj-z)))))))

(defun quat->euler (quat)
  #|
  // roll (x-axis rotation)
  double sinr_cosp = +2.0 * (q.w() * q.x() + q.y() * q.z());
  double cosr_cosp = +1.0 - 2.0 * (q.x() * q.x() + q.y() * q.y());
  roll = atan2(sinr_cosp, cosr_cosp);

  // pitch (y-axis rotation)
  double sinp = +2.0 * (q.w() * q.y() - q.z() * q.x());
  if (fabs(sinp) >= 1)
  pitch = copysign(M_PI / 2, sinp); // use 90 degrees if out of range
  else
  pitch = asin(sinp);

  // yaw (z-axis rotation)
  double siny_cosp = +2.0 * (q.w() * q.z() + q.x() * q.y());
  double cosy_cosp = +1.0 - 2.0 * (q.y() * q.y() + q.z() * q.z());
  yaw = atan2(siny_cosp, cosy_cosp);
  |#
  (flet ((copysign (x y)
           (let ((x (abs x))
                 (sign (signum y)))
             (if (= sign -1)
                 (* -1 x)
                 x))))

    (m:with-quat ((q quat))
      (let* (;; Roll (x-axis)
             (sinr_cosp (* 2.0 (+ (* q.w q.x) (* q.y q.z))))
             (cosr_cosp (- 1.0 (* 2.0 (+ (* q.x q.x) (* q.y q.y)))))
             (roll (atan sinr_cosp cosr_cosp))
             ;; Pitch (y-axis)
             (sinp (* 2.0 (- (* q.w q.y) (* q.z q.x))))
             (pitch (if (>= (abs sinp) 1)
                        (copysign (/ pi 2) sinp)
                        (asin sinp)))
             ;; Yaw (z-axis)
             (siny_cosp (* 2.0 (+ (* q.w q.z) (* q.x q.y))))
             (cosy_cosp (- 1.0 (* 2.0 (+ (* q.y q.y) (* q.z q.z)))))
             (yaw (atan siny_cosp cosy_cosp)))

        (m:vec3 roll pitch yaw)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Components
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;
;; Component: player-movement
;;
;; First we need a component that allows the player to move around.
;; We're going to left the left joystick place the player and the right one
;; orient the player.
;; ;;;;;;;;;

(fl:define-component player-movement ()
  ((transform :default nil)
   (max-velocity :default 1500)
   (translate-deadzone :default .1)
   (rotate-deadzone :default .1)
   ;; We just hack in a boundary cube you can't go outside of.
   ;; The format is minx, maxx, miny, maxy, minz, maxz
   (boundary-cube :default (make-boundary-cube -900 900 -500 500 0 0))))

;; upon attaching, this component will store find the transform component
;; on the actor to which it has been attached and keep a direct reference to it.
(defmethod fl:on-component-attach ((self player-movement) actor)
  (declare (ignore actor))
  (with-accessors ((actor fl:actor) (transform transform)) self
    (setf transform (fl:actor-component-by-type actor 'transform))))


(defmethod fl:on-component-update ((self player-movement))
  (with-accessors ((context fl:context) (transform transform)
                   (max-velocity max-velocity)
                   (translate-deadzone translate-deadzone)
                   (rotate-deadzone rotate-deadzone)
                   (boundary-cube boundary-cube))
      self
    (au:mvlet* ((lx ly (fl.input:get-gamepad-analog (fl:input-data context)
                                                    '(:gamepad1 :left-stick)))
                (instant-p (zerop (fl:frame-count context))))

      ;; First, we settle the notion of how the player translates around with
      ;; left stick
      (let* (;; Dead with deadzones and other bad data around the input vector.
             (vec (m:vec3 lx ly 0))
             (vec (if (> (m:length vec) 1) (m:normalize vec) vec))
             (vec (if (< (m:length vec) translate-deadzone) (m:vec3) vec))
             ;; Compute the actual translation vector related to our frame time!
             (vec (m:* vec (* max-velocity (fl:frame-time context))))
             ;; and ensure we clip the translation vector so we can't go out of
             ;; the boundary cube we set.
             (current-translation
               ;; TODO NOTE: Prolly should fix these to be external.
               (fl.comp::current (fl.comp::translation transform)))
             (vec (clip-movement-vector vec current-translation boundary-cube)))

        (fl.comp:translate transform vec))

      ;; Then we settle the notion of how the player is oriented.  We're setting
      ;; a hard angle of rotation each time so we overwrite the previous value.
      (unless (or (= lx ly 0.0) (< (m:length (m:vec3 lx ly 0)) rotate-deadzone))
        (let* ((angle (atan (- lx) ly))
               (angle (if (< angle 0)
                          (+ pi (- pi (abs angle)))
                          angle)))
          (fl.comp:rotate transform
                          (m:vec3 0 0 angle)
                          :replace-p t
                          :instant-p instant-p))))))

;; ;;;;;;;;;
;; Component: line-mover
;;
;; Move something in a straight line along a specified cardinal axis. This is
;; not a sophisticated component. We use it to move projectiles.
;;;;;;;;;

(fl:define-component line-mover ()
  ((direction :default :+y) ;; NOTE: may be :+y :-y :+x :-x :+z :-z or an m:vec3
   (transform :default nil)
   (velocity :default 0)))

(defmethod fl:on-component-attach ((self line-mover) actor)
  (declare (ignore actor))
  (with-accessors ((actor fl:actor) (transform transform)) self
    (setf transform (fl:actor-component-by-type actor 'fl.comp:transform))))

(defmethod fl:on-component-physics-update ((self line-mover))
  (with-accessors ((context fl:context)
                   (transform transform)
                   (velocity velocity)
                   (direction direction))
      self
    ;; TODO: Figure out why I need this when physics is faster. It appears I
    ;; can compute a physics frame before components are attached?
    (when transform
      (fl.comp:translate
       transform
       (let* ((local (fl.comp:local transform))
              (a (m:normalize
                  (if (symbolp direction)
                      (m:vec3
                       (case direction
                         (:+x (m:get-column local 0))
                         (:-x (m:negate
                               (m:get-column local 0)))
                         (:+y (m:get-column local 1))
                         (:-y (m:negate
                               (m:get-column local 1)))
                         (:+z (m:get-column local 2))
                         (:-z (m:negate
                               (m:get-column local 2)))))
                      direction)))
              (move-delta (* velocity (fl:delta context))))
         (m:* a move-delta))))))

;; ;;;;;;;;;
;; Component: hit-points
;;
;; Manage a number of hit-points for something and destroy my actor if I fall
;; to zero or below.
;;;;;;;;;

(fl:define-component hit-points ()
  ((hp :default 1)))

(defmethod fl:on-collision-enter ((self hit-points) other-collider)
  (with-accessors ((context fl:context)) self
    (let ((other-hit-points (fl:actor-component-by-type
                             (fl:actor other-collider)
                             'hit-points)))
      (cond
        (other-hit-points
         (decf (hp self) (hp other-hit-points)))
        (t
         ;; The physics system discovered a hit, but the thing that hit us
         ;; doesn't have a hit-points component, so we'll assume one point
         ;; of damage arbitrarily.
         (decf (hp self) 1)))

      ;; Now, do the consequences.
      (when (<= (hp self) 0)
        ;; Destroy my actor.
        (fl:destroy (fl:actor self))
        ;; And, if the actor had an explosion component, cause the explosion
        ;; to happen
        (let* ((actor-transform (fl:actor-component-by-type (fl:actor self)
                                                            'fl.comp:transform))
               (parent-model (fl.comp:model actor-transform))
               (parent-translation (m:get-translation parent-model))
               (parent-rotation (m:quat parent-model))
               (explosion (fl:actor-component-by-type (fl:actor self)
                                                      'explosion)))
          (when explosion
            (make-explosion context
                            parent-translation
                            (quat->euler parent-rotation)
                            :name (name explosion)
                            :frames (frames explosion))))))))


;; ;;;;;;;;;
;; Component: projectile
;;
;; This describes a projectile (bullet, asteroid, etc) and its API to set it up
;; when it spawns.
;;;;;;;;;

(fl:define-component projectile ()
  ((name :default nil)
   (frames :default 0)
   (transform :default nil)
   (collider :default nil)
   (mover :default nil)
   (sprite :default nil)))

;; we use this at runtime to instantiate a bullet prefab and fill in everything
;; it needs to become effective in the world.
(defun make-projectile (context translation rotation physics-layer
                        &key
                          (destroy-ttl 2)
                          (velocity 1000)
                          (direction :+y)
                          (prefab-name "projectile")
                          (prefab-library 'lgj-04/2019)
                          (name "bullet01")
                          (frames 1))
  (let* ((new-projectile
           ;; TODO: This API needs to take a context. prefab-descriptor prolly
           ;; needs to be a function, not a macro.
           (first (fl:make-prefab-instance
                   (%fl::core context)
                   ;; TODO: prefab-descriptor is wrong here.
                   `((,prefab-name ,prefab-library)))))
         ;; TODO: I'm expecting the new-projectile to have components here
         ;; without having gone through the flow. BAD!
         (projectile-transform (fl:actor-component-by-type new-projectile
                                                           'fl.comp:transform))
         (projectile (fl:actor-component-by-type new-projectile 'projectile)))

    ;; Set the spatial configuration
    (fl.comp:translate projectile-transform translation
                       :instant-p t :replace-p t)
    ;; XXX This interface needs to take a quat here also
    (fl.comp:rotate projectile-transform rotation
                    :instant-p t :replace-p t)

    (setf
     ;; Basic identification of the projectile
     (name projectile) name
     (frames projectile) frames
     ;; Give the collider a cheesy name until I get rid of this name feature.
     (fl:display-id (collider projectile)) name
     ;; Set what layer is the collider on?
     ;; TODO: When setting this, ensure I move the collider to the right
     ;; layer in the physics system. XXXXXXXXXX
     (fl.comp:on-layer (collider projectile)) physics-layer
     ;; How fast is the projectile going?
     (velocity (mover projectile)) velocity
     ;; Tell the sprite what it should be rendering
     ;; TODO: make NAME and FRAMES public for sprite component.
     (fl.comp::name (sprite projectile)) name
     (fl.comp::frames (sprite projectile)) frames

     ;; and, what direction is it going in?
     (direction (mover projectile)) direction)

    ;; By default projectiles live a certain amount of time.
    (fl:destroy-after-time new-projectile :ttl destroy-ttl)

    new-projectile))


;; ;;;;;;;;;
;; Component: explosion
;;
;; Describe an explosion.
;;;;;;;;;

(fl:define-component explosion ()
  ((sprite :default nil)
   (name :default "explode01-01")
   (frames :default 15)))

;; NOTE: No physics layers for this since they don't even participate in the
;; collisions.
(defun make-explosion (context translation rotation
                       &key (destroy-ttl 2)
                         (prefab-name "generic-explosion")
                         (prefab-library 'lgj-04/2019)
                         (name "explode01-01")
                         (frames 15))
  (let* ((new-explosion
           (first (fl:make-prefab-instance
                   (%fl::core context)
                   ;; TODO: prefab-descriptor is wrong here.
                   `((,prefab-name ,prefab-library)))))
         (explosion-transform (fl:actor-component-by-type new-explosion
                                                          'fl.comp:transform))
         (explosion (fl:actor-component-by-type new-explosion 'explosion)))

    (setf
     ;; Configure the sprite.
     (fl.comp::name (sprite explosion)) name
     (fl.comp::frames (sprite explosion)) frames)

    (fl.comp:translate explosion-transform translation
                       :instant-p t :replace-p t)
    (fl.comp:rotate explosion-transform rotation
                    :instant-p t :replace-p t)

    ;; By default explosions live a certain amount of time.
    (fl:destroy-after-time new-explosion :ttl destroy-ttl)

    new-explosion))

;; ;;;;;;;;;
;; Component: gun
;;
;; This fires the specified projectiles
;;;;;;;;;

(fl:define-component gun ()
  ((emitter-transform :default nil)
   (physics-layer :default nil)
   (rotate-deadzone :default .1)
   (fire-period :default 25);; hz
   ;; Keeps track of how much time passed since we fired last.
   (cooldown-time :default 0)
   ;; name and frames of projectile to fire.
   (name :default "bullet01")
   ;; TODO: Bug, if I put 1 here, I get the WRONG sprite sometimes.
   (frames :default 2)))

(defmethod fl:on-component-attach ((self gun) actor)
  (declare (ignore actor))
  (with-accessors ((actor fl:actor) (emitter-transform emitter-transform)) self
    (setf emitter-transform (fl:actor-component-by-type
                             actor 'fl.comp:transform))))

(defmethod fl:on-component-update ((self gun))
  (with-accessors ((context fl:context)
                   (rotate-deadzone rotate-deadzone)
                   (fire-period fire-period)
                   (cooldown-time cooldown-time)
                   (emitter-transform emitter-transform))
      self

    ;; TODO: I could make a macro to do this syntax work, like WITH-TIMER, but I
    ;; think a names registrant of a function into the component that is called
    ;; repeatedly is better since enabling and disabling from elsewhere becomes
    ;; possible. Will think about for later.
    (cond
      ;; We exceeded our cooldown time, so time to fire!
      ((>= cooldown-time (/ fire-period))
       ;; We keep track of time such that we're most accurate in time that we
       ;; next need to fire.
       (loop :while (>= cooldown-time (/ fire-period))
             :do (decf cooldown-time (/ fire-period)))

       (au:mvlet* ((rx ry (fl.input:get-gamepad-analog
                           (fl:input-data context) '(:gamepad1 :right-stick))))
         (let* ((parent-model (fl.comp:model emitter-transform))
                (parent-translation (m:get-translation parent-model)))
           (unless (or (= rx ry 0.0)
                       (< (m:length (m:vec3 rx ry 0)) rotate-deadzone))
             (let* ((angle (atan (- rx) ry))
                    (angle (if (< angle 0)
                               (+ pi (- pi (abs angle)))
                               angle)))
               ;; The rotation we use is indicated by the right stick vector.
               (make-projectile context
                                parent-translation
                                (m:vec3 0 0 angle)
                                (physics-layer self)
                                :velocity 2000
                                :name (name self)
                                :frames (frames self)))))))

      ;; Just accumulate more time until we know we can fire again.
      (t
       (incf cooldown-time (fl:frame-time context))))))

;; ;;;;;;;;;
;; Component: planet
;;
;; Handles hit point management of the planet, plus animations/behavior when it
;; is about to be destroyed.
;;;;;;;;;

(fl:define-component planet ()
  ;; TODO: Can't have nil slots, fix it so we can.
  ((junk :default 0)))

;; ;;;;;;;;;
;; Component: asteroid-field
;;
;; The asteroid field simply fires asteroids at the planet in an ever increasing
;; difficulty.
;;;;;;;;;

(fl:define-component asteroid-field ()
  ((pause-p :default nil)
   (spawn-period :default 1) ;; hz
   (cooldown-time :default 0)
   (difficulty :default 1)
   (difficulty-period :default 1/10) ;; Hz
   (difficulty-time :default 0)))


(defmethod fl:on-component-update ((self asteroid-field))
  (with-accessors ((spawn-period spawn-period)
                   (cooldown-time cooldown-time)
                   (difficulty difficulty)
                   (difficulty-period difficulty-period)
                   (difficulty-time difficulty-time)
                   (pause-p pause-p)
                   (context fl:context))
      self

    (when pause-p
      ;;
      (return-from fl:on-component-update))

    (let ((transform
            (fl:actor-component-by-type (fl:actor self) 'fl.comp:transform)))
      (flet ((ransign (val &optional (offset 0))
               (+ (* (random (if (zerop val) 1 val))
                     (if (zerop (random 2)) 1 -1))
                  offset)))
        (cond
          ((>= cooldown-time (/ (* spawn-period difficulty)))
           (loop :while (>= cooldown-time (/ (* spawn-period difficulty)))
                 :do (decf cooldown-time (/ (* spawn-period difficulty))))

           ;; Find a spot offscreen to start the asteroid
           (let* (origin
                  ;; The target point picked out of the center box in director
                  ;; space that we'll convert to world space.
                  ;;
                  ;; TODO: abstract this to ue a boundary cube.
                  (target
                    (fl.comp:transform-point
                     transform
                     (m:vec3 (ransign 300.0) (ransign 300.0) .1)))
                  (quadrant (random 4)))

             ;; pick an origin point in director space and convert it to world
             ;; space
             (setf origin
                   (fl.comp:transform-point
                    transform
                    (case quadrant
                      (0 ;; left side
                       (m:vec3 -1000.0 (ransign 600.0) .1))
                      (1 ;; top side
                       (m:vec3 (ransign 1000.0) 600.0 .1))
                      (2 ;; right side
                       (m:vec3 1000.0 (ransign 600.0) .1))
                      (3 ;; bottom side
                       (m:vec3 (ransign 1000.0) -600.0 .1)))))

             (make-projectile context
                              origin
                              (m:vec3)
                              :enemy-bullet
                              :velocity  (ransign 50 400)
                              ;; this direction is in world space.
                              ;; it moves from the origin to the target.
                              :direction (m:normalize (m:- target origin))
                              :name "asteroid01-01"
                              :frames 16
                              :destroy-ttl 4)))

          (t
           (incf cooldown-time (fl:frame-time context))))

        ;; Now increase difficulty!
        (cond
          ((>= difficulty-time (/ difficulty-period))
           (loop :while (>= difficulty-time (/ difficulty-period))
                 :do (decf difficulty-time (/ difficulty-period)))

           (incf difficulty 1))

          (t
           (incf difficulty-time (fl:frame-time context))))))))






;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefabs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fl:define-prefab "projectile" (:library lgj-04/2019)
  "A generic projectile that can be a bullet, or an asteroid, or whatever."

  (projectile :transform (fl:ref :self :component 'fl.comp:transform)
              :mover (fl:ref :self :component 'line-mover)
              :collider (fl:ref :self :component 'fl.comp:collider/sphere)
              :sprite (fl:ref :self :component 'fl.comp:sprite))
  (explosion :name "explode01-01" :frames 15)
  (hit-points :hp 1)
  (line-mover)
  (fl.comp:sprite :spec :spritesheet-data)
  (fl.comp:collider/sphere :center (m:vec3)
                           :on-layer :enemy-bullet
                           :referent (fl:ref :self :component 'hit-points)
                           :radius 15)

  (fl.comp:render :material 'sprite-sheet
                  :mode :sprite)
  (fl.comp:actions :default-actions '((:type fl.actions:sprite-animate
                                       :duration 0.5
                                       :repeat-p t))))

(fl:define-prefab "player-ship" (:library lgj-04/2019)
  "The venerable Player Ship. Controls how it looks, collides, and movement."
  (explosion :name "explode01-01" :frames 15)
  (hit-points :hp 1)
  (player-movement)
  (fl.comp:collider/sphere :center (m:vec3)
                           :on-layer :player
                           :referent (fl:ref :self :component 'hit-points)
                           :radius 30)
  ("ship-body"
   (fl.comp:sprite :spec :spritesheet-data
                   :name "ship26")
   (fl.comp:render :material 'sprite-sheet
                   :mode :sprite)
   ("center-gun"
    (fl.comp:transform :translate (m:vec3 0 0 0))
    (gun :physics-layer :player-bullet :name "bullet01" :frames 2))

   ("exhaust"
    (fl.comp:transform :translate (m:vec3 0 -60 0))
    (fl.comp:sprite :spec :spritesheet-data
                    :name "exhaust03-01"
                    :frames 8)
    (fl.comp:render :material 'sprite-sheet
                    :mode :sprite)
    (fl.comp:actions :default-actions '((:type fl.actions:sprite-animate
                                         :duration 0.5
                                         :repeat-p t))))))

(fl:define-prefab "player-ship-mockette" (:library lgj-04/2019)
  "An image of the ship, but no colliders or guns."
  (fl.comp:sprite :spec :spritesheet-data
                  :name "ship26")
  (fl.comp:render :material 'sprite-sheet
                  :mode :sprite))


(fl:define-prefab "remaining-player-ships" (:library lgj-04/2019)
  "These are the unused remaining player ships. They are subtraced from
once the player dies. When they are all gone, the game is over."
  (("player-ship-1" :link ("/player-ship-mockette" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 200 -60 0)))
  (("player-ship-2" :link ("/player-ship-mockette" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 100 -60 0)))
  (("player-ship-3" :link ("/player-ship-mockette" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 0 -60 0))))

(fl:define-prefab "generic-planet" (:library lgj-04/2019)
  (planet)
  (hit-points :hp 5)
  (fl.comp:collider/sphere :center (m:vec3)
                           :on-layer :planet
                           :radius 145)
  (fl.comp:sprite :spec :spritesheet-data
                  :name "planet01")
  (fl.comp:render :material 'sprite-sheet
                  :mode :sprite)
  ;; This has a bug when using a single frame.
  #++(fl.comp:actions :default-actions '((:type fl.actions:sprite-animate
                                          :duration 0.5
                                          :repeat-p t))))

(fl:define-prefab "generic-explosion" (:library lgj-04/2019)
  (explosion :sprite (fl:ref :self :component 'fl.comp:sprite))
  (fl.comp:sprite :spec :spritesheet-data
                  ;; TODO: When this is misnamed, the error is extremely obscure
                  :name "explode01-01"
                  :frames 15)
  (fl.comp:render :material 'sprite-sheet
                  :mode :sprite)
  (fl.comp:actions :default-actions '((:type fl.actions:sprite-animate
                                       :duration 0.5
                                       :repeat-p nil))))

(fl:define-prefab "asteroid-test" (:library lgj-04/2019)
  (hit-points :hp 5)
  (explosion :name "explode04-01" :frames 15)
  (fl.comp:collider/sphere :center (m:vec3)
                           :on-layer :enemy
                           :referent (fl:ref :self :component 'hit-points)
                           :radius 20)
  (fl.comp:sprite :spec :spritesheet-data
                  :name "asteroid01-01")
  (fl.comp:render :material 'sprite-sheet
                  :mode :sprite)
  (fl.comp:actions :default-actions '((:type fl.actions:sprite-animate
                                       :duration 0.5
                                       :repeat-p t))))

(fl:define-prefab "starfield" (:library lgj-04/2019)
  ("bug-todo:implicit-transform:see-trello"
   (fl.comp:transform :scale (m:vec3 960)
                      ;; NOTE: ortho projection, so we can put starfield way
                      ;; back.
                      :translate (m:vec3 0 0 -100))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'starfield)))


(fl:define-prefab "level-0" (:library lgj-04/2019)
  (asteroid-field)
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 0 100 -1)
                      :scale (m:vec3 .9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01")))


(fl:define-prefab "level-1" (:library lgj-04/2019)
  (asteroid-field)
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 -200 100 -1)
                      :scale (m:vec3 .9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01"))
  (("planet-1" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 200 100 -1)
                      :scale (m:vec3 .9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet02")))

(fl:define-prefab "level-2" (:library lgj-04/2019)
  (asteroid-field)
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 0 100 -1)
                      :scale (m:vec3 .9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01"))
  (("planet-1" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 -200 -100 -1)
                      :scale (m:vec3 .9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet02"))
  (("planet-2" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 200 -100 -1)
                      :scale (m:vec3 .9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet03")))

(fl:define-prefab "protect-the-planets" (:library lgj-04/2019)
  "The top most level prefab which has the component which drives the game
sequencing."
  (fl.comp:transform :scale (m:vec3 1))
  (("camera" :copy ("/cameras/ortho" :from fl.example::examples))
   (fl.comp:transform :translate (m:vec3 0 0 500)))
  (("remaining-player-ships" :link ("/remaining-player-ships" :from
                                                              lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 -900 550 -10)))
  (("player-ship" :link ("/player-ship" :from lgj-04/2019)))
  (("current-level" :copy ("/level-0" :from lgj-04/2019))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefab descriptors for convenience
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fl:define-prefab-descriptor lgj-04/2019 ()
  ("protect-the-planets" fl.example::lgj-04/2019))
