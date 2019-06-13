(in-package #:first-light.example)


;; "Protect the Planets!" by Peter Keller (psilord@cs.wisc.edu)
;; Requirements: gamepad, preferably xbox one like, linux, gtx 660 or better.
;;
;; left stick controls movement, right-stick controls orientation
;;

;; The various categories don't have to be defined in this order or in a single
;; file.

;; TODO: Pixel_Outlaw says to use an egg shape for a shot pattern:
;; http://i.imgur.com/J6oKb1E.png
;; And here is some math that can help:
;; http://www.mathematische-basteleien.de/eggcurves.htm

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

;; These two textures were created by Pixel_Outlaw for use in this game.
(fl:define-texture warning-wave (:texture-2d)
  (:data #((:lgj-04/2019 "warning-wave.tiff"))))

(fl:define-texture warning-mothership (:texture-2d)
  (:data #((:lgj-04/2019 "warning-mothership.tiff"))))

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
              (:mix-color (v4:one)))))

(fl:define-material warning-mothership
  (:profiles (fl.materials:u-mvp)
   :shader fl.gpu.texture:unlit-texture
   :uniforms ((:tex.sampler1 'warning-mothership)
              (:mix-color (v4:one))
              #++(:min-intensity (v4:make 0.1 0.1 0.1 0))
              #++(:max-intensity (v4:one)))))

(fl:define-material warning-wave
  (:profiles (fl.materials:u-mvp)
   :shader fl.gpu.texture:unlit-texture
   :uniforms ((:tex.sampler1 'warning-wave)
              (:mix-color (v4:one))
              #++(:min-intensity (v3:make 0.1 0.1 0.1 1))
              #++(:max-intensity (v4:one)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random Types we need, some will go into FL properly in a future date
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass region ()
  ((%center :accessor center
            :initarg :center
            :initform (v3:make 0.0 0.0 0.0))))

(defclass region-cuboid (region)
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

(defun make-region-cuboid (center minx maxx miny maxy minz maxz)
  (make-instance 'region-cuboid
                 :center center
                 :minx minx :maxx maxx
                 :miny miny :maxy maxy
                 :minz minz :maxz maxz))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions some will go into FL in a future date.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clip-movement-vector (movement-vector current-translation region-cuboid)
  "Clip the MOVEMENT-VECTOR by an amount that will cause it to not violate the
REGION-CUBOID when MOVEMENT-VECTOR is added to the CURRENT-TRANSLATION.
Return a newly allocated and adjusted MOVEMENT-VECTOR."

  (with-accessors ((center center)
                   (minx minx) (maxx maxx) (miny miny) (maxy maxy)
                   (minz minz) (maxz maxz))
      region-cuboid
    (v3:with-components ((c current-translation)
                         (m movement-vector))
      ;; add the movement-vector to the current-translation
      (let* ((nx (+ cx mx))
             (ny (+ cy my))
             (nz (+ cz mz))
             ;; And the default adjustments to the movement-vector
             (adj-x 0)
             (adj-y 0)
             (adj-z 0))
        ;; Then if it violates the boundary cube, compute the adjustment we
        ;; need to the movement vector to fix it.
        (let ((offset-minx (+ (v3:x center) minx))
              (offset-maxx (+ (v3:x center) maxx))
              (offset-miny (+ (v3:y center) miny))
              (offset-maxy (+ (v3:y center) maxy))
              (offset-minz (+ (v3:z center) minz))
              (offset-maxz (+ (v3:z center) maxz)))
          (when (< nx offset-minx)
            (setf adj-x (- offset-minx nx)))

          (when (> nx offset-maxx)
            (setf adj-x (- offset-maxx nx)))

          (when (< ny offset-miny)
            (setf adj-y (- offset-miny ny)))

          (when (> ny offset-maxy)
            (setf adj-y (- offset-maxy ny)))

          (when (< nz offset-minz)
            (setf adj-z (- offset-minz nz)))

          (when (> nz offset-maxz)
            (setf adj-z (- offset-maxz nz)))

          ;; NOTE: Allocates memory.
          (v3:make (+ mx adj-x) (+ my adj-y) (+ mz adj-z)))))))

(defun quat->euler (quat)
  (flet ((copysign (x y)
           (let ((x (abs x))
                 (sign (signum y)))
             (if (= sign -1)
                 (* -1 x)
                 x))))

    (q:with-components ((q quat))
      (let* (;; Roll (x-axis)
             (sinr_cosp (* 2.0 (+ (* qw qx) (* qy qz))))
             (cosr_cosp (- 1.0 (* 2.0 (+ (* qx qx) (* qy qy)))))
             (roll (atan sinr_cosp cosr_cosp))
             ;; Pitch (y-axis)
             (sinp (* 2.0 (- (* qw qy) (* qz qx))))
             (pitch (if (>= (abs sinp) 1)
                        (copysign (/ pi 2) sinp)
                        (asin sinp)))
             ;; Yaw (z-axis)
             (siny_cosp (* 2.0 (+ (* qw qz) (* qx qy))))
             (cosy_cosp (- 1.0 (* 2.0 (+ (* qy qy) (* qz qz)))))
             (yaw (atan siny_cosp cosy_cosp)))

        (v3:make roll pitch yaw)))))

(defun orient<- (style &rest axis/angles)
  "Compute a RIGHT TO LEFT composite rotation of the plist of AXIS/ANGLE
specifications.

AXIS/ANGLES is a p-list of AXIS and ANGLE values repeating after one another.

An AXIS can be :X, :Y, :Z, or a v3 representing the axis of rotation.

ANGLE is a number in radians.

Together an AXIS/ANGLE combination represents a rotation of an angle around an
axis.

An example call: (orient<- :x (/ pi 4) (v3:make 1.0 1.0 1.0) (/ pi 2) :y pi)
Which means:
  Build a quaternion that represents the final state of:
    First, rotate pi around the Y axis first,
    Second, rotate (/ pi 2) around the #(1.0 1.0 1.0) axis,
    Third, rotate (/ pi 4) around the :X axis.

The returned value is a single quaternion representing the final state of the
entire sequence of right to left applied axis/angle rotations."

  (let ((total-quat-rotation (q:id))
        (rotation-axis (v3:make 0.0 0.0 0.0))
        (current-quat (q:id)))

    ;; TODO: Fix to handle destructuring errors or other types of errors.
    (loop :for (axis angle) :on axis/angles :by #'cddr
          :do (v3:with-components ((v rotation-axis))
                (case axis
                  (:x
                   (psetf vx 1.0
                          vy 0.0
                          vz 0.0))
                  (:y
                   (psetf vx 0.0
                          vy 1.0
                          vz 0.0))
                  (:z
                   (psetf vx 0.0
                          vy 0.0
                          vz 1.0))
                  (otherwise
                   ;; TODO: Typecheck, or otherwise try and figure out quickly,
                   ;; that we passed in a v3 representing the axis itself.
                   (v3:with-components ((a axis))
                     (psetf vx ax
                            vy ay
                            vz az))))

                ;; We rotate around the UNIT axis vector!
                (v3:normalize! rotation-axis rotation-axis)

                ;; Make the individual quaternion rotation to represent the
                ;; axis/angle representation.
                ;; TODO: Why is this not in the quaternion API?
                (q:with-components ((c current-quat))
                  (let* ((angle/2 (/ angle 2.0))
                         (ca2 (float (cos angle/2) 1f0))
                         (sa2 (float (sin angle/2) 1f0)))
                    (psetf cw ca2
                           cx (* vx sa2)
                           cy (* vy sa2)
                           cz (* vz sa2)))))

              ;; Then update the accumulating rotation carefully minding the
              ;; multiplication order to ensure the final rotation we compute
              ;; applies in _right to left_ order. NOTE: That means the order
              ;; for this multiply should be total <- total * current because
              ;; we're using a reduction like pass to _associate_ left to right
              ;; the multiplications. But, since we're keeping the ultimate
              ;; order of the applications the same as the argument order, the
              ;; final rotation applies right to left.
              (ecase style
                (:local
                  (q:*! total-quat-rotation current-quat total-quat-rotation))
                (:world
                 (q:*! total-quat-rotation total-quat-rotation current-quat)))
              ;; And ensure it is normalizes for the next concatenation....
              (q:normalize! total-quat-rotation total-quat-rotation))

    total-quat-rotation))

(defun orient-> (style &rest axis/angles)
  "The ORIENT<- function is preferred over this one.

This function does the same thing as ORIENT<- except the AXIS/ANGLES are applied
in LEFT to RIGHT ordering."
  (apply #'orient<-
         style
         (au:flatten
          (nreverse
           (loop :for (axis angle) :on axis/angles :by #'cddr
                 :collect (list axis angle))))))

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
   ;; We just hack in a boundary cube you can't go outside of. This is in the
   ;; local space of the actor to which this component is attached.
   ;; The format is minx, maxx, miny, maxy, minz, maxz
   (region-cuboid :default (make-region-cuboid (v3:make 0.0 0.0 0.0)
                                               -900 900 -500 500 0 0))))

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
                   (region-cuboid region-cuboid))
      self
    (au:mvlet* ((lx ly (fl.input:get-gamepad-analog (fl:input-data context)
                                                    '(:gamepad1 :left-stick)))
                (instant-p (zerop (fl:frame-count context))))

      ;; First, we settle the notion of how the player translates around with
      ;; left stick
      (let* (;; Dead with deadzones and other bad data around the input vector.
             (vec (v3:make lx ly 0))
             (vec (if (> (v3:length vec) 1) (v3:normalize vec) vec))
             (vec (if (< (v3:length vec) translate-deadzone) (v3:zero) vec))
             ;; Compute the actual translation vector related to our frame time!
             (vec (v3:scale vec
                            (float (* max-velocity (fl:frame-time context))
                                   1f0)))
             ;; and ensure we clip the translation vector so we can't go out of
             ;; the boundary cube we set.
             (current-translation
               ;; TODO NOTE: Prolly should fix these to be external.
               (fl.comp::current (fl.comp::translation transform)))
             (vec (clip-movement-vector vec current-translation region-cuboid)))

        (fl.comp:translate transform vec))

      ;; Then we settle the notion of how the player is oriented.  We're setting
      ;; a hard angle of rotation each time so we overwrite the previous value.
      (unless (or (= lx ly 0.0) (< (v3:length (v3:make lx ly 0)) rotate-deadzone))
        (let* ((angle (atan (- lx) ly))
               (angle (if (< angle 0)
                          (+ pi (- pi (abs angle)))
                          angle)))
          (fl.comp:rotate transform
                          (v3:make 0 0 angle)
                          :replace-p t
                          :instant-p instant-p))))))

;; ;;;;;;;;;
;; Component: line-mover
;;
;; Move something in a straight line along a specified cardinal axis. This is
;; not a sophisticated component. We use it to move projectiles.
;;;;;;;;;

(fl:define-component line-mover ()
  ((direction :default :+y) ;; NOTE: may be :+y :-y :+x :-x :+z :-z or a vec3
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
              (x (m4:rotation-axis-to-vec3 local :x))
              (y (m4:rotation-axis-to-vec3 local :y))
              (z (m4:rotation-axis-to-vec3 local :z))
              (a (v3:normalize
                  (if (symbolp direction)
                      (ecase direction
                        (:+x x)
                        (:-x (v3:negate x))
                        (:+y y)
                        (:-y (v3:negate y))
                        (:+z z)
                        (:-z (v3:negate z)))
                      direction)))
              (move-delta (* velocity (fl:delta context))))
         (v3:scale a move-delta))))))

;; ;;;;;;;;;
;; Component: damage-points
;;
;; We describe an amount of damage that we give to something under appropriate
;; conditions during a collision. No need for any methods since this is only a
;; repository of information consulted by the game as it is needed for each
;; appropriate actor that has it.
;; ;;;;;;;;;

(fl:define-component damage-points ()
  ((dp :default 1)))

;; ;;;;;;;;;
;; Component: hit-points
;;
;; Manage a number of hit-points for something and destroy my actor if I fall
;; to zero or below.
;;;;;;;;;

(fl:define-component hit-points ()
  ((hp :default 1)
   (invulnerability-timer :default 0)))

(defmethod fl:on-component-update ((self hit-points))
  (with-accessors ((context fl:context)
                   (invulnerability-timer invulnerability-timer))
      self
    (when (> invulnerability-timer 0)
      (decf invulnerability-timer (fl:frame-time context))
      (when (<= invulnerability-timer 0)
        (setf invulnerability-timer 0)))))

(defmethod fl:on-collision-enter ((self hit-points) other-collider)
  (with-accessors ((context fl:context)
                   (invulnerability-timer invulnerability-timer))
      self
    (let ((other-damage-points (fl:actor-component-by-type
                                (fl:actor other-collider)
                                'damage-points)))

      (when (> invulnerability-timer 0)
        ;; If we're invulnerable, we cannot take damage.
        (return-from fl:on-collision-enter nil))

      (cond
        (other-damage-points
         (decf (hp self) (dp other-damage-points)))
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
               (parent-translation (m4:get-translation parent-model))
               (parent-rotation (q:from-mat4 parent-model))
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
   (sprite :default nil)
   (action :default nil)))

;; we use this at runtime to instantiate a bullet prefab and fill in everything
;; it needs to become effective in the world.
(defun make-projectile (context translation rotation physics-layer
                        &key
                          (scale (v3:make 1.0 1.0 1.0))
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
    ;; And adjust the scale too.
    (fl.comp:scale projectile-transform scale
                   :instant-p t :replace-p t)

    (setf
     ;; Basic identification of the projectile
     (name projectile) name
     (frames projectile) frames
     ;; Give the collider a cheesy name until I get rid of this name feature.
     (fl:display-id (collider projectile)) name
     ;; Set what layer is the collider on?
     ;; TODO: When setting this, ensure I move the collider to the right
     ;; layer in the physics system.
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
                (parent-translation (m4:get-translation parent-model)))
           (unless (or (= rx ry 0.0)
                       (< (v3:length (v3:make rx ry 0)) rotate-deadzone))
             (let* ((angle (atan (- rx) ry))
                    (angle (if (< angle 0)
                               (+ pi (- pi (abs angle)))
                               angle)))
               ;; The rotation we use is indicated by the right stick vector.
               (make-projectile context
                                parent-translation
                                (v3:make 0 0 angle)
                                (physics-layer self)
                                :velocity 2000
                                :name (name self)
                                :frames (frames self)))))))

      ;; Just accumulate more time until we know we can fire again.
      (t
       (incf cooldown-time (fl:frame-time context))))))

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
   (difficulty-time :default 0)
   (asteroid-db :default #(("asteroid01-01" 16)
                           ("asteroid02-01" 16)
                           ("asteroid03-01" 16)
                           ("asteroid04-01" 16)
                           ("asteroid05-01" 16)
                           ("asteroid06-01" 16)))
   (scale-range :default (v2:make .75 1.25))))


(defmethod fl:on-component-update ((self asteroid-field))
  (with-accessors ((spawn-period spawn-period)
                   (cooldown-time cooldown-time)
                   (difficulty difficulty)
                   (difficulty-period difficulty-period)
                   (difficulty-time difficulty-time)
                   (pause-p pause-p)
                   (context fl:context)
                   (asteroid-db asteroid-db)
                   (scale-range scale-range))
      self

    (when pause-p
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
                     (v3:make (ransign 300.0) (ransign 300.0) 0.1)))
                  (quadrant (random 4)))

             ;; pick an origin point in director space and convert it to world
             ;; space
             (setf origin
                   (fl.comp:transform-point
                    transform
                    (case quadrant
                      (0 ;; left side
                       (v3:make -1000.0 (ransign 600.0) 0.1))
                      (1 ;; top side
                       (v3:make (ransign 1000.0) 600.0 0.1))
                      (2 ;; right side
                       (v3:make 1000.0 (ransign 600.0) 0.1))
                      (3 ;; bottom side
                       (v3:make (ransign 1000.0) -600.0 0.1)))))

             (destructuring-bind (name frames)
                 (aref asteroid-db (random (length asteroid-db)))
               (let* ((uniform-scale
                        (+ (aref scale-range 0)
                           (random (- (float (aref scale-range 1) 1.0)
                                      (float (aref scale-range 0) 1.0))))))
                 (make-projectile context
                                  origin
                                  (v3:zero)
                                  :enemy-bullet
                                  :velocity  (ransign 50 400)
                                  ;; this direction is in world space.
                                  ;; it moves from the origin to the target.
                                  :direction (v3:normalize (v3:- target origin))
                                  :scale (v3:make uniform-scale
                                                  uniform-scale
                                                  uniform-scale)
                                  :name name
                                  :frames frames
                                  :destroy-ttl 4)))))

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

;; ;;;;;;;;;
;; Component: player-stable
;;
;; This component manages a stable of player ships, subtracting or adding to
;; that stable as the player dies (or perhaps gets 1ups).
;; ;;;;;;;;;

;; TODO: This component will have a much different form when we're able to
;; enable/disable actors.

(fl:define-component player-stable ()
  ((max-lives :default 3)
   (lives-remaining :default 3)
   (stable :default nil)
   ;; TODO: make prefab-descriptor external (and possibly also have a function
   ;; variant. Also have it return a vector instead of a list, more useful
   ;; for indexing.
   (mockette-prefab :default (fl.prefab::prefab-descriptor
                               ("player-ship-mockette" lgj-04/2019)))
   ;; We keep references to all of the mockettes in an array that matches their
   ;; position on the screen so we can destroy them or re-create them as the
   ;; player dies or gets 1ups.
   (mockette-refs :default nil)
   ;; Next one is do increaseing lives grow :left or :right from the origin?
   ;; THis is to support two or more player.
   (direction :default :right)
   ;; How far to place the mockette origins from each other.
   (width-increment :default 100)))

(defmethod fl:on-component-initialize ((self player-stable))
  (setf (mockette-refs self)
        (make-array (max-lives self) :initial-element nil)))

(defun make-mockette (player-stable mockette-index)
  (with-accessors ((mockette-prefab mockette-prefab)
                   (mockette-refs mockette-refs)
                   (width-increment width-increment)
                   (direction direction)
                   (stable stable))
      player-stable
    (let* ((dir (ecase direction (:left -1) (:right 1)))
           (mockette (first
                      (fl:make-prefab-instance
                       (%fl::core (fl:context player-stable))
                       mockette-prefab
                       :parent stable)))
           (transform (fl:actor-component-by-type mockette 'fl.comp:transform)))

      (fl.comp:translate
       transform (v3:make (* mockette-index (* dir width-increment)) -60 0))

      (setf (aref mockette-refs mockette-index) mockette))))

;; This is useful if you want to use the prefab in a player1 or player2
;; configuration. Just change the direction and regenerate it.
(defun regenerate-lives (player-stable)
  (with-accessors ((lives-remaining lives-remaining)
                   (mockette-refs mockette-refs))
      player-stable
    ;; Initialize the stable under the holder with player-remaining mockettes.
    (dotimes (life lives-remaining)
      (when (aref mockette-refs life)
        (fl:destroy (aref mockette-refs life)))
      (make-mockette player-stable life))))


(defmethod fl:on-component-attach ((self player-stable) actor)
  (declare (ignore actor))
  (regenerate-lives self))

(defun consume-life (player-stable)
  "Remove a life from the stable (which removes it from the display too).
If there are no more lives to remove, return NIL. If there was a life to remove,
return the lives-remaining after the life has been consumed."
  (with-accessors ((lives-remaining lives-remaining)
                   (mockette-refs mockette-refs)
                   (stable stable))
      player-stable

    (when (zerop lives-remaining)
      (return-from consume-life NIL))

    ;; lives-remaining is indexed by one, but the mockettes are indexed by zero.
    (let ((mockette-index (1- lives-remaining)))
      (fl:destroy (aref mockette-refs mockette-index))
      (setf (aref mockette-refs mockette-index) nil)
      (decf lives-remaining))
    lives-remaining))

(defun add-life (player-stable)
  (with-accessors ((max-lives max-lives)
                   (lives-remaining lives-remaining)
                   (mockette-prefab mockette-prefab)
                   (mockette-holder mockette-holder)
                   (mockette-refs mockette-refs)
                   (width-increment width-increment)
                   (stable stable))
      player-stable

    (when (eql lives-remaining max-lives)
      ;; You don't get more than max-lives in this implementation.
      (return-from add-life NIL))

    (incf lives-remaining)
    (make-mockette player-stable (1- lives-remaining))))

;; ;;;;;;;;;
;; Component: Tags (candidate component for core FL)
;;
;; This component is a registration system for tag symbols that we can insert
;; into this component. This allows us to locate all actors with a certain tag,
;; or query if an actor currently has a certain tag.
;;
;; ;;;;;;;;;

(fl:define-component tags ()
  (;; TODO: Technically, this is for initialization only.  I should rename it.
   ;; Also the tags only are present if this component is attached to something.
   ;; So, when looking for "does this component have this tag" I shouldn't just
   ;; MEMBER it out of this list, cause that isn't the point.
   (tags :default nil))

  (;; In this shared storage, we keep an association between a tag and the actor
   ;; set which uses it, and form each actor to the tag set it has. This allows
   ;; extremely fast lookups in either direction.
   (:db eq)))

;; private API (probably)
(defun tags-refs (context)
  ;; Create the DB if not present.
  (fl:with-shared-storage
      (context context)
      ((tag->actors tag->actors/present-p ('tags :db :tag->actors)
                    ;; key: tag, Value: hash table of actor -> actor
                    (au:dict #'eq))
       (actor->tags actor->tags/present-p ('tags :db :actor->tags)
                    ;; Key: actor, Value: hash table of tag -> tag
                    (au:dict #'eq)))

    (values tag->actors actor->tags)))

(defmethod fl:on-component-initialize ((self tags))
  ;; Seed the shared storage cache.
  ;; We're modifying the tags list, so copy-seq it to ensure it isn't a quoted
  ;; list.
  (setf (tags self) (copy-seq (tags self)))
  (tags-refs (fl:context self)))

;; private API
(defun %tags-add (self &rest adding-tags)
  (with-accessors ((context fl:context)
                   (actor fl:actor))
      self
    (au:mvlet ((tag->actors actor->tags (tags-refs context)))
      (dolist (tag adding-tags)
        ;; Add a tag -> actor set link
        (unless (au:href tag->actors tag)
          (setf (au:href tag->actors tag) (au:dict #'eq)))
        (setf (au:href tag->actors tag actor) actor)

        ;; Add an actor -> tag set link
        (unless (au:href actor->tags actor)
          (setf (au:href actor->tags actor) (au:dict #'eq)))
        (setf (au:href actor->tags actor tag) tag)))))

;; public API
(defun tags-add (self &rest tags)
  "Uniquely add a set of TAGS to the current tags in the SELF instance which
must be a TAGS component."
  (dolist (tag tags)
    (pushnew tag (tags self)))
  (apply #'%tags-add tags))

;; private API
(defun %tags-remove (self &rest removing-tags)
  (with-accessors ((context fl:context)
                   (actor fl:actor))
      self
    (au:mvlet ((tag->actors actor->tags (tags-refs context)))
      (dolist (tag removing-tags)
        ;; Remove the tag -> actor set link
        (remhash tag (au:href actor->tags actor))
        (when (zerop (hash-table-count (au:href actor->tags actor)))
          (remhash actor actor->tags))

        ;; Remove the actor -> tag set link
        (remhash actor (au:href tag->actors tag))
        (when (zerop (hash-table-count (au:href tag->actors tag)))
          (remhash tag tag->actors))))))

;; public API
(defun tags-remove (self &rest tags)
  "Remove all specified tags from the set of TAGS from the current tags in the
SELF instance which must be a TAGS component."
  (dolist (tag tags)
    (setf (tags self) (remove tag (tags self))))
  (apply #'%tags-remove tags))

;; public API
(defmethod tags-has-tag-p ((self tags) query-tag)
  "Return T if the SELF tags component contains the QUERY-TAG."
  (with-accessors ((context fl:context)
                   (actor fl:actor))
      self
    (au:mvlet ((tag->actors actor->tags (tags-refs context)))
      (declare (ignore tag->actors))
      (au:href actor->tags actor query-tag))))

;; public API
(defmethod tags-has-tag-p ((self fl:actor) query-tag)
  "Return T if there is a tags component on the SELF actor and it also
contains the QUERY-TAG."
  (au:when-let ((tags-component (fl:actor-component-by-type self 'tags)))
    (tags-has-tag-p tags-component query-tag)))

;; public API
(defun tags-find-actors-with-tag (context query-tag)
  "Return a list of actors that are tagged with the QUERY-TAG. Return
NIL if no such list exists."
  (au:mvlet ((tag->actors actor->tags (tags-refs context)))
    (declare (ignore actor->tags))
    (au:when-let ((actors (au:href tag->actors query-tag)))
      (au:hash-keys actors))))

(defmethod fl:on-component-attach ((self tags) actor)
  (with-accessors ((context fl:context)
                   (actor fl:actor)
                   (tags tags))
      self
    (dolist (tag tags)
      (%tags-add self tag))))

(defmethod fl:on-component-detach ((self tags) actor)
  ;; NOTE: all components are detached before they are destroyed.
  (with-accessors ((context fl:context)
                   (actor fl:actor)
                   (tags tags))
      self
    (dolist (tag tags)
      (%tags-remove self tag))))

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
;; Component: director
;;
;; The director component runs the entire game, ensuring to move between
;; states of the game, notice and replace the player when they die
;;;;;;;;;

(fl:define-component director ()
  (;; The director manages the overall state the game is in, plus it
   ;; micromanages the playing state to ensure fun ensues.
   ;;
   ;; state-machine:
   ;; :waiting-to-play -> :playing -> :game-over -> :initials-entry
   ;;    ^                                              |
   ;;    |                                              |
   ;;    +----------------------------------------------+
   (current-game-state :default :waiting-to-play)
   (levels :default (fl.prefab::prefab-descriptor
                      ("level-0" lgj-04/2019)
                      ("level-1" lgj-04/2019)
                      ("level-2" lgj-04/2019)))
   ;; which level is considered the demo level.
   (demo-level :default 0)
   ;; Which level are we playing?
   (current-level :default 0)
   ;; This is where the current level is instanced and stored as a child
   (level-holder :default NIL)
   ;; The stable from which we know we can get another player.
   (player-1-stable :default nil)
   ;; When we instantiate a player, this is the place it goes.
   (current-player-holder :default NIL)))

(defmethod fl:on-component-update ((self director))
  (with-accessors ((current-game-state current-game-state)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (level-holder level-holder)
                   (current-player-holder current-player-holder))
      self
    (setf current-game-state
          (process-director-state self current-game-state))))

;; each method returns the new state it should do for the next update.
(defgeneric process-director-state (director state))

(defmethod process-director-state ((self director)
                                   (state (eql :waiting-to-play)))
  (declare (ignore state))
  (with-accessors ((current-game-state current-game-state)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (level-holder level-holder)
                   (current-player-holder current-player-holder))
      self
    ;; 1. Spawn the demo-level
    ;; 2. Flash "Press Start to Play" sign
    ;; 3. Listen for Start button.
    ;; 4. When start button is pressed, destroy the level
    ;; and transition to :playing state.

    ;; TOOD: For now, skip this and transition directly to :playing.
    :playing))

(defmethod process-director-state ((self director)
                                   (state (eql :playing)))
  (declare (ignore state))
  (with-accessors ((context fl:context)
                   (current-game-state current-game-state)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (level-holder level-holder)
                   (player-1-stable player-1-stable)
                   (current-player-holder current-player-holder))
      self
    ;; TODO: Not done yet.
    ;; 1. If the current-level is not loaded, load it.

    ;; 2. load a new player if need be
    (let ((player-alive-p (tags-find-actors-with-tag context :player)))
      (if (not player-alive-p)
          ;; Ask the player-stable to see if I can get a new player.
          (let ((player-life (consume-life player-1-stable)))
            (cond
              (player-life
               (let ((new-player-instance
                       (first (fl:make-prefab-instance
                               (%fl::core context)
                               '(("player-ship" lgj-04/2019))
                               :parent current-player-holder))))
                 ;; TODO: we don't use new-player-instance yet. If it looks
                 ;; like I don't need to, then remove this binding.
                 (declare (ignore new-player-instance))
                 :playing))
              (t
               ;; No new players, game over!
               :game-over)))

          :playing))))

(defmethod process-director-state ((self director)
                                   (state (eql :game-over)))
  (declare (ignore state))
  (with-accessors ((current-game-state current-game-state)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (level-holder level-holder)
                   (current-player-holder current-player-holder))
      self
    :game-over))

(defmethod process-director-state ((self director)
                                   (state (eql :initials-entry)))
  (declare (ignore state))
  (with-accessors ((current-game-state current-game-state)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (level-holder level-holder)
                   (current-player-holder current-player-holder))
      self
    :initials-entry))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefabs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fl:define-prefab "projectile" (:library lgj-04/2019)
  "A generic projectile that can be a bullet, or an asteroid, or whatever."

  (projectile :transform (fl:ref :self :component 'fl.comp:transform)
              :mover (fl:ref :self :component 'line-mover)
              :collider (fl:ref :self :component 'fl.comp:collider/sphere)
              :sprite (fl:ref :self :component 'fl.comp:sprite)
              :action (fl:ref :self :component 'fl.comp:actions))
  (damage-points :dp 1)
  (explosion :name "explode01-01" :frames 15)
  (hit-points :hp 1)
  (line-mover)
  (fl.comp:sprite :spec :spritesheet-data)
  (fl.comp:collider/sphere :center (v3:zero)
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
  (tags :tags '(:player))
  (explosion :name "explode01-01" :frames 15)
  (damage-points :dp 1)
  (hit-points :hp 1
              ;; When the player is born, they are automatically invulnerable
              ;; for 1 second.
              :invulnerability-timer 1)
  (player-movement)
  (fl.comp:collider/sphere :center (v3:zero)
                           :on-layer :player
                           :referent (fl:ref :self :component 'hit-points)
                           :radius 30)
  ("ship-body"
   (fl.comp:sprite :spec :spritesheet-data
                   :name "ship26")
   (fl.comp:render :material 'sprite-sheet
                   :mode :sprite)
   ("center-gun"
    (fl.comp:transform :translate (v3:zero))
    (gun :physics-layer :player-bullet :name "bullet01" :frames 2))

   ("exhaust"
    (fl.comp:transform :translate (v3:make 0 -60 0))
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


(fl:define-prefab "player-stable" (:library lgj-04/2019)
  ;; TODO: Clarify when we actually need the / infront of the actor name during
  ;; use of FL:REF. Here is seems we DON'T need one, but sometimes we do!
  (player-stable :stable (fl:ref "stable-holder"))
  ("stable-holder"))

(fl:define-prefab "generic-planet" (:library lgj-04/2019)
  (planet)
  (hit-points :hp 5)
  (fl.comp:collider/sphere :center (v3:zero)
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

(fl:define-prefab "warning-wave-sign" (:library lgj-04/2019)
  ("sign"
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'warning-wave)))

(fl:define-prefab "warning-mothership-sign" (:library lgj-04/2019)
  ("sign"
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'warning-mothership)))

(fl:define-prefab "starfield" (:library lgj-04/2019)
  ("bug-todo:implicit-transform:see-trello"
   (fl.comp:transform :scale (v3:make 960 960 960)
                      ;; NOTE: ortho projection, so we can put starfield way
                      ;; back.
                      :translate (v3:make 0 0 -100))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'starfield)))


(fl:define-prefab "level-0" (:library lgj-04/2019)
  (asteroid-field)
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 0 100 -1)
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01")))


(fl:define-prefab "level-1" (:library lgj-04/2019)
  (asteroid-field)
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make -200 100 -1)
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01"))
  (("planet-1" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 200 100 -1)
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet02")))

(fl:define-prefab "level-2" (:library lgj-04/2019)
  (asteroid-field)
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 0 100 -1)
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01"))
  (("planet-1" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make -200 -100 -1)
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet02"))
  (("planet-2" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 200 -100 -1)
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet03")))

(fl:define-prefab "protect-the-planets" (:library lgj-04/2019)
  "The top most level prefab which has the component which drives the game
sequencing."
  (fl.comp:transform :scale (v3:one))
  (director :level-holder (fl:ref "/protect-the-planets/active-level")
            :player-1-stable (fl:ref "/protect-the-planets/player-1-stable"
                                     :component 'player-stable))

  ;; The component that controls how many players we have left and how to
  ;; visualize that.
  (("player-1-stable" :link ("/player-stable" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make -900 550 -10)))

  #++(("WARNING" :copy ("/warning-wave-sign" :from lgj-04/2019))
      (fl.comp:transform :translate (v3:make 0 0 10)
                         :scale (v3:make 500 500 500)))

  (("camera" :copy ("/cameras/ortho" :from fl.example::examples))
   (fl.comp:transform :translate (v3:make 0 0 500)))

  ("active-level")


  #++(("player-ship" :link ("/player-ship" :from lgj-04/2019)))

  (("current-level" :copy ("/level-0" :from lgj-04/2019))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefab descriptors for convenience
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fl:define-prefab-descriptor lgj-04/2019 ()
  ("protect-the-planets" fl.example::lgj-04/2019))
