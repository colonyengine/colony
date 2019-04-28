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

      ;; Then we settle the notion of how the player rotates around with right
      ;; stick. We're setting a hard angle of rotation each time so we overwrite
      ;; the previous value.
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
  ((axis :default 1) ;; NOTE: 0 is x, 1 is y, 2 is z.
   (transform :default nil)
   (velocity :default 0)))

(defmethod fl:on-component-attach ((self line-mover) actor)
  (declare (ignore actor))
  (with-accessors ((actor fl:actor) (transform transform)) self
    (setf transform (fl:actor-component-by-type actor 'fl.comp:transform))))

(defmethod fl:on-component-physics-update ((self line-mover))
  (with-accessors ((context fl:context) (transform transform)
                   (velocity velocity) (axis axis))
      self
    ;; TODO: Figure out why I need this when physics is faster. It appears I
    ;; can compute a physics frame before components are attached?
    (when transform
      (fl.comp:translate
       transform
       (let ((a (m:normalize (m:vec3 (m:get-column (fl.comp:local transform)
                                                   axis))))
             (move-delta (* velocity (fl:delta context))))
         (m:* a move-delta))))))

;; ;;;;;;;;;
;; Component: projectile
;;
;; This describes a projectile (bullet, asteroid, etc) and its API to set it up
;; when it spawns.
;;;;;;;;;

(fl:define-component projectile ()
  ((name :default nil)
   (frames :default 0)
   (hit-points :default 1)
   (transform :default nil)
   (collider :default nil)
   (mover :default nil)
   (sprite :default nil)))

;; If the bullet hit anything else according to the physics system, it dies.
(defmethod fl:on-collision-enter ((self projectile) other-collider)
  (fl:destroy (fl:actor self)))

;; we use this at runtime to instantiate a bullet prefab and fill in everything
;; it needs to become effective in the world.
(defun make-projectile (context translation rotation physics-layer
                        &key (destroy-ttl 2) (velocity 1000)
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
     (fl.comp::frames (sprite projectile)) frames)

    ;; By default projectiles live a certain amount of time.
    (fl:destroy-after-time new-projectile :ttl destroy-ttl)

    new-projectile))


;; ;;;;;;;;;
;; Component: gun
;;
;; This fires the specified projectiles
;;;;;;;;;

(fl:define-component gun ()
  ((emitter-transform :default nil)
   (physics-layer :default nil)
   (rotate-deadzone :default .1)
   (fire-period :default 20);; hz
   ;; Below keeps track of how much time passed since we fired last.
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
               ;; The rotation we use is indicated by the stick vector.
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



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefabs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(fl:define-prefab "projectile" (:library lgj-04/2019)
  "A generic projectile that can be a bullet, or an asteroid, or whatever."

  (projectile :transform (fl:ref :self :component 'fl.comp:transform)
              :mover (fl:ref :self :component 'line-mover)
              :collider (fl:ref :self :component 'fl.comp:collider/sphere)
              :sprite (fl:ref :self :component 'fl.comp:sprite))
  (line-mover)
  (fl.comp:sprite :spec :spritesheet-data)
  (fl.comp:collider/sphere :center (m:vec3)
                           :referent (fl:ref :self :component 'projectile)
                           :radius 10)

  (fl.comp:render :material 'sprite-sheet
                  :mode :sprite)
  (fl.comp:actions :default-actions '((:type fl.actions:sprite-animate
                                       :duration 0.5
                                       :repeat-p t))))

(fl:define-prefab "player-ship" (:library lgj-04/2019)
  (player-movement)
  (fl.comp:collider/sphere :center (m:vec3)
                           :on-layer :player
                           :radius 30)
  ("ship-body"
   (fl.comp:sprite :spec :spritesheet-data
                   :name "ship26")
   (fl.comp:render :material 'sprite-sheet
                   :mode :sprite)
   ("center-gun"
    (fl.comp:transform :translate (m:vec3 0 0 0))
    (gun :physics-layer :player :name "bullet01" :frames 2))

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
  (fl.comp:sprite :spec :spritesheet-data
                  :name "planet01")
  (fl.comp:render :material 'sprite-sheet
                  :mode :sprite))


(fl:define-prefab "starfield" (:library lgj-04/2019)
  ("bug-todo:implicit-transform:see-trello"
   (fl.comp:transform :scale (m:vec3 960)
                      ;; NOTE: ortho projection, so we can put starfield way
                      ;; back.
                      :translate (m:vec3 0 0 -100))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'starfield)))


(fl:define-prefab "level-0" (:library lgj-04/2019)
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("remaining-player-ships" :link ("/remaining-player-ships" :from
                                                              lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 -900 550 -10)))
  (("player-ship" :link ("/player-ship" :from lgj-04/2019)))
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 0 100 -1)
                      :scale (m:vec3 .9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01")))


(fl:define-prefab "level-1" (:library lgj-04/2019)
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("remaining-player-ships" :link ("/remaining-player-ships" :from
                                                              lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 -900 550 -10)))
  (("player-ship" :link ("/player-ship" :from lgj-04/2019)))
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
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("remaining-player-ships" :link ("/remaining-player-ships" :from
                                                              lgj-04/2019))
   (fl.comp:transform :translate (m:vec3 -900 550 -10)))
  (("player-ship" :link ("/player-ship" :from lgj-04/2019)))
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
  (("camera" :copy ("/cameras/ortho" :from fl.example::examples)))
  (("current-level" :copy ("/level-2" :from lgj-04/2019))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefab descriptors for convenience
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fl:define-prefab-descriptor player-ship ()
  ("player-ship" fl.example::lgj-04/2019))

(fl:define-prefab-descriptor lgj-04/2019 ()
  ("protect-the-planets" fl.example::lgj-04/2019))
