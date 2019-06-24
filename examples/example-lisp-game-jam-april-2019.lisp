(in-package #:first-light.example)

;; "Protect the Planets!"
;; by Peter Keller (psilord@cs.wisc.edu)
;; With significant contributions by: Michael Fiano (mail@michaelfiano.com)
;;
;; Requirements: gamepad, preferably xbox/ps4 like, linux, gtx 660 or better but
;; nvidia gpus are not specifically required.
;;
;; Controls:
;; left stick controls movement,
;; right-stick controls shooting and direction of shooting,
;; right trigger slows movement.

;; The commented delimited categories group the code into various sections.  The
;; FL related sections, like Textures, Materials, Components, etc, generally
;; don't have to be in this order and don't have to be in a single file. They
;; can be spread sround and in any order you like. BUT--Components must be
;; defined before the Prefabs that use it.


;; TODO: Pixel_Outlaw says to use an egg shape for a shot pattern:
;; http://i.imgur.com/J6oKb1E.png
;; And here is some math that can help:
;; http://www.mathematische-basteleien.de/eggcurves.htm

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: Because we don't yet have mesh/sprite rendering order in first-light,
;; or order independent transparency tools, etc yet, we'll need to define were
;; things exist in layers perpendicular to the orthographic camera so they can
;; be rendered in order according to the zbuffer. This also means no
;; translucency since the rendering can happen in any order. Stencil textures
;; are ok though.  NOTE: We must be careful here since things that collide with
;; each other must actually be physically close together in the game.
(defparameter *draw-layer* (au:dict #'eq
                                    :starfield -100f0
                                    :player-stable -99f0

                                    :planet -.07f0
                                    :planet-explosion -.06f0
                                    :asteroid -.05f0
                                    :enemy-ship -.04f0
                                    :enemy-explosion -.03f0
                                    :enemy-bullet -.02f0
                                    :player-bullet -.01f0
                                    :player 0.00f0
                                    :player-explosion 0.01f0

                                    :sign 400f0
                                    :camera 500f0
                                    ))
(defun dl (draw-layer-name)
  (au:href *draw-layer* draw-layer-name))


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

(fl:define-texture game-over (:texture-2d)
  (:data #((:lgj-04/2019 "game-over.tiff"))))

(fl:define-texture title (:texture-2d)
  (:data #((:lgj-04/2019 "title.tiff"))))

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

(fl:define-material title
  (:profiles (fl.materials:u-mvp)
   :shader fl.gpu.texture:unlit-texture-decal
   :uniforms ((:tex.sampler1 'title)
              (:min-intensity (v4:make 0f0 0f0 0f0 .5f0))
              (:max-intensity (v4:one)))))

(fl:define-material starfield
  (:profiles (fl.materials:u-mvpt)
   :shader fl.gpu.user:starfield
   :uniforms ((:tex 'fl.example::starfield)
              (:mix-color (v4:one)))))

(fl:define-material warning-mothership
  (:profiles (fl.materials:u-mvp)
   :shader fl.gpu.texture:unlit-texture-decal
   :uniforms ((:tex.sampler1 'warning-mothership)
              (:min-intensity (v4:make 0f0 0f0 0f0 .5f0))
              (:max-intensity (v4:one)))))

(fl:define-material warning-wave
  (:profiles (fl.materials:u-mvp)
   :shader fl.gpu.texture:unlit-texture-decal
   :uniforms ((:tex.sampler1 'warning-wave)
              (:min-intensity (v4:make 0f0 0f0 0f0 .5f0))
              (:max-intensity (v4:one)))))

(fl:define-material game-over
  (:profiles (fl.materials:u-mvp)
   :shader fl.gpu.texture:unlit-texture-decal
   :uniforms ((:tex.sampler1 'game-over)
              (:min-intensity (v4:make 0f0 0f0 0f0 .5f0))
              (:max-intensity (v4:one)))))

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

;; TODO: Request to put into vec3 package, make one for vec2, and vec4 too.
(defmacro raw-dot (lx ly lz rx ry rz)
  "Return a scalar value that is the dot product of the left vector and the
right vector."
  `(cl:+ (cl:* ,lx ,rx) (cl:* ,ly ,ry) (cl:* ,lz ,rz)))

;; TODO: Request to put into quaternion package, we need this like 5 other
;; places too.
(defmacro raw-quaternion-multiply (ow ox oy oz lqw lqx lqy lqz rqw rqx rqy rqz)
  ;; TODO: Figure out of I did a once only, how does the assembly look and
  ;; will it add any additional instructions. If so, keep it like it is.
  "Return a quaternion multiplication form to psetf OW OX OY OZ with the
result of the multiplication of a left quaternion, represented as
LQW LQX LQY LQZ, with a right quaternion, represented as RQW RQX RQY RQZ.
This exapnsion does NOT perform once only bindings of the input forms
in the interest of speed. So, if you need that you must do it before the
macro is expanded."
  `(psetf ,ow (cl:- (cl:* ,lqw ,rqw) (cl:* ,lqx ,rqx) (cl:* ,lqy ,rqy)
                    (cl:* ,lqz ,rqz))
          ,ox (cl:- (cl:+ (cl:* ,lqw ,rqx) (cl:* ,lqx ,rqw) (cl:* ,lqy ,rqz))
                    (cl:* ,lqz ,rqy))
          ,oy (cl:- (cl:+ (cl:* ,lqw ,rqy) (cl:* ,lqy ,rqw) (cl:* ,lqz ,rqx))
                    (cl:* ,lqx ,rqz))
          ,oz (cl:- (cl:+ (cl:* ,lqw ,rqz) (cl:* ,lqz ,rqw) (cl:* ,lqx ,rqy))
                    (cl:* ,lqy ,rqx))))

;; TODO: Request to put this into the quaternion package.
(defun orient<-! (out style &rest axis/angles)
  "Compute a RIGHT to LEFT composite rotation of the plist of AXIS/ANGLE
specifications. RIGHT to LEFT means the right most AXIS/ANGLE in the AXIS/ANGLES
argument list is the first rotation applied, and so on, going leftwards. This is
the notation most commonly used when performing quaternion transforms on paper,
and we replicate it here.

OUT is a destructively modified quaternion representing the final state of all
specified rotations. This quaternion rotsation does not suffer from gimbal lock
no matter what the rotations were that created it.

STYLE is wither :WORLD or :LOCAL.
 :WORLD means that each rotation should act as if it is around the axis
specified in world space.
 :LOCAL means that each rotation rotates around the specified axis after that
axis had been rotated by the previous rotation. The initial previous rotation
is identity.

AXIS/ANGLES is a p-list of AXIS and ANGLE values repeating after one another.

An AXIS can be :X, :Y, :Z, or a v3 vector representing the axis of rotation.
A supplied v3 vector does not have to be normalized.

ANGLE is a single-float in radians.

Together an AXIS/ANGLE combination represents a rotation of an angle around an
axis.

An example call:

  (orient<-! out :world :x (/ pi 4) (v3:make 1.0 1.0 1.0) (/ pi 2) :y pi)

means:
  Build a quaternion that represents the final state of:
    First, rotate pi around the world Y axis first,
    Second, rotate (/ pi 2) around the world #(1.0 1.0 1.0) axis,
    Third, rotate (/ pi 4) around the world X axis.
    Lastly, destructively store the final quaternion into OUT.
    The return value will be OUT.
"

  ;; NOTE: We use this raw method of the math to prevent any consing.
  (let ((vx 0f0) ;; rotation axis vector as a vec3
        (vy 0f0)
        (vz 0f0)
        (cw 1f0) ;; current axis/angle as a quaternion
        (cx 0f0)
        (cy 0f0)
        (cz 0f0))

    (q:id! out)

    ;; TODO: Fix to handle destructuring errors or other types of errors.
    (loop :for (axis angle) :on axis/angles :by #'cddr
          :do (case axis
                ;; all the default axes are considered normalized already.
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
                          vz az))
                 ;; Normalize the supplied axis vector.
                 (let ((len (sqrt (raw-dot vx vy vz vx vy vz))))
                   ;; TODO: In the event of an actual zero, this will be wildly
                   ;; wrong. Prolly should look at it more.
                   (unless (zerop len)
                     (psetf vx (/ vx len)
                            vy (/ vy len)
                            vz (/ vz len))))))

              ;; Make the individual quaternion rotation to represent the
              ;; axis/angle representation.
              ;; TODO: Why is this generally not in the quaternion API?
              ;; It should be.
              (let* ((angle/2 (/ angle 2.0))
                     (ca2 (float (cos angle/2) 1f0))
                     (sa2 (float (sin angle/2) 1f0)))
                (psetf cw ca2
                       cx (* vx sa2)
                       cy (* vy sa2)
                       cz (* vz sa2)))

              ;; Then accumulate the current rotation into the OUT variable.
              ;; The order of multiplication indicates wether or not we're
              ;; using local axis rotations, or world axis rotations.
              ;; But notice we're _associating_ the arguments left to right,
              ;; but we aren't changing their commutativity (wrt to style).
              (q:with-components ((o out))
                (ecase style
                  (:local
                    (raw-quaternion-multiply ow ox oy oz
                                             cw cx cy cz
                                             ow ox oy oz))
                  (:world
                   (raw-quaternion-multiply ow ox oy oz
                                            ow ox oy oz
                                            cw cx cy cz))))

              ;; Ensure it is ready for the next accumulation.
              (q:normalize! out out))

    out))

;; TODO: Request to put this into the quaternion package.
(defun orient<- (style &rest axis/angles)
  "Please see the documentation string for the function ORIENT<-! to understand
how to use this function."
  (apply #'orient<-! (q:id) style axis/angles))

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
      (au:mvlet*
          (;; Deal with deadzones and other bad data around the input vector.
           (vec (v3:make lx ly 0))
           (vec (if (> (v3:length vec) 1) (v3:normalize vec) vec))
           (vec (if (< (v3:length vec) translate-deadzone) (v3:zero) vec))
           ;; Right trigger modifies speed. pull to lerp from full speed
           ;; to half speed.
           (ty
            (nth-value 1 (fl.input:get-gamepad-analog (fl:input-data context)
                                                      '(:gamepad1 :triggers))))
           ;; Compute the actual translation vector related to our frame time!
           (vec
            (v3:scale vec
                      (float (* (au:lerp ty max-velocity (/ max-velocity 2f0))
                                (fl:frame-time context))
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
                          (q:orient :local :z angle)
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
                            parent-rotation
			    (scale explosion)
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
(defun make-projectile (context translation rotation physics-layer depth-layer
                        &key
                          (parent nil)
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
                   `((,prefab-name ,prefab-library))
                   :parent parent)))
         ;; TODO: I'm expecting the new-projectile to have components here
         ;; without having gone through the flow. BAD!
         (projectile-transform (fl:actor-component-by-type new-projectile
                                                           'fl.comp:transform))
         (projectile (fl:actor-component-by-type new-projectile 'projectile)))

    ;; Set the spatial configuration
    (setf (v3:z translation) (dl depth-layer))
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
   (scale :default (v3:make 1.0 1.0 1.0))
   (frames :default 15)))

;; NOTE: No physics layers for this since they don't even participate in the
;; collisions.
(defun make-explosion (context translation rotation scale
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

    (fl.comp:scale explosion-transform scale
                   :instant-p t :replace-p t)
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
   (depth-layer :default nil)
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
                   (emitter-transform emitter-transform)
                   (depth-layer depth-layer))
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
                                (q:orient :local :z angle)
                                (physics-layer self)
                                depth-layer
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
   (asteroid-holder :default nil)
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
                   (asteroid-holder asteroid-holder)
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
                                  q:+id+
                                  :enemy-bullet
                                  :asteroid
                                  :velocity  (ransign 50 400)
                                  ;; this direction is in world space.
                                  ;; it moves from the origin to the target.
                                  :direction (v3:normalize (v3:- target origin))
                                  :scale (v3:make uniform-scale
                                                  uniform-scale
                                                  uniform-scale)
                                  :name name
                                  :frames frames
                                  :destroy-ttl 4
                                  :parent asteroid-holder)))))

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

(defun reset-stable (player-stable)
  (with-accessors ((max-lives max-lives)
                   (lives-remaining lives-remaining))
      player-stable

    (setf lives-remaining max-lives)
    (regenerate-lives player-stable)))


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

;; The director manages the overall state the game is in, plus it
;; micromanages the playing state to ensure fun ensues. Each state is
;; non-blocking and manages a distinct state in which the game may be.
;;
;; state-machine:
;;
;; <start>
;;   |
;;   v
;; :waiting-to-play -> :quit
;;   |
;;   v
;; :level-spawn
;;   |
;;   v
;; :player-spawn -> :game-over
;;   |
;;   v
;; :playing -> :player-spawn | :end-of-level | :quit
;;   |
;;   v
;; :game-over -> :waiting-to-play | :quit
;;   |
;;   v
;; :initials-entry -> :waiting-to-play | :quit
;;
;; :end-of-level -> :level-spawn | :quit
;;
;; :quit
;;   |
;;   v
;; <terminate>
;;

(fl:define-component director ()
  ((current-game-state :default :waiting-to-play)
   (previous-game-state :default nil)
   ;; The db of levels over which the game progresses.
   (levels :default (fl.prefab::prefab-descriptor
                      ("level-0" lgj-04/2019)
                      ("level-1" lgj-04/2019)
                      ("level-2" lgj-04/2019)))
   ;; which level is considered the demo level.
   (demo-level :default (fl.prefab::prefab-descriptor
                          ("demo-level" lgj-04/2019)))
   ;; Which level are we playing?
   (current-level :default 0)
   ;; The actual root instance of the actor for the current level.
   (current-level-ref :default nil)
   ;; This is the parent of the levels when they are instantiated.
   (level-holder :default nil)
   ;; The stable from which we know we can get another player.
   (player-1-stable :default nil)
   ;; When we instantiate a player, this is the place it goes.
   (current-player-holder :default NIL)
   ;; And a reference to the actual player instance
   (current-player :default NIL)

   ;; Timer
   ;; When we respawn a player, we wait max-wait-time before they show up.
   (player-respawn-max-wait-time :default .5f0)
   (player1-respawn-timer :default 0)
   (player1-waiting-for-respawn :default nil)

   ;; Timer
   ;; When we enter game over, this is how long to show the gameover sign.
   (game-over-max-wait-time :default 2)
   (game-over-timer :default 0)))

;; each method returns the new state it should do for the next update.
(defgeneric process-director-state (director state previous-state))

;; From here, we dispatch to the state management GF.
(defmethod fl:on-component-update ((self director))
  (with-accessors ((current-game-state current-game-state)
                   (previous-game-state previous-game-state)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (level-holder level-holder)
                   (current-player-holder current-player-holder))
      self

    (let (new-game-state)
      (setf new-game-state
            (process-director-state self current-game-state previous-game-state)

            previous-game-state current-game-state

            current-game-state new-game-state))))

(defmethod process-director-state ((self director)
                                   (state (eql :waiting-to-play))
                                   previous-state)
  (with-accessors ((context fl:context)
                   (demo-level demo-level)
                   (current-level current-level)
                   (current-level-ref current-level-ref)
                   (level-holder level-holder)
                   (player-1-stable player-1-stable))
      self
    (let ((next-state :waiting-to-play))
      ;; When we've just transitioned to this state from not itself, it means
      ;; we need to do the work to show the demo level.
      (unless (eq state previous-state)
        ;; 1. Spawn the demo-level which includes the PtP sign and press play
        ;; to start.

        (when current-level-ref
          ;; Whatever was previously there, get rid of.
          (fl:destroy current-level-ref))

        (setf current-level-ref
              (first (fl:make-prefab-instance
                      (%fl::core context)
                      demo-level
                      :parent level-holder)))

        ;; 2. ensure the lives player1-stable is set to max if not already so.
        (when (/= (lives-remaining player-1-stable)
                  (max-lives player-1-stable))
          ;; TODO: If this happens first frame, something goes wrong. FIXME.
          ;; The TODO is why the WHEN guard is around this line.
          (reset-stable player-1-stable)))


      ;; 3. We always listen for the start button so we can play a game.
      (when (fl.input:input-enter-p (fl:input-data context) '(:gamepad1 :start))
        (setf current-level 0 ;; start at beginning of level progression
              next-state :level-spawn))

      next-state)))

(defmethod process-director-state ((self director)
                                   (state (eql :level-spawn))
                                   previous-state)
  (with-accessors ((context fl:context)
                   (current-game-state current-game-state)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (current-level-ref current-level-ref)
                   (level-holder level-holder)
                   (current-player-holder current-player-holder))
      self

    ;; Destroy old level, if any.
    (when current-level-ref
      (fl:destroy current-level-ref)
      (setf current-level-ref nil))

    ;; spawn current level requested
    (setf current-level-ref
          (first (fl:make-prefab-instance
                  (%fl::core context)
                  ;; TODO: Odd requirement for the LIST here given how I access
                  ;; this prefab description.
                  (list (nth current-level levels))
                  :parent level-holder)))

    :player-spawn))


(defmethod process-director-state ((self director)
                                   (state (eql :player-spawn))
                                   previous-state)
  (with-accessors ((context fl:context)
                   (current-player-holder current-player-holder)
                   (current-player current-player)
                   (player-1-stable player-1-stable)
                   (player-respawn-max-wait-time player-respawn-max-wait-time)
                   (player1-respawn-timer player1-respawn-timer)
                   (player1-waiting-for-respawn player1-waiting-for-respawn))
      self

    (let ((next-state :playing)
          (player-alive-p (tags-find-actors-with-tag context :player)))

      (when (not player-alive-p)
        (if (not (eq state previous-state))
            (setf player1-respawn-timer 0
                  next-state :player-spawn)
            (if (>= player1-respawn-timer player-respawn-max-wait-time)
                ;; We're waiting for respawn, and the time has come.
                ;; Create the player.
                (let ((player-life (consume-life player-1-stable)))
                  (if (null player-life)
                      ;; out of players!
                      (setf next-state :game-over)
                      (let ((new-player-instance
                              (first (fl:make-prefab-instance
                                      (%fl::core context)
                                      '(("player-ship" lgj-04/2019))
                                      :parent current-player-holder))))
                        ;; TODO: make player respawn in same place and
                        ;; orientation as where they died.

                        ;; Store the new player, and implicitly go to the
                        ;; unchanged next-state.
                        (setf current-player new-player-instance))))
                (progn
                  (incf player1-respawn-timer (fl:frame-time context))
                  (setf next-state :player-spawn)))))

      next-state)))

(defmethod process-director-state ((self director)
                                   (state (eql :playing))
                                   previous-state)

  (with-accessors ((context fl:context)
                   (current-game-state current-game-state)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (level-holder level-holder)
                   (player-1-stable player-1-stable)
                   (current-player-holder current-player-holder)
                   (current-player current-player))
      self

    ;; Notice if the player is dead and go to respawn.
    (let ((player-alive-p (tags-find-actors-with-tag context :player)))
      (if player-alive-p
          :playing
          :player-spawn))))


(defmethod process-director-state ((self director)
                                   (state (eql :game-over))
                                   previous-state)
  (with-accessors ((context fl:context)
                   (levels levels)
                   (demo-level demo-level)
                   (current-level current-level)
                   (level-holder level-holder)
                   (game-over-max-wait-time game-over-max-wait-time)
                   (game-over-timer game-over-timer)
                   (current-player-holder current-player-holder))
      self

    ;; TODO: Show a game over sign for a while.
    (unless (eq state previous-state)
      (setf game-over-timer 0)
      ;; TODO: Spawn game-over sign and set to destroy in max-time seconds
      (let ((game-over-sign
              (first (fl:make-prefab-instance
                      (%fl::core context)
                      '(("game-over-sign" lgj-04/2019))))))
        (fl:destroy-after-time game-over-sign :ttl game-over-max-wait-time)))

    (cond
      ((>= game-over-timer game-over-max-wait-time)
       :waiting-to-play)
      (t
       (incf game-over-timer (fl:frame-time context))
       :game-over))))

(defmethod process-director-state ((self director)
                                   (state (eql :initials-entry))
                                   previous-state)

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
  (explosion :name "explode04-01" :frames 15
             :scale (v3:make 2.0 2.0 2.0))
  (damage-points :dp 1)
  (hit-points :hp 1
              ;; When the player is born, they are automatically invulnerable
              ;; for 1 second.
              ;; TODO: NEED(!) to visualize this effect!
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
    (gun :physics-layer :player-bullet
         :depth-layer :player-bullet
         :name "bullet01" :frames 2))

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
  (fl.comp:transform :translate (v3:make 0 0 (dl :sign))
                     :scale (v3:make 512 512 512))
  ("sign"
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'warning-wave)))

(fl:define-prefab "warning-mothership-sign" (:library lgj-04/2019)
  (fl.comp:transform :translate (v3:make 0 0 (dl :sign))
                     :scale (v3:make 512 512 512))
  ("sign"
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'warning-mothership)))

(fl:define-prefab "title-sign" (:library lgj-04/2019)
  (fl.comp:transform :translate (v3:make 0 0 (dl :sign))
                     :scale (v3:make 512 512 512))
  ("sign"
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'title)))

(fl:define-prefab "game-over-sign" (:library lgj-04/2019)
  (fl.comp:transform :translate (v3:make 0 0 (dl :sign))
                     :scale (v3:make 512 512 512))
  ("sign"
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'game-over)))

(fl:define-prefab "starfield" (:library lgj-04/2019)
  ("bug-todo:implicit-transform:see-trello"
   (fl.comp:transform :scale (v3:make 960 960 960)
                      ;; NOTE: ortho projection, so we can put starfield way
                      ;; back.
                      :translate (v3:make 0 0 (dl :starfield)))
   (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
   (fl.comp:render :material 'starfield)))


(fl:define-prefab "demo-level" (:library lgj-04/2019)
  (asteroid-field :asteroid-holder (fl:ref "/demo-level/asteroids"))
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  ("asteroids")
  (("title" :copy ("/title-sign" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 0 0 (dl :sign))
                      ;; TODO: BUG: the scale in the original transform
                      ;; should have been preserved.
                      :scale (v3:make 512 512 512))))


(fl:define-prefab "level-0" (:library lgj-04/2019)
  (asteroid-field :asteroid-holder (fl:ref "/level-0/asteroids"))
  ("asteroids")
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 0 100 (dl :planet))
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01")))


(fl:define-prefab "level-1" (:library lgj-04/2019)
  (asteroid-field :asteroid-holder (fl:ref "/level-1/asteroids"))
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  ("asteroids")
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make -200 100 (dl :planet))
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01"))
  (("planet-1" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 200 100 (dl :planet))
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet02")))

(fl:define-prefab "level-2" (:library lgj-04/2019)
  (asteroid-field :asteroid-holder (fl:ref "/level-2/asteroids"))
  (("starfield" :link ("/starfield" :from lgj-04/2019)))
  ("asteroids")
  (("planet-0" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 0 100 (dl :planet))
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet01"))
  (("planet-1" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make -200 -100 (dl :planet))
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet02"))
  (("planet-2" :link ("/generic-planet" :from lgj-04/2019))
   (fl.comp:transform :translate (v3:make 200 -100 (dl :planet))
                      :scale (v3:make 0.9 0.9 0.9))
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet03")))

(fl:define-prefab "protect-the-planets" (:library lgj-04/2019)
  "The top most level prefab which has the component which drives the game
sequencing."
  (fl.comp:transform :scale (v3:one))
  (director :level-holder (fl:ref "/protect-the-planets/current-level")
            :player-1-stable (fl:ref "/protect-the-planets/player-1-stable"
                                     :component 'player-stable))

  (("camera" :copy ("/cameras/ortho" :from fl.example::examples))
   (fl.comp:transform :translate (v3:make 0 0 (dl :camera))))

  (("player-1-stable" :link ("/player-stable" :from lgj-04/2019))
   (fl.comp:transform
    :translate (v3:make -900 550 (dl :player-stable))))


  ("current-level"))




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefab descriptors for convenience
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fl:define-prefab-descriptor lgj-04/2019 ()
  ("protect-the-planets" fl.example::lgj-04/2019))
