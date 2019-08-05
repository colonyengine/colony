(in-package #:virality.examples)

;;; Textures

(v:define-texture sprites (:texture-2d)
  (:data #(:spritesheet)))

;;; Components

(v:define-component simple-movement ()
  ((%transform :reader transform)))

(defmethod v:on-component-initialize ((self simple-movement))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'comp.transform:transform))
    (comp.transform:translate %transform (v3:vec -400 0 0) :replace-p t :instant-p t)))

(defmethod v:on-component-update ((self simple-movement))
  (u:mvlet* ((context (v:context self))
             (transform (transform self))
             (lx ly (v:get-gamepad-analog context '(:gamepad1 :left-stick)))
             (rx ry (v:get-gamepad-analog context '(:gamepad1 :right-stick)))
             (instant-p (zerop (v:frame-count context))))
    (let ((vec (v3:vec lx ly 0)))
      (v3:scale! vec (if (> (v3:length vec) 1) (v3:normalize vec) vec) 150.0)
      (comp.transform:translate transform
                                (v3:+ (v3:vec -400 0 0) vec)
                                :replace-p t
                                :instant-p instant-p)
      (unless (= rx ry 0.0)
        (let* ((angle (atan (- rx) ry))
               (angle (if (minusp angle)
                          (+ pi (- pi (abs angle)))
                          angle)))
          (comp.transform:rotate transform
                                 (q:orient :local :z angle)
                                 :replace-p t
                                 :instant-p instant-p))))))

(v:define-component shot-mover ()
  ((%transform :reader transform)
   (%velocity :reader velocity
              :initarg :velocity
              :initform 0)))

(defmethod v:on-component-initialize ((self shot-mover))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'comp.transform:transform))))

(defmethod v:on-component-update ((self shot-mover))
  (comp.transform:translate
   (transform self)
   (let ((a (v3:normalize (m4:rotation-axis-to-vec3
                           (comp.transform:local (transform self)) :y)))
         (move-delta (float (* (velocity self)
                               (v:frame-time (v:context self)))
                            1f0)))
     (v3:scale a move-delta))))

(v:define-component shot-emitter ()
  ((%transform :reader transform)))

(defmethod v:on-component-initialize ((self shot-emitter))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'comp.transform:transform))))

(defmethod v:on-component-update ((self shot-emitter))
  (let ((context (v:context self)))
    (when (or (v:input-enter-p context '(:gamepad1 :a))
              (v:input-enter-p context '(:mouse :left)))
      (let* ((parent-model (comp.transform:model (transform self)))
             (parent-translation (m4:get-translation parent-model))
             (parent-rotation (q:from-mat4 parent-model))
             (new-actor (v:make-actor context :display-id "Ship bullet"))
             (transform (v:make-component context
                                          'comp.transform:transform
                                          :translate parent-translation
                                          :rotate parent-rotation))
             (shot-mover (v:make-component context 'shot-mover :velocity 1000))
             (sprite (v:make-component context
                                       'comp.sprite:sprite
                                       :spec :spritesheet-data
                                       :name "bullet01"
                                       :frames 2))
             (render (v:make-component context
                                       'comp.render:render
                                       :material `(contrib.mat:sprite
                                                   ,(a:make-gensym '#:sprite)
                                                   :uniforms
                                                   ((:sprite.sampler sprites)))
                                       :mode :sprite)))
        (v:attach-components
         new-actor transform shot-mover sprite render)
        (v:spawn-actor new-actor)
        (v:destroy-after-time new-actor :ttl 2)))))

;;; Prefabs

(v:define-prefab "sprite-1" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("ship"
   (comp.transform:transform :rotate (q:orient :local :z (/ pi -2)))
   (simple-movement)
   (shot-emitter)
   ("ship-body"
    (comp.sprite:sprite :spec :spritesheet-data
                        :name "ship29")
    (comp.render:render :material `(contrib.mat:sprite
                                    ,(a:make-gensym '#:sprite)
                                    :uniforms ((:sprite.sampler sprites)))
                        :mode :sprite)
    ("exhaust"
     (comp.transform:transform :translate (v3:vec 0 -140 0))
     (comp.sprite:sprite :spec :spritesheet-data
                         :name "exhaust03-01"
                         :frames 8)
     (comp.render:render :material `(contrib.mat:sprite
                                     ,(a:make-gensym '#:sprite)
                                     :uniforms ((:sprite.sampler sprites)))
                         :mode :sprite)
     (comp.actions:actions :default '((:type contrib.action:sprite-animate
                                       :duration 0.5
                                       :repeat-p t)))))))

(v:define-prefab "sprite-2" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("plane"
   (comp.transform:transform :scale 2)
   (comp.sprite:sprite :spec :spritesheet-data
                       :name "planet04")
   (comp.render:render :material `(contrib.mat:sprite
                                   ,(a:make-gensym '#:sprite)
                                   :uniforms ((:sprite.sampler sprites)))
                       :mode :sprite)
   (comp.actions:actions :default '((:type contrib.action:rotate
                                     :duration 4
                                     :shape origin.shaping:bounce-in
                                     :repeat-p t)))))

;;; Prefab descriptors

(v:define-prefab-descriptor sprite-1 ()
  ("sprite-1" examples))

(v:define-prefab-descriptor sprite-2 ()
  ("sprite-2" examples))
