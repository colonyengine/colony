(in-package #:first-light.example)

;;; Textures

(fl:define-texture sprites (:texture-2d)
  (:data #(:spritesheet)))

;;; Components

(fl:define-component simple-movement ()
  ((%transform :reader transform)))

(defmethod fl:on-component-initialize ((self simple-movement))
  (with-slots (%transform) self
    (setf %transform (fl:actor-component-by-type (fl:actor self) 'transform))
    (fl.comp:translate %transform
                       (v3:vec -400 0 0)
                       :replace-p t
                       :instant-p t)))

(defmethod fl:on-component-update ((self simple-movement))
  (u:mvlet* ((context (fl:context self))
             (transform (transform self))
             (lx ly (fl:get-gamepad-analog (fl:input-data context)
                                           '(:gamepad1 :left-stick)))
             (rx ry (fl:get-gamepad-analog (fl:input-data context)
                                           '(:gamepad1 :right-stick)))
             (instant-p (zerop (fl:frame-count context))))
    (let ((vec (v3:vec lx ly 0)))
      (v3:scale! vec (if (> (v3:length vec) 1) (v3:normalize vec) vec) 150.0)
      (fl.comp:translate transform
                         (v3:+ (v3:vec -400 0 0) vec)
                         :replace-p t
                         :instant-p instant-p)
      (unless (= rx ry 0.0)
        (let* ((angle (atan (- rx) ry))
               (angle (if (minusp angle)
                          (+ pi (- pi (abs angle)))
                          angle)))
          (fl.comp:rotate transform
                          (q:orient :local :z angle)
                          :replace-p t
                          :instant-p instant-p))))))

(fl:define-component shot-mover ()
  ((%transform :reader transform)
   (%velocity :reader velocity
              :initarg :velocity
              :initform 0)))

(defmethod fl:on-component-initialize ((self shot-mover))
  (with-slots (%transform) self
    (setf %transform (fl:actor-component-by-type (fl:actor self)
                                                 'fl.comp:transform))))

(defmethod fl:on-component-update ((self shot-mover))
  (fl.comp:translate
   (transform self)
   (let ((a (v3:normalize (m4:rotation-axis-to-vec3
                           (fl.comp:local (transform self)) :y)))
         (move-delta (float (* (velocity self)
                               (fl:frame-time (fl:context self)))
                            1f0)))
     (v3:scale a move-delta))))

(fl:define-component shot-emitter ()
  ((%transform :reader transform)))

(defmethod fl:on-component-initialize ((self shot-emitter))
  (with-slots (%transform) self
    (setf %transform (fl:actor-component-by-type
                      (fl:actor self) 'fl.comp:transform))))

(defmethod fl:on-component-update ((self shot-emitter))
  (let ((context (fl:context self)))
    (when (or (fl:input-enter-p (fl:input-data context) '(:gamepad1 :a))
              (fl:input-enter-p (fl:input-data context) '(:mouse :left)))
      (let* ((parent-model (fl.comp:model (transform self)))
             (parent-translation (m4:get-translation parent-model))
             (parent-rotation (q:from-mat4 parent-model))
             (new-actor (fl:make-actor context :display-id "Ship bullet"))
             (transform (fl:make-component context
                                           'fl.comp:transform
                                           :translate parent-translation
                                           :rotate parent-rotation))
             (shot-mover (fl:make-component context 'shot-mover :velocity 1000))
             (sprite (fl:make-component context
                                        'sprite
                                        :spec :spritesheet-data
                                        :name "bullet01"
                                        :frames 2))
             (render (fl:make-component context
                                        'render
                                        :material `(contrib.mat:sprite
                                                    ,(a:make-gensym '#:sprite)
                                                    :uniforms
                                                    ((:sprite.sampler sprites)))
                                        :mode :sprite)))
        (fl:attach-multiple-components
         new-actor transform shot-mover sprite render)
        (fl:spawn-actor new-actor)
        (fl:destroy-after-time new-actor :ttl 2)))))

;;; Prefabs

(fl:define-prefab "sprite-1" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("ship"
   (fl.comp:transform :rotate (q:orient :local :z (/ pi -2)))
   (simple-movement)
   (shot-emitter)
   ("ship-body"
    (fl.comp:sprite :spec :spritesheet-data
                    :name "ship29")
    (fl.comp:render :material `(contrib.mat:sprite
                                ,(a:make-gensym '#:sprite)
                                :uniforms ((:sprite.sampler sprites)))
                    :mode :sprite)
    ("exhaust"
     (fl.comp:transform :translate (v3:vec 0 -140 0))
     (fl.comp:sprite :spec :spritesheet-data
                     :name "exhaust03-01"
                     :frames 8)
     (fl.comp:render :material `(contrib.mat:sprite
                                 ,(a:make-gensym '#:sprite)
                                 :uniforms ((:sprite.sampler sprites)))
                     :mode :sprite)
     (fl.comp:actions :default-actions '((:type contrib.action:sprite-animate
                                          :duration 0.5
                                          :repeat-p t)))))))

(fl:define-prefab "sprite-2" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("plane"
   (fl.comp:transform :scale 2)
   (fl.comp:sprite :spec :spritesheet-data
                   :name "planet04")
   (fl.comp:render :material `(contrib.mat:sprite
                               ,(a:make-gensym '#:sprite)
                               :uniforms ((:sprite.sampler sprites)))
                   :mode :sprite)
   (fl.comp:actions :default-actions '((:type contrib.action:rotate
                                        :duration 4
                                        :shape origin.shaping:bounce-in
                                        :repeat-p t)))))

;;; Prefab descriptors

(fl:define-prefab-descriptor sprite-1 ()
  ("sprite-1" examples))

(fl:define-prefab-descriptor sprite-2 ()
  ("sprite-2" examples))
