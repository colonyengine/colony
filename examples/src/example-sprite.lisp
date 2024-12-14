(in-package #:colony-examples)

;;; Textures

(c:define-texture-map sprites (:2d :unique)
  (texmap:mipmap (textures sprites)))

(c:define-texture sprites (:texture-2d)
  ;; TODO: TMAP Convert :data to be texture-map name.
  (:data #((textures sprites))))

;;; Components

(c:define-component simple-movement ()
  ((%transform :reader transform)))

(defmethod c:on-component-initialize ((self simple-movement))
  (with-slots (%transform) self
    (setf %transform (c:component-by-type (c:actor self) 'comp:transform))
    (c:translate self (v3:vec -400f0 0f0 0f0) :replace t :instant t)))

(defmethod c:on-component-update ((self simple-movement))
  (u:mvlet* ((context (c:context self))
             (lx ly (c:get-gamepad-analog
                     context :radial-scaled '(:gamepad1 :left-stick)))
             (rx ry (c:get-gamepad-analog
                     context :radial-scaled '(:gamepad1 :right-stick)))
             (instant-p (zerop (c:frame-count context))))
    (let ((vec (v3:vec lx ly 0f0)))
      (v3:scale! vec (if (> (v3:length vec) 1) (v3:normalize vec) vec) 150f0)
      (c:translate self
                   (v3:+ (v3:vec -400f0 0f0 0f0) vec)
                   :replace t
                   :instant instant-p
                   :space :inertial)
      (unless (= rx ry 0f0)
        (let* ((angle (atan (- rx) ry))
               (angle (if (minusp angle)
                          (+ o:pi (- o:pi (abs angle)))
                          angle)))
          (c:rotate self
                    (q:orient :local :z angle)
                    :replace t
                    :instant instant-p))))))

(c:define-component shot-mover ()
  ((%transform :reader transform)
   (%velocity :reader velocity
              :initarg :velocity
              :initform 0f0)))

(defmethod c:on-component-initialize ((self shot-mover))
  (with-slots (%transform) self
    (setf %transform (c:component-by-type (c:actor self) 'comp:transform))))

(defmethod c:on-component-update ((self shot-mover))
  (c:translate self
               (v3:scale v3:+up+
                         (* (velocity self) (c:frame-time (c:context self))))))

(c:define-component shot-emitter ()
  ((%transform :reader transform)))

(defmethod c:on-component-initialize ((self shot-emitter))
  (with-slots (%transform) self
    (setf %transform (c:component-by-type (c:actor self) 'comp:transform))))

(defmethod c:on-component-update ((self shot-emitter))
  (let ((context (c:context self)))
    (when (or (c:on-button-enter context :gamepad1 :a)
              (c:on-button-enter context :mouse :left))
      (let* ((parent-model (c:get-model-matrix self))
             (parent-translation (m4:get-translation parent-model))
             (parent-rotation (q:from-mat4 parent-model))
             (new-actor (c:make-actor context :display-id "Ship bullet"))
             (transform (c:make-component context
                                          'comp:transform
                                          :translate parent-translation
                                          :rotate parent-rotation))
             (shot-mover (c:make-component context 'shot-mover
                                           :velocity 1000f0))
             (sprite (c:make-component context
                                       'comp:sprite
                                       :spec '(metadata sprites)
                                       :name "bullet01"
                                       :frames 2))
             (render (c:make-component context
                                       'comp:render
                                       :material `(x:sprite
                                                   sprite
                                                   :uniforms
                                                   ((:sprite.sampler sprites)))
                                       :slave sprite)))
        (c:attach-components
         new-actor transform shot-mover sprite render)
        (c:spawn-actor new-actor)
        (c:destroy new-actor :ttl 2f0)))))

;;; Prefabs

(c:define-prefab "sprite" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("ship"
   (comp:transform :rotate (q:orient :local :z (- o:pi/2)))
   (simple-movement)
   (shot-emitter)
   ("ship-body"
    (comp:sprite :spec '(metadata sprites)
                 :name "ship29")
    (comp:render :material `(x:sprite
                             sprite
                             :uniforms ((:sprite.sampler sprites)))
                 :slave (c:ref :self :component'comp:sprite))
    ("exhaust"
     (comp:transform :translate (v3:vec 0f0 -140f0 0f0))
     (comp:sprite :spec '(metadata sprites)
                  :name "exhaust03-01"
                  :frames 8
                  :duration 0.75)
     (comp:render :material `(x:sprite
                              sprite
                              :uniforms ((:sprite.sampler sprites)))
                  :slave (c:ref :self :component'comp:sprite))))))
