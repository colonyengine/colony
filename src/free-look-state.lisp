(in-package #:virality.engine)

(defclass free-look-state ()
  ((%context :reader context
             :initarg :context)
   (%camera-transform :reader camera-transform
                      :initarg :camera-transform)
   (%initial-state :accessor initial-state
                   :initform nil)
   (%initial-orientation :accessor initial-orientation
                         :initform (m4:mat 1))
   (%key-state :reader key-state
               :initform (u:dict #'eq))
   (%key-speed :reader key-speed
               :initform 30f0)
   (%mouse-sensitivity :reader mouse-sensitivity
                       :initform 10f0)))

(defun make-free-look-state (context camera-transform)
  (make-instance 'free-look-state
                 :context context
                 :camera-transform camera-transform))

(defun set-initial-free-look-orientation (state model)
  (unless (initial-state state)
    (setf (initial-orientation state) model
          (initial-state state) t)))

(defun reset-free-look-state (state)
  (let* ((transform (camera-transform state))
         (model (initial-orientation state))
         (translation (m4:get-translation model))
         (rotation (q:from-mat4 model)))
    (c/xform:translate transform translation :replace-p t)
    (c/xform:rotate transform rotation :replace-p t)))

(defun update-free-look-key-state (state)
  (let ((context (context state))
        (key-state (key-state state)))
    (when (input-enter-p context '(:key :backspace))
      (reset-free-look-state state))
    (cond
      ((input-enabled-p context '(:key :w))
       (setf (u:href key-state :forward) t))
      ((input-exit-p context '(:key :w))
       (setf (u:href key-state :forward) nil)))
    (cond
      ((input-enabled-p context '(:key :s))
       (setf (u:href key-state :backward) t))
      ((input-exit-p context '(:key :s))
       (setf (u:href key-state :backward) nil)))
    (cond
      ((input-enabled-p context '(:key :a))
       (setf (u:href key-state :strafe-left) t))
      ((input-exit-p context '(:key :a))
       (setf (u:href key-state :strafe-left) nil)))
    (cond
      ((input-enabled-p context '(:key :d))
       (setf (u:href key-state :strafe-right) t))
      ((input-exit-p context '(:key :d))
       (setf (u:href key-state :strafe-right) nil)))
    (cond
      ((input-enabled-p context '(:key :left))
       (setf (u:href key-state :turn-left) t))
      ((input-exit-p context '(:key :left))
       (setf (u:href key-state :turn-left) nil)))
    (cond
      ((input-enabled-p context '(:key :right))
       (setf (u:href key-state :turn-right) t))
      ((input-exit-p context '(:key :right))
       (setf (u:href key-state :turn-right) nil)))
    (cond
      ((input-enabled-p context '(:key :pageup))
       (setf (u:href key-state :strafe-up) t))
      ((input-exit-p context '(:key :pageup))
       (setf (u:href key-state :strafe-up) nil)))
    (cond
      ((input-enabled-p context '(:key :pagedown))
       (setf (u:href key-state :strafe-down) t))
      ((input-exit-p context '(:key :pagedown))
       (setf (u:href key-state :strafe-down) nil)))))

(defun free-look/key-move (transform key-state speed)
  (flet ((velocity (direction plus minus)
           (v3:scale (c/xform:transform-direction transform direction)
                     (cond
                       ((u:href key-state plus) speed)
                       ((u:href key-state minus) (- speed))
                       (t 0f0)))))
    (let* ((x (velocity v3:+right+ :strafe-right :strafe-left))
           (y (velocity v3:+up+ :strafe-up :strafe-down))
           (z (velocity v3:+back+ :forward :backward))
           ;; TODO: normalize?
           (vec (v3:+ (v3:+ x y) z))
           (angle (velocity v3:+up+ :turn-left :turn-right)))
      (c/xform:translate transform vec)
      (c/xform:rotate/velocity transform angle 1f0))))

(defun free-look/mouse-move (state transform speed)
  (flet ((velocity (direction x)
           (v3:scale (c/xform:transform-direction transform direction)
                     (* x speed))))
    (u:mvlet* ((context (context state))
               (mx my dx dy (in:get-mouse-position context))
               (x (velocity v3:+right+ dx))
               (y (velocity v3:+up+ dy))
               (z (velocity v3:+forward+ dy))
               ;; TODO: normalize?
               (vec (v3:+ x y))
               (angle (v3:+ (velocity v3:+down+ dx)
                            (velocity v3:+right+ dy))))
      (when (or (input-enter-p context '(:key :lshift))
                (input-enter-p context '(:key :lctrl))
                (input-enter-p context '(:key :lalt)))
        (in::enable-relative-motion context))
      (when (input-enabled-p context '(:key :lshift))
        (c/xform:translate transform vec))
      (when (input-enabled-p context '(:key :lalt))
        (c/xform:translate transform z))
      (when (input-enabled-p context '(:key :lctrl))
        (c/xform:rotate/velocity transform angle 1f0))
      (when (or (input-exit-p context '(:key :lshift))
                (input-exit-p context '(:key :lctrl))
                (input-exit-p context '(:key :lalt)))
        (in::disable-relative-motion context)))))

(defun update-free-look-state (state)
  (let* ((transform (camera-transform state))
         (dt (frame-time (context state)))
         (key-speed (* (key-speed state) dt))
         (mouse-speed (* (mouse-sensitivity state) dt)))
    (update-free-look-key-state state)
    (free-look/key-move transform (key-state state) key-speed)
    (free-look/mouse-move state transform mouse-speed)))
