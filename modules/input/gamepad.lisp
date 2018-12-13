(in-package :first-light.input)

(fl.util:define-constant +gamepad-axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y) (:triggers :x)
      (:triggers :y))
  :test #'equalp)

(fl.util:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick-button :right-stick-button :left-shoulder
      :right-shoulder :up :down :left :right)
  :test #'equalp)

(defstruct gamepad id instance name handle)

(defstruct gamepad-analog-state x y deadzone)

;;; Utility functions

(defun get-gamepad-by-instance (input-data gamepad-instance)
  (fl.util:href (gamepad-instances input-data) gamepad-instance))

(defun generate-gamepad-id (input-data)
  (with-slots (%gamepad-instances %detached-gamepads) input-data
    (or (pop %detached-gamepads)
        (fl.util:format-symbol :keyword "GAMEPAD~d" (1+ (hash-table-count %gamepad-instances))))))

(defun prepare-gamepads (gamepad-database)
  (load-gamepad-database gamepad-database)
  (enable-background-gamepad-events))

(defun shutdown-gamepads (input-data)
  (let ((instance-table (gamepad-instances input-data)))
    (fl.util:do-hash-values (v instance-table)
      (sdl2:game-controller-close (gamepad-handle v)))
    (clrhash instance-table)))

(defun normalize-gamepad-analog-value (sub-device axis value)
  (if (eq sub-device :triggers)
      (fl.util:map-domain 0 32767 0 1 value)
      (let ((clamped (fl.util:clamp value -32767 32767)))
        (case axis
          (:x (fl.util:map-domain -32767 32767 -1 1 clamped))
          (:y (fl.util:map-domain -32767 32767 1 -1 clamped))))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :axial)) analog-state)
  (with-slots (deadzone x y) analog-state
    (flm:with-vec2 ((v (flm:vec2 x y)))
      (flm:stabilize v deadzone v)
      (values v.x v.y))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial)) analog-state)
  (with-slots (deadzone x y) analog-state
    (flm:with-vec2 ((v (flm:vec2 x y)))
      (if (< (flm:length v) deadzone)
          (values 0.0 0.0)
          (values v.x v.y)))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial-scaled)) analog-state)
  (with-slots (deadzone x y) analog-state
    (flm:with-vec2 ((v (flm:vec2 x y)))
      (let ((length (flm:length v)))
        (if (< length deadzone)
            (values 0.0 0.0)
            (progn
              (flm:* (flm:normalize v) (/ (- length deadzone) (- 1 deadzone)) v)
              (values v.x v.y)))))))

(defun load-gamepad-database (file-path)
  (sdl2:game-controller-add-mappings-from-file (namestring file-path)))

(defun enable-background-gamepad-events ()
  (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1"))

;;; Events

(defun on-gamepad-attach (input-data gamepad-index)
  (when (sdl2:game-controller-p gamepad-index)
    (let* ((handle (sdl2:game-controller-open gamepad-index))
           (instance (sdl2:game-controller-instance-id handle))
           (id (generate-gamepad-id input-data))
           (gamepad (make-gamepad :id id
                                  :instance instance
                                  :name (sdl2:game-controller-name handle)
                                  :handle handle)))
      (setf (fl.util:href (gamepad-instances input-data) instance) gamepad
            (fl.util:href (gamepad-ids input-data) id) gamepad)
      (input-transition-in input-data (list id :attach)))))

(defun on-gamepad-detach (input-data gamepad-instance)
  (let* ((instance-table (gamepad-instances input-data))
         (id-table (gamepad-ids input-data))
         (gamepad (fl.util:href instance-table gamepad-instance))
         (id (gamepad-id gamepad)))
    (sdl2:game-controller-close (gamepad-handle gamepad))
    (fl.util:appendf (detached-gamepads input-data) (list id))
    (remhash id id-table)
    (remhash gamepad-instance instance-table)
    (input-transition-out input-data (list id :attach))))

(defun on-gamepad-analog-move (input-data gamepad-instance axis value)
  (destructuring-bind (sub-device axis) axis
    (let* ((gamepad (get-gamepad-by-instance input-data gamepad-instance))
           (key (list (gamepad-id gamepad) sub-device))
           (value (normalize-gamepad-analog-value sub-device axis value)))
      (symbol-macrolet ((state (fl.util:href (states input-data) key)))
        (if (not state)
            (setf state (make-gamepad-analog-state :x 0.0 :y 0.0 :deadzone 0.0))
            (case axis
              (:x (setf (gamepad-analog-state-x state) value))
              (:y (setf (gamepad-analog-state-y state) value))))))))

(defun on-gamepad-button-up (input-data gamepad-instance button)
  (let* ((gamepad (get-gamepad-by-instance input-data gamepad-instance))
         (id (gamepad-id gamepad)))
    (input-transition-out input-data (list id button))
    (input-transition-out input-data (list id :any))
    (input-transition-out input-data '(:button :any))))

(defun on-gamepad-button-down (input-data gamepad-instance button)
  (let* ((gamepad (get-gamepad-by-instance input-data gamepad-instance))
         (id (gamepad-id gamepad)))
    (input-transition-in input-data (list id button))
    (input-transition-in input-data (list id :any))
    (input-transition-in input-data '(:button :any))))

;;; User protocol

(defun get-gamepad-name (input-data gamepad-id)
  (let ((gamepad (fl.util:href (gamepad-ids input-data) gamepad-id)))
    (gamepad-name gamepad)))

(defun get-gamepad-analog (input-data input)
  (fl.util:if-found (state (fl.util:href (states input-data) input))
                    (%get-gamepad-analog :radial-scaled state)
                    (values 0.0 0.0)))
