(in-package #:virality.input)

(a:define-constant +gamepad-axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y)
      (:triggers :x) (:triggers :y))
  :test #'equalp)

(a:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick-button :right-stick-button
      :left-shoulder :right-shoulder :up :down :left :right)
  :test #'equalp)

(defstruct gamepad id instance name handle)

(defstruct gamepad-analog-state x y deadzone)

;;; Utility functions

(defun get-gamepad-by-instance (data gamepad-instance)
  (u:href (gamepad-instances data) gamepad-instance))

(defun generate-gamepad-id (data)
  (with-slots (%gamepad-instances %detached-gamepads) data
    (or (pop %detached-gamepads)
        (a:format-symbol :keyword "GAMEPAD~d"
                         (1+ (hash-table-count %gamepad-instances))))))

(defun prepare-gamepads (gamepad-database)
  (load-gamepad-database gamepad-database)
  (enable-background-gamepad-events))

(defun shutdown-gamepads (context)
  (let* ((data (v::input-data (v::core context)))
         (instance-table (gamepad-instances data)))
    (u:do-hash-values (v instance-table)
      (sdl2:game-controller-close (gamepad-handle v)))
    (clrhash instance-table)))

(defun normalize-gamepad-analog-value (sub-device axis value)
  (if (eq sub-device :triggers)
      (u:map-domain 0 32767 0 1 value)
      (let ((clamped (a:clamp value -32767 32767)))
        (ecase axis
          (:x (u:map-domain -32767 32767 -1 1 clamped))
          (:y (u:map-domain -32767 32767 1 -1 clamped))))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :axial)) analog-state)
  (with-slots (deadzone x y) analog-state
    (v2:with-components ((v (v2:vec x y)))
      (values vx vy))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial)) analog-state)
  (with-slots (deadzone x y) analog-state
    (v2:with-components ((v (v2:vec x y)))
      (if (< (v2:length v) deadzone)
          (values 0.0 0.0)
          (values vx vy)))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial-scaled))
                                analog-state)
  (with-slots (deadzone x y) analog-state
    (v2:with-components ((v (v2:vec x y)))
      (let ((length (v2:length v)))
        (if (< length deadzone)
            (values 0.0 0.0)
            (progn
              (v2:scale! v
                         (v2:normalize v)
                         (/ (- length deadzone) (- 1 deadzone)))
              (values vx vy)))))))

(defun load-gamepad-database (file-path)
  (sdl2:game-controller-add-mappings-from-file (namestring file-path)))

(defun enable-background-gamepad-events ()
  (sdl2-ffi.functions:sdl-set-hint
   sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1"))

;;; Events

(defun on-gamepad-attach (context gamepad-index)
  (let ((data (v::input-data (v::core context))))
    (when (sdl2:game-controller-p gamepad-index)
      (let* ((handle (sdl2:game-controller-open gamepad-index))
             (instance (sdl2:game-controller-instance-id handle))
             (id (generate-gamepad-id data))
             (gamepad (make-gamepad :id id
                                    :instance instance
                                    :name (sdl2:game-controller-name handle)
                                    :handle handle)))
        (setf (u:href (gamepad-instances data) instance) gamepad
              (u:href (gamepad-ids data) id) gamepad)
        (input-transition-in data (list id :attach))))))

(defun on-gamepad-detach (context gamepad-instance)
  (let* ((data (v::input-data (v::core context)))
         (instance-table (gamepad-instances data))
         (id-table (gamepad-ids data))
         (gamepad (u:href instance-table gamepad-instance))
         (id (gamepad-id gamepad)))
    (sdl2:game-controller-close (gamepad-handle gamepad))
    (a:appendf (detached-gamepads data) (list id))
    (remhash id id-table)
    (remhash gamepad-instance instance-table)
    (input-transition-out data (list id :attach))))

(defun on-gamepad-analog-move (context gamepad-instance axis value)
  (destructuring-bind (sub-device axis) axis
    (let* ((data (v::input-data (v::core context)))
           (gamepad (get-gamepad-by-instance data gamepad-instance))
           (key (list (gamepad-id gamepad) sub-device))
           (value (normalize-gamepad-analog-value sub-device axis value)))
      (symbol-macrolet ((state (u:href (states data) key)))
        (if (not state)
            (setf state (make-gamepad-analog-state :x 0.0 :y 0.0 :deadzone 0.0))
            (ecase axis
              (:x (setf (gamepad-analog-state-x state) value))
              (:y (setf (gamepad-analog-state-y state) value))))))))

(defun on-gamepad-button-up (context gamepad-instance button)
  (let* ((data (v::input-data (v::core context)))
         (gamepad (get-gamepad-by-instance data gamepad-instance))
         (id (gamepad-id gamepad)))
    (input-transition-out data (list id button))
    (input-transition-out data (list id :any))
    (input-transition-out data '(:button :any))))

(defun on-gamepad-button-down (context gamepad-instance button)
  (let* ((data (v::input-data (v::core context)))
         (gamepad (get-gamepad-by-instance data gamepad-instance))
         (id (gamepad-id gamepad)))
    (input-transition-in data (list id button))
    (input-transition-in data (list id :any))
    (input-transition-in data '(:button :any))))

;;; User protocol

(defun get-gamepad-name (context gamepad-id)
  (let* ((data (v::input-data (v::core context)))
         (gamepad (u:href (gamepad-ids data) gamepad-id)))
    (gamepad-name gamepad)))

(defun get-gamepad-analog (context input)
  (let ((data (v::input-data (v::core context))))
    (u:if-found (state (u:href (states data) input))
                (%get-gamepad-analog :radial-scaled state)
                (values 0.0 0.0))))
