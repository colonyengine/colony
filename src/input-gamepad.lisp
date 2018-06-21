(in-package :%fl)

(au:define-constant +gamepad-axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y) (:triggers :x)
      (:triggers :y))
  :test #'equalp)

(au:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick :right-stick :left-shoulder :right-shoulder :up
      :down :left :right)
  :test #'equalp)

(defstruct gamepad id instance name handle)

(defstruct gamepad-analog-state x y deadzone)

;;; Utility functions

(defun get-gamepad-by-instance (core-state gamepad-instance)
  (au:href (gamepad-instances (input-data core-state)) gamepad-instance))

(defun generate-gamepad-id (core-state)
  (with-slots (%gamepad-instances %detached-gamepads) (input-data core-state)
    (or (pop %detached-gamepads)
        (au:format-symbol :keyword "GAMEPAD~d" (1+ (hash-table-count %gamepad-instances))))))

(defun shutdown-gamepads (core-state)
  (let ((instance-table (gamepad-instances (input-data core-state))))
    (au:do-hash-values (v instance-table)
      (sdl2:game-controller-close (gamepad-handle v)))
    (clrhash instance-table)))

(defun normalize-gamepad-analog-value (sub-device axis value)
  (if (eq sub-device :triggers)
      (au:map-domain 0 32767 0 1 value)
      (let ((clamped (au:clamp value -32767 32767)))
        (case axis
          (:x (au:map-domain -32767 32767 -1 1 clamped))
          (:y (au:map-domain -32767 32767 1 -1 clamped))))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :axial)) analog-state)
  (with-slots (deadzone x y) analog-state
    (v2:with-components ((v (v2:make x y)))
      (v2:stabilize! v v :tolerance deadzone)
      (values vx vy))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial)) analog-state)
  (with-slots (deadzone x y) analog-state
    (v2:with-components ((v (v2:make x y)))
      (if (< (v2:magnitude v) deadzone)
          (values 0.0 0.0)
          (values vx vy)))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial-scaled)) analog-state)
  (with-slots (deadzone x y) analog-state
    (v2:with-components ((v (v2:make x y)))
      (let ((magnitude (v2:magnitude v)))
        (if (< magnitude deadzone)
            (values 0.0 0.0)
            (progn
              (v2:scale! v (v2:normalize v) (/ (- magnitude deadzone) (- 1 deadzone)))
              (values vx vy)))))))

(defun load-gamepad-database ()
  (sdl2:game-controller-add-mappings-from-file
   (namestring
    (uiop/pathname:merge-pathnames*
     (get-extension-path)
     (make-pathname :defaults *default-pathname-defaults* :name "gamepad-db" :type "txt")))))

(defun enable-background-gamepad-events ()
  (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1"))

(defun prepare-gamepads ()
  (load-gamepad-database)
  (enable-background-gamepad-events))

;;; Events

(defun on-gamepad-attach (core-state gamepad-index)
  (when (sdl2:game-controller-p gamepad-index)
    (let* ((handle (sdl2:game-controller-open gamepad-index))
           (instance (sdl2:game-controller-instance-id handle))
           (id (generate-gamepad-id core-state))
           (gamepad (make-gamepad :id id
                                  :instance instance
                                  :name (sdl2:game-controller-name handle)
                                  :handle handle)))
      (setf (au:href (gamepad-instances (input-data core-state)) instance) gamepad
            (au:href (gamepad-ids (input-data core-state)) id) gamepad)
      (input-transition-in core-state (list id :attach)))))

(defun on-gamepad-detach (core-state gamepad-instance)
  (let* ((instance-table (gamepad-instances (input-data core-state)))
         (id-table (gamepad-ids (input-data core-state)))
         (gamepad (au:href instance-table gamepad-instance))
         (id (gamepad-id gamepad)))
    (sdl2:game-controller-close (gamepad-handle gamepad))
    (au:appendf (detached-gamepads (input-data core-state)) (list id))
    (remhash id id-table)
    (remhash gamepad-instance instance-table)
    (input-transition-out core-state (list id :attach))))

(defun on-gamepad-analog-move (core-state gamepad-instance axis value)
  (destructuring-bind (sub-device axis) axis
    (let* ((gamepad (get-gamepad-by-instance core-state gamepad-instance))
           (key (list (gamepad-id gamepad) sub-device))
           (value (normalize-gamepad-analog-value sub-device axis value)))
      (symbol-macrolet ((state (au:href (states (input-data core-state)) key)))
        (if (not state)
            (setf state (make-gamepad-analog-state :x 0.0 :y 0.0 :deadzone 0.0))
            (case axis
              (:x (setf (gamepad-analog-state-x state) value))
              (:y (setf (gamepad-analog-state-y state) value))))))))

(defun on-gamepad-button-up (core-state gamepad-instance button)
  (let* ((gamepad (get-gamepad-by-instance core-state gamepad-instance))
         (id (gamepad-id gamepad)))
    (input-transition-out core-state (list id button))
    (input-transition-out core-state (list id :any))
    (input-transition-out core-state '(:button :any))))

(defun on-gamepad-button-down (core-state gamepad-instance button)
  (let* ((gamepad (get-gamepad-by-instance core-state gamepad-instance))
         (id (gamepad-id gamepad)))
    (input-transition-in core-state (list id button))
    (input-transition-in core-state (list id :any))
    (input-transition-in core-state '(:button :any))))

;;; User protocol

(defun get-gamepad-name (context gamepad-id)
  (let ((gamepad (au:href (gamepad-ids (input-data (core-state context))) gamepad-id)))
    (gamepad-name gamepad)))

(defun get-gamepad-analog (context input)
  (au:if-found (state (au:href (states (input-data (core-state context))) input))
               (%get-gamepad-analog :radial-scaled state)
               (values 0.0 0.0)))
