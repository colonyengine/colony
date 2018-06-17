(in-package :%fl.core)

(au:define-constant +gamepad-axis-names+
    #(:left-horizontal :left-vertical :right-horizontal :right-vertical :trigger-left
      :trigger-right)
  :test #'equalp)

(au:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick :right-stick :left-shoulder :right-shoulder :up
      :down :left :right)
  :test #'equalp)

(defstruct gamepad id instance description handle)

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

(defun normalize-gamepad-axis-value (axis value)
  (case axis
    ((:trigger-left :trigger-right)
     (au:map-domain 0 32767 0 1 value))
    (t
     (au:map-domain -32767 32767 -1 1 (au:clamp value -32767 32767)))))

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
                                  :description (sdl2:game-controller-name handle)
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

(defun on-gamepad-axis-move (core-state gamepad-instance axis value)
  (let ((gamepad (get-gamepad-by-instance core-state gamepad-instance)))
    (setf (au:href (states (input-data core-state)) (list (gamepad-id gamepad) axis))
          (normalize-gamepad-axis-value axis value))))

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

(defun gamepad-attached-p (context gamepad-id)
  (not (member gamepad-id (detached-gamepads (input-data (core-state context))))))

(defun get-gamepad-description (context gamepad-id)
  (let ((gamepad (au:href (gamepad-ids (input-data (core-state context))) gamepad-id)))
    (gamepad-description gamepad)))

(defun get-gamepad-axis (context gamepad-id stick/axis)
  (let ((states (states (input-data (core-state context)))))
    (au:href states (list gamepad-id stick/axis))))
