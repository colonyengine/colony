(in-package :fl.core)

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

(defun load-gamepad-database ()
  (sdl2:game-controller-add-mappings-from-file
   (namestring
    (uiop/pathname:merge-pathnames*
     (get-extension-path)
     (make-pathname :defaults *default-pathname-defaults* :name "gamepad-db" :type "txt")))))

(defun normalize-gamepad-axis-value (axis value)
  (case axis
    ((:trigger-left :trigger-right)
     (au:map-domain 0 32767 0 1 value))
    (t
     (au:map-domain -32767 32767 -1 1 (au:clamp value -32767 32767)))))

;;; Events

(defun on-gamepad-attach (core-state gamepad-index)
  (when (sdl2:game-controller-p gamepad-index)
    (let* ((handle (sdl2:game-controller-open gamepad-index))
           (instance (sdl2:game-controller-instance-id handle))
           (instance-table (gamepad-instances (input-data core-state)))
           (id (generate-gamepad-id core-state))
           (gamepad (make-gamepad :id id
                                  :instance instance
                                  :description (sdl2:game-controller-name handle)
                                  :handle handle)))
      (setf (au:href instance-table instance) gamepad
            (au:href (gamepad-ids (input-data core-state)) id) gamepad)
      (input-transition-in core-state (list id :attach)))))

(defun on-gamepad-detach (core-state gamepad-instance)
  (let* ((instance-table (gamepad-instances (input-data core-state)))
         (ids (gamepad-ids (input-data core-state)))
         (gamepad (au:href instance-table gamepad-instance)))
    (sdl2:game-controller-close (gamepad-handle gamepad))
    (au:appendf (detached-gamepads (input-data core-state)) (list (gamepad-id gamepad)))
    (remhash (gamepad-id gamepad) ids)
    (remhash gamepad-instance instance-table)
    (input-transition-out core-state (list (gamepad-id gamepad) :attach))))

(defun on-gamepad-axis-move (core-state gamepad-instance axis value)
  (let ((gamepad (get-gamepad-by-instance core-state gamepad-instance)))
    (setf (au:href (states (input-data core-state)) (list (gamepad-id gamepad) axis))
          (normalize-gamepad-axis-value axis value))))

(defun on-gamepad-button-up (core-state gamepad-instance button)
  (let ((gamepad (get-gamepad-by-instance core-state gamepad-instance)))
    (input-transition-out core-state (list (gamepad-id gamepad) button))))

(defun on-gamepad-button-down (core-state gamepad-instance button)
  (let ((gamepad (get-gamepad-by-instance core-state gamepad-instance)))
    (input-transition-in core-state (list (gamepad-id gamepad) button))))

;;; User protocol

(defun gamepad-attached-p (context gamepad-id)
  (not (member gamepad-id (detached-gamepads (input-data (core-state context))))))

(defun gamepad-button-enter-p (context gamepad-id button)
  (input-enter-p context gamepad-id button))

(defun gamepad-button-enabled-p (context gamepad-id button)
  (input-enabled-p context gamepad-id button))

(defun gamepad-button-exit-p (context gamepad-id button)
  (input-exit-p context gamepad-id button))

(defun get-gamepad-description (context gamepad-id)
  (let ((gamepad (au:href (gamepad-ids (input-data (core-state context))) gamepad-id)))
    (gamepad-description gamepad)))

(defun get-gamepad-axis (context gamepad-id stick/axis)
  (let ((states (states (input-data (core-state context)))))
    (au:href states `(,gamepad-id ,stick/axis))))
