(in-package :fl.core)

(au:define-constant +gamepad-axis-names+
    #(:left-horizontal :left-vertical :right-horizontal :right-vertical :trigger-left
      :trigger-right)
  :test #'equalp)

(au:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick :right-stick :left-shoulder :right-shoulder :up
      :down :left :right)
  :test #'equalp)

(defstruct gamepad id name description handle)

;;; Utility functions

(defun get-gamepad-by-id (core-state gamepad-id)
  (au:href (attached-gamepads (input-data core-state)) gamepad-id))

(defun generate-gamepad-name (core-state)
  (with-slots (%attached-gamepads %detached-gamepads) (input-data core-state)
    (or (pop %detached-gamepads)
        (au:format-symbol :keyword "GAMEPAD~d" (1+ (hash-table-count %attached-gamepads))))))

(defun shutdown-gamepads (core-state)
  (let ((attached (attached-gamepads (input-data core-state))))
    (au:do-hash-values (v attached)
      (sdl2:game-controller-close (gamepad-handle v)))
    (clrhash attached)))

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
           (id (sdl2:game-controller-instance-id handle))
           (attached (attached-gamepads (input-data core-state))))
      (setf (au:href attached id)
            (make-gamepad :id id
                          :name (generate-gamepad-name core-state)
                          :description (sdl2:game-controller-name handle)
                          :handle handle)))))

(defun on-gamepad-detach (core-state gamepad-id)
  (let* ((attached (attached-gamepads (input-data core-state)))
         (gamepad (au:href attached gamepad-id)))
    (sdl2:game-controller-close (gamepad-handle gamepad))
    (au:appendf (detached-gamepads (input-data core-state)) (list (gamepad-name gamepad)))
    (remhash gamepad-id attached)))

(defun on-gamepad-axis-move (core-state gamepad-id axis value)
  (let* ((gamepad (get-gamepad-by-id core-state gamepad-id))
         (state (cons (gamepad-name gamepad) axis)))
    (setf (au:href (states (input-data core-state)) state)
          (normalize-gamepad-axis-value axis value))))

(defun on-gamepad-button-up (core-state gamepad-id button)
  (let ((gamepad (get-gamepad-by-id core-state gamepad-id)))
    (button-transition-out core-state (cons (gamepad-name gamepad) button))))

(defun on-gamepad-button-down (core-state gamepad-id button)
  (let ((gamepad (get-gamepad-by-id core-state gamepad-id)))
    (button-transition-in core-state (cons (gamepad-name gamepad) button))))

;;; User protocol

(defun gamepad-attached-p (context gamepad-id)
  (not (member gamepad-id (detached-gamepads (input-data (core-state context))))))

(defun gamepad-button-enter-p (context gamepad-id button)
  (button-state-enter-p context gamepad-id button))

(defun gamepad-button-enabled-p (context gamepad-id button)
  (button-state-enabled-p context gamepad-id button))

(defun gamepad-button-leave-p (context gamepad-id button)
  (button-state-leave-p context gamepad-id button))

(defun get-gamepad-axis (context gamepad-id stick/axis)
  (let ((states (states (input-data (core-state context)))))
    (au:href states (cons gamepad-id stick/axis))))
