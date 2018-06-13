(in-package :fl.core)

(defstruct button-state
  (enabled nil)
  (enter nil)
  (leave nil))

(defstruct mouse-motion-state
  (position (v2i:make 0 0))
  (delta (v2i:make 0 0)))

(defstruct gamepad
  (id nil)
  (name nil)
  (handle nil))

(defclass input-data ()
  ((%attached-gamepads :reader attached-gamepads
                       :initform (au:dict #'eq))
   (%detached-gamepads :accessor detached-gamepads
                       :initform nil)
   (%states :reader states
            :initform (au:dict #'equal))))

(defun make-input-data ()
  (make-instance 'input-data))

;;; Input events

(defun on-mouse-button-up (core-state button)
  (declare (ignore core-state button)))

(defun on-mouse-button-down (core-state button)
  (declare (ignore core-state button)))

(defun on-mouse-scroll-vertical (core-state amount)
  (declare (ignore core-state amount)))

(defun on-mouse-scroll-horizontal (core-state amount)
  (declare (ignore core-state amount)))

(defun on-mouse-move (core-state x y dx dy)
  (declare (ignore core-state x y dx dy)))

(defun on-key-up (core-state key)
  (declare (ignore core-state key)))

(defun on-key-down (core-state key)
  ;; TODO: Remove this later when possible.
  (when (eq key :escape)
    (stop-engine core-state)))

(defun on-gamepad-attach (core-state gamepad-id gamepad-handle)
  (let* ((attached (attached-gamepads (input-data core-state)))
         (gamepad-name (get-gamepad-name core-state)))
    (setf (au:href attached gamepad-id)
          (make-gamepad :id gamepad-id :name gamepad-name :handle gamepad-handle))))

(defun on-gamepad-detach (core-state gamepad-id)
  (let* ((attached (attached-gamepads (input-data core-state)))
         (gamepad (au:href attached gamepad-id)))
    (fl.host:close-gamepad (host core-state) (gamepad-handle gamepad))
    (au:appendf (detached-gamepads (input-data core-state)) (list (gamepad-name gamepad)))
    (remhash gamepad-id attached)))

(defun on-gamepad-axis-move (core-state gamepad-id axis value))

(defun on-gamepad-button-up (core-state gamepad-id button)
  (declare (ignore core-state gamepad-id button)))

(defun on-gamepad-button-down (core-state gamepad-id button)
  (format t "~s: ~s~%" (get-gamepad-from-id core-state gamepad-id) button))

;;; Gamepad management

(defun get-gamepad-name (core-state)
  (with-slots (%attached-gamepads %detached-gamepads) (input-data core-state)
    (or (pop %detached-gamepads)
        (au:format-symbol :keyword "GAMEPAD~d" (1+ (hash-table-count %attached-gamepads))))))

(defun get-gamepad-from-id (core-state gamepad-id)
  (let ((gamepad (au:href (attached-gamepads (input-data core-state)) gamepad-id)))
    (sdl2::game-controller-name (gamepad-handle gamepad))))

(defun shutdown-gamepads (core-state)
  (let ((attached (attached-gamepads (input-data core-state))))
    (au:do-hash-values (v attached)
      (fl.host:close-gamepad (host core-state) (gamepad-handle v)))
    (clrhash attached)))

(defun load-gamepad-database ()
  (sdl2:game-controller-add-mappings-from-file
   (namestring
    (uiop/pathname:merge-pathnames*
     (get-extension-path)
     (make-pathname :defaults *default-pathname-defaults* :name "gamepad-db" :type "txt")))))
