(in-package :fl.core)

(defclass input-data ()
  ((%active-gamepads :reader active-gamepads
                     :initform (au:dict #'eq))
   (%attached-gamepads :accessor attached-gamepads
                       :initform (au:dict #'eq))
   (%detached-gamepads :accessor detached-gamepads
                       :initform nil)
   (%buttons-entering :accessor buttons-entering
                      :initform nil)
   (%buttons-leaving :accessor buttons-leaving
                     :initform nil)
   (%states :reader states
            :initform (au:dict #'equal
                               '(:mouse . :motion) (make-mouse-motion-state)
                               '(:mouse . :scroll-horizontal) 0
                               '(:mouse . :scroll-vertical) 0))))

(defun make-input-data ()
  (make-instance 'input-data))

(defmacro event-case ((event) &body handlers)
  `(case (sdl2:get-event-type ,event)
     ,@(au:collecting
         (dolist (handler handlers)
           (destructuring-bind (type options . body) handler
             (let ((body (list* `(declare (ignorable ,@(au:plist-values options))) body)))
               (dolist (type (au:ensure-list type))
                 (au:when-let ((x (sdl2::expand-handler event type options body)))
                   (collect x)))))))))

(defun dispatch-event (core-state event)
  (event-case (event)
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (on-window-show core-state))
       (:hide (on-window-hide core-state))
       (:move (on-window-move core-state :x data1 :y data2))
       (:resize (on-window-resize core-state :width data1 :height data2))
       (:minimize (on-window-minimize core-state))
       (:maximize (on-window-maximize core-state))
       (:restore (on-window-restore core-state))
       (:mouse-focus-enter (on-window-mouse-focus-enter core-state))
       (:mouse-focus-leave (on-window-mouse-focus-leave core-state))
       (:keyboard-focus-enter (on-window-keyboard-focus-enter core-state))
       (:keyboard-focus-leave (on-window-keyboard-focus-leave core-state))
       (:close (on-window-close core-state))))
    (:mousebuttonup
     (:button button)
     (on-mouse-button-up core-state (aref +mouse-button-names+ button)))
    (:mousebuttondown
     (:button button)
     (on-mouse-button-down core-state (aref +mouse-button-names+ button)))
    (:mousewheel
     (:x x :y y)
     (on-mouse-scroll core-state x y))
    (:mousemotion
     (:x x :y y :xrel dx :yrel dy)
     (on-mouse-move core-state x y dx dy))
    (:keyup
     (:keysym keysym)
     (on-key-up core-state (aref +key-names+ (sdl2:scancode-value keysym))))
    (:keydown
     (:keysym keysym)
     (on-key-down core-state (aref +key-names+ (sdl2:scancode-value keysym))))
    (:controllerdeviceadded
     (:which index)
     (on-gamepad-attach core-state index))
    (:controllerdeviceremoved
     (:which gamepad-id)
     (on-gamepad-detach core-state gamepad-id))
    (:controlleraxismotion
     (:which gamepad-id :axis axis :value value)
     (on-gamepad-axis-move core-state gamepad-id (aref +gamepad-axis-names+ axis) value))
    (:controllerbuttonup
     (:which gamepad-id :button button)
     (on-gamepad-button-up core-state gamepad-id (aref +gamepad-button-names+ button)))
    (:controllerbuttondown
     (:which gamepad-id :button button)
     (on-gamepad-button-down core-state gamepad-id (aref +gamepad-button-names+ button)))))

(defun perform-input-state-tasks (core-state)
  (let ((states (au:href (states (input-data core-state)))))
    (setf (au:href states (cons :mouse :scroll-horizontal)) 0
          (au:href states (cons :mouse :scroll-vertical)) 0)
    (enable-entering-buttons core-state)
    (disable-leaving-buttons core-state)))

(defun handle-events (core-state)
  (perform-input-state-tasks core-state)
  (loop :with event = (sdl2:new-event)
        :until (zerop (sdl2:next-event event :poll))
        :do (dispatch-event core-state event)
        :finally (sdl2:free-event event)))
