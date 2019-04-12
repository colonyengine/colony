(in-package :first-light.input)

(defclass input-data ()
  ((%gamepad-instances :reader gamepad-instances
                       :initform (au:dict #'eq))
   (%gamepad-ids :accessor gamepad-ids
                 :initform (au:dict #'eq))
   (%detached-gamepads :accessor detached-gamepads
                       :initform nil)
   (%entering :accessor entering
              :initform nil)
   (%exiting :accessor exiting
             :initform nil)
   (%states :reader states
            :initform (au:dict #'equal
                               '(:mouse :motion) (make-mouse-motion-state)
                               '(:mouse :scroll-horizontal) 0
                               '(:mouse :scroll-vertical) 0))))

(defun make-input-data ()
  (make-instance 'input-data))

(defmacro event-case ((event) &body handlers)
  `(case (sdl2:get-event-type ,event)
     ,@(au:collecting
         (dolist (handler handlers)
           (destructuring-bind (type options . body) handler
             (let ((body (list*
                          `(declare (ignorable ,@(au:plist-values options)))
                          body)))
               (dolist (type (au:ensure-list type))
                 (au:when-let ((x (sdl2::expand-handler
                                   event type options body)))
                   (collect x)))))))))

(defun dispatch-event (input-data event)
  (event-case (event)
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (on-window-show input-data))
       (:hide (on-window-hide input-data))
       (:move (on-window-move input-data :x data1 :y data2))
       (:resize (on-window-resize input-data :width data1 :height data2))
       (:minimize (on-window-minimize input-data))
       (:maximize (on-window-maximize input-data))
       (:restore (on-window-restore input-data))
       (:mouse-focus-enter (on-window-mouse-focus-enter input-data))
       (:mouse-focus-leave (on-window-mouse-focus-exit input-data))
       (:keyboard-focus-enter (on-window-keyboard-focus-enter input-data))
       (:keyboard-focus-leave (on-window-keyboard-focus-exit input-data))
       (:close (on-window-close input-data))))
    (:mousebuttonup
     (:button button)
     (on-mouse-button-up input-data (aref +mouse-button-names+ button)))
    (:mousebuttondown
     (:button button)
     (on-mouse-button-down input-data (aref +mouse-button-names+ button)))
    (:mousewheel
     (:x x :y y)
     (on-mouse-scroll input-data x y))
    (:mousemotion
     (:x x :y y :xrel dx :yrel dy)
     (on-mouse-move input-data x y dx dy))
    (:keyup
     (:keysym keysym)
     (on-key-up input-data (aref +key-names+ (sdl2:scancode-value keysym))))
    (:keydown
     (:keysym keysym)
     (on-key-down input-data (aref +key-names+ (sdl2:scancode-value keysym))))
    (:controllerdeviceadded
     (:which index)
     (on-gamepad-attach input-data index))
    (:controllerdeviceremoved
     (:which gamepad-id)
     (on-gamepad-detach input-data gamepad-id))
    (:controlleraxismotion
     (:which gamepad-id :axis axis :value value)
     (on-gamepad-analog-move
      input-data gamepad-id (aref +gamepad-axis-names+ axis) value))
    (:controllerbuttonup
     (:which gamepad-id :button button)
     (on-gamepad-button-up
      input-data gamepad-id (aref +gamepad-button-names+ button)))
    (:controllerbuttondown
     (:which gamepad-id :button button)
     (on-gamepad-button-down
      input-data gamepad-id (aref +gamepad-button-names+ button)))))

(defun perform-input-state-tasks (input-data)
  (declare (optimize speed))
  (let ((states (au:href (states input-data))))
    (setf (au:href states '(:mouse :scroll-horizontal)) 0
          (au:href states '(:mouse :scroll-vertical)) 0)
    (enable-entering input-data)
    (disable-exiting input-data)))

(defun handle-events (input-data)
  (declare (optimize speed))
  (perform-input-state-tasks input-data)
  (loop :with event = (sdl2:new-event)
        :until (zerop (the fixnum (sdl2:next-event event :poll)))
        :do (dispatch-event input-data event)
        :finally (sdl2:free-event event)))
