(in-package #:virality.engine)

(defclass input-data ()
  ((%gamepad-instances :reader gamepad-instances
                       :initform (u:dict))
   (%gamepad-ids :accessor gamepad-ids
                 :initform (u:dict))
   (%detached-gamepads :accessor detached-gamepads
                       :initform nil)
   (%entering :accessor entering
              :initform nil)
   (%exiting :accessor exiting
             :initform nil)
   (%states :reader states
            :initform (u:dict #'equal
                              '(:mouse :motion) (make-mouse-motion-state)
                              '(:mouse :scroll-horizontal) 0
                              '(:mouse :scroll-vertical) 0))))

(defun make-input-data ()
  (make-instance 'input-data))

(defmacro event-case ((event) &body handlers)
  (let (events)
    (dolist (handler handlers)
      (destructuring-bind (type options . body) handler
        (let ((body (list*
                     `(declare (ignorable ,@(u:plist-values options)))
                     body)))
          (dolist (type (a:ensure-list type))
            (a:when-let ((x (sdl2::expand-handler
                             event type options body)))
              (push x events))))))
    `(case (sdl2:get-event-type ,event)
       ,@(nreverse events))))

(defun dispatch-event (context event)
  (event-case (event)
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (on-window-show context))
       (:hide (on-window-hide context))
       (:move (on-window-move context :x data1 :y data2))
       (:resize (on-window-resize context :width data1 :height data2))
       (:minimize (on-window-minimize context))
       (:maximize (on-window-maximize context))
       (:restore (on-window-restore context))
       (:mouse-focus-enter (on-window-mouse-focus-enter context))
       (:mouse-focus-leave (on-window-mouse-focus-exit context))
       (:keyboard-focus-enter (on-window-keyboard-focus-enter context))
       (:keyboard-focus-leave (on-window-keyboard-focus-exit context))
       (:close (on-window-close context))))
    (:mousebuttonup
     (:button button)
     (on-mouse-button-up context (aref +mouse-button-names+ button)))
    (:mousebuttondown
     (:button button)
     (on-mouse-button-down context (aref +mouse-button-names+ button)))
    (:mousewheel
     (:x x :y y)
     (on-mouse-scroll context x y))
    (:mousemotion
     (:x x :y y :xrel dx :yrel dy)
     (on-mouse-move context x y dx dy))
    (:keyup
     (:keysym keysym)
     (on-key-up context (aref +key-names+ (sdl2:scancode-value keysym))))
    (:keydown
     (:keysym keysym)
     (on-key-down context (aref +key-names+ (sdl2:scancode-value keysym))))
    (:controllerdeviceadded
     (:which index)
     (on-gamepad-attach context index))
    (:controllerdeviceremoved
     (:which gamepad-id)
     (on-gamepad-detach context gamepad-id))
    (:controlleraxismotion
     (:which gamepad-id :axis axis :value value)
     (on-gamepad-analog-move
      context gamepad-id (aref +gamepad-axis-names+ axis) value))
    (:controllerbuttonup
     (:which gamepad-id :button button)
     (on-gamepad-button-up
      context gamepad-id (aref +gamepad-button-names+ button)))
    (:controllerbuttondown
     (:which gamepad-id :button button)
     (on-gamepad-button-down
      context gamepad-id (aref +gamepad-button-names+ button)))))

(defun perform-input-state-tasks (context)
  (declare (optimize speed))
  (let* ((input-data (input-data context))
         (states (states input-data)))
    (setf (u:href states '(:mouse :scroll-horizontal)) 0
          (u:href states '(:mouse :scroll-vertical)) 0)
    (enable-entering input-data)
    (disable-exiting input-data)))

(defun handle-events (context)
  (declare (optimize speed))
  (perform-input-state-tasks context)
  (loop :with event = (sdl2:new-event)
        :until (zerop (the fixnum (sdl2:next-event event :poll)))
        :do (dispatch-event context event)
        :finally (sdl2:free-event event)))
