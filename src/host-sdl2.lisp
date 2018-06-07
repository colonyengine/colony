(in-package :fl.host)

(defmethod initialize-host ((host (eql :sdl2)))
  (let ((flags '(:everything)))
    (unless (apply #'sdl2:was-init flags)
      (let ((flags (autowrap:mask-apply 'sdl2::sdl-init-flags flags)))
        (sdl2::check-rc (sdl2::sdl-init flags))))))

(defmethod shutdown-host ((host (eql :sdl2)))
  (let ((channel sdl2::*main-thread-channel*))
    (sdl2::sdl-quit)
    (setf sdl2::*main-thread-channel* nil
          sdl2::*lisp-message-event* nil)
    (when channel
      (sdl2::sendmsg channel nil))))

(defmethod create-window ((host (eql :sdl2)) title width height)
  (let ((flags '(:opengl)))
    (sdl2:create-window :title title :w width :h height :flags flags)))

(defmethod create-opengl-context ((host (eql :sdl2)) window major-version minor-version)
  (sdl2:gl-set-attrs :context-profile-mask sdl2-ffi::+sdl-gl-context-profile-core+
                     :context-major-version major-version
                     :context-minor-version minor-version)
  (sdl2:gl-create-context window))

(defmethod close-window ((host (eql :sdl2)) window)
  (sdl2:destroy-window window))

(defmethod get-refresh-rate ((host (eql :sdl2)) window)
  (declare (ignore window))
  (nth-value 3 (sdl2:get-current-display-mode 0)))

(defmethod redraw-window ((host (eql :sdl2)) window)
  (sdl2:gl-swap-window window))

(defmethod set-draw-mode ((host (eql :sdl2)) mode)
  (ecase mode
    (:immediate (sdl2:gl-set-swap-interval 0))
    (:sync (sdl2:gl-set-swap-interval 1))))

(defmethod get-window-title ((host (eql :sdl2)) window)
  (sdl2:get-window-title window))

(defmethod set-window-title ((host (eql :sdl2)) window title)
  (sdl2:set-window-title window title))

(defmethod get-window-size ((host (eql :sdl2)) window)
  (multiple-value-list (sdl2:get-window-size window)))

(defmethod set-window-size ((host (eql :sdl2)) window width height)
  (sdl2:set-window-size window width height))

(defmethod get-window-mode ((host (eql :sdl2)) window)
  (if (member :fullscreen-desktop (sdl2:get-window-flags window))
      :fullscreen
      :windowed))

(defmethod set-window-mode ((host (eql :sdl2)) window mode)
  (ecase mode
    (:fullscreen (sdl2:set-window-fullscreen window :desktop))
    (:windowed (sdl2:set-window-fullscreen window :windowed))))

(defmethod set-window-hidden ((host (eql :sdl2)) window)
  (sdl2:hide-window window))

(defmethod set-window-visible ((host (eql :sdl2)) window)
  (sdl2:show-window window))

(defmethod set-cursor-hidden ((host (eql :sdl2)))
  (sdl2:hide-cursor))

(defmethod set-cursor-visible ((host (eql :sdl2)))
  (sdl2:show-cursor))

(defmacro event-case ((event) &body handlers)
  `(case (sdl2:get-event-type ,event)
     ,@(au:collecting
         (dolist (handler handlers)
           (destructuring-bind (type options . body) handler
             (let ((body (list* `(declare (ignorable ,@(au:plist-values options))) body)))
               (dolist (type (au:ensure-list type))
                 (au:when-let ((x (sdl2::expand-handler event type options body)))
                   (collect x)))))))))

(defmethod dispatch-event ((host (eql :sdl2)) core-state event)
  (event-case (event)
    (:windowevent
     (:timestamp ts :window-id id :event event-type :data1 data1 :data2 data2)
     (let ((event-type (aref +window-event-names+ event-type)))
       (case event-type
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
         (:close (on-window-close core-state)))))
    (:mousebuttonup
     (:which id :timestamp ts :button button :state state :clicks clicks :x x :y y)
     (let ((button (aref +mouse-button-names+ button)))
       (on-mouse-button-up core-state button)))
    (:mousebuttondown
     (:which id :timestamp ts :button button :state state :clicks clicks :x x :y y)
     (let ((button (aref +mouse-button-names+ button)))
       (on-mouse-button-down core-state button)))
    (:mousewheel
     (:which id :timestamp ts :x x :y y)
     (unless (zerop x)
       (on-mouse-scroll-horizontal core-state x))
     (unless (zerop y)
       (on-mouse-scroll-vertical core-state y)))
    (:mousemotion
     (:which id :timestamp ts :state state :x x :y y :xrel xrel :yrel yrel)
     (on-mouse-move core-state id x y xrel yrel))
    (:keyup
     (:timestamp ts :state state :repeat repeat :keysym keysym)
     (let ((key (aref +key-names+ (sdl2:scancode-value keysym))))
       (on-key-up core-state key)))
    (:keydown
     (:timestamp ts :state state :repeat repeat :keysym keysym)
     (let ((key (aref +key-names+ (sdl2:scancode-value keysym))))
       (on-key-down core-state key)
       ;; TODO: Remove the following when we are able to consume events.
       (when (eq key :escape)
         (stop-engine core-state))))
    (:controllerdeviceadded
     (:which id :timestamp ts)
     (on-gamepad-attach core-state id))
    (:controllerdeviceremoved
     (:which id :timestamp ts)
     (on-gamepad-detach core-state id))
    (:controlleraxismotion
     (:which id :timestamp ts :axis axis :value value)
     (let ((axis (aref +gamepad-axis-names+ axis))
           (value (au:map-domain -32768 32767 0 1 value)))
       (on-gamepad-axis-move core-state id axis value)))
    (:controllerbuttonup
     (:which id :timestamp ts :button button)
     (let ((button (aref +gamepad-button-names+ button)))
       (on-gamepad-button-up core-state id button)))
    (:controllerbuttondown
     (:which id :timestamp ts :button button)
     (let ((button (aref +gamepad-button-names+ button)))
       (on-gamepad-button-down core-state id button)))))

(defmethod handle-events ((host (eql :sdl2)) core-state)
  (loop :with event = (sdl2:new-event)
        :until (zerop (sdl2:next-event event :poll))
        :do (dispatch-event host core-state event)
        :finally (sdl2:free-event event)))
