(in-package :fl.host)

(au:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore :mouse-focus-enter
      :mouse-focus-leave :keyboard-focus-enter :keyboard-focus-leave :close nil nil)
  :test #'equalp)

(au:define-constant +key-names+
    #(:unknown nil nil nil :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y
      :z :1 :2 :3 :4 :5 :6 :7 :8 :9 :0 :return :escape :backspace :tab :space :minus :equals
      :leftbracket :rightbracket :backslash :nonushash :semicolon :apostrophe :grave :comma :period
      :slash :capslock :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :printscreen :scrolllock
      :pause :insert :home :pageup :delete :end :pagedown :right :left :down :up :numlockclear
      :kp_divide :kp_multiply :kp_minus :kp_plus :kp_enter :kp_1 :kp_2 :kp_3 :kp_4 :kp_5 :kp_6 :kp_7
      :kp_8 :kp_9 :kp_0 :kp_period :nonusbackslash :application :power :kp_equals :f13 :f14 :f15
      :f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :execute :help :menu :select :stop :again :undo
      :cut :copy :paste :find :mute :volumeup :volumedown :lockingcapslock :lockingnumlock
      :lockingscrolllock :kp_comma :kp_equalsas400 :international1 :international2 :international3
      :international4 :international5 :international6 :international7 :international8
      :international9 :lang1 :lang2 :lang3 :lang4 :lang5 :lang6 :lang7 :lang8 :lang9 :alterase
      :sysreq :cancel :clear :prior :return2 :separator :out :oper :clearagain :crsel :exsel nil
      nil nil nil nil nil nil nil nil nil nil :kp_00 :kp_000 :thousandsseparator :decimalseparator
      :currencyunit :currencysubunit :kp_leftparen :kp_rightparen :kp_leftbrace :kp_rightbrace
      :kp_tab :kp_backspace :kp_a :kp_b :kp_c :kp_d :kp_e :kp_f :kp_xor :kp_power :kp_percent
      :kp_less :kp_greater :kp_ampersand :kp_dblampersand :kp_verticalbar :kp_dblverticalbar
      :kp_colon :kp_hash :kp_space :kp_at :kp_exclam :kp_memstore :kp_memrecall :kp_memclear
      :kp_memadd :kp_memsubtract :kp_memmultiply :kp_memdivide :kp_plusminus :kp_clear
      :kp_clearentry :kp_binary :kp_octal :kp_decimal :kp_hexadecimal nil nil :lctrl :lshift :lalt
      :lgui :rctrl :rshift :ralt :rgui nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil nil nil :mode :audionext :audioprev :audiostop :audioplay
      :audiomute :mediaselect :www :mail :calculator :computer :ac_search :ac_home :ac_back
      :ac_forward :ac_stop :ac_refresh :ac_bookmarks :brightnessdown :brightnessup :displayswitch
      :kbdillumtoggle :kbdillumdown :kbdillumup :eject :sleep)
  :test #'equalp)

(au:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(au:define-constant +gamepad-device-names+
    #(:player1 :player2 :player3 :player4 :player5 :player6 :player7 :player8)
  :test #'equalp)

(au:define-constant +gamepad-axis-names+
    #(nil :left-horizontal :left-vertical :right-horizontal :right-vertical :trigger-left
      :trigger-right nil)
  :test #'equalp)

(au:define-constant +gamepad-button-names+
    #(nil :a :b :x :y :back :guide :start :left-stick :right-stick :left-shoulder :right-shoulder :up
      :down :left :right nil)
  :test #'equalp)

(defgeneric initialize-host (host)
  (:method (host)
    (error "Host ~s does not implement INITIALIZE-HOST." host))
  (:method ((host (eql :sdl2)))
    (let ((flags '(:everything)))
      (unless (apply #'sdl2:was-init flags)
        (let ((flags (autowrap:mask-apply 'sdl2::sdl-init-flags flags)))
          (sdl2::check-rc (sdl2::sdl-init flags)))))))

(defgeneric shutdown-host (host)
  (:method (host)
    (error "Host ~s does not implement SHUTDOWN-HOST." host))
  (:method ((host (eql :sdl2)))
    (let ((channel sdl2::*main-thread-channel*))
      (sdl2::sdl-quit)
      (setf sdl2::*main-thread-channel* nil
            sdl2::*lisp-message-event* nil)
      (when channel
        (sdl2::sendmsg channel nil)))))

(defgeneric create-window (host title width height)
  (:method (host window major-version minor-version)
    (error "Host ~s does not implement CREATE-WINDOW." host))
  (:method ((host (eql :sdl2)) title width height)
    (let ((flags '(:opengl)))
      (sdl2:create-window :title title :w width :h height :flags flags))))

(defgeneric create-opengl-context (host window major-version minor-version)
  (:method (host window major-version minor-version)
    (error "Host ~s does not implement CREATE-OPENGL-CONTEXT." host))
  (:method ((host (eql :sdl2)) window major-version minor-version)
    (sdl2:gl-set-attrs :context-profile-mask sdl2-ffi::+sdl-gl-context-profile-core+
                       :context-major-version major-version
                       :context-minor-version minor-version)
    (sdl2:gl-create-context window)))

(defgeneric close-window (host window)
  (:method (host window)
    (error "Host ~s does not implement CLOSE-WINDOW." host))
  (:method ((host (eql :sdl2)) window)
    (sdl2:destroy-window window)))

(defgeneric get-refresh-rate (host window)
  (:method (host window)
    (error "Host ~s does not implement GET-REFRESH-RATE." host))
  (:method ((host (eql :sdl2)) window)
    (declare (ignore window))
    (nth-value 3 (sdl2:get-current-display-mode 0))))

(defgeneric redraw-window (host window)
  (:method (host window)
    (error "Host ~s does not implement REDRAW-WINDOW." host))
  (:method ((host (eql :sdl2)) window)
    (sdl2:gl-swap-window window)))

(defgeneric set-draw-mode (host mode)
  (:method (host mode)
    (error "Host ~s does not implement SET-DRAW-MODE." host))
  (:method ((host (eql :sdl2)) mode)
    (ecase mode
      (:immediate (sdl2:gl-set-swap-interval 0))
      (:sync (sdl2:gl-set-swap-interval 1)))))

(defgeneric get-window-title (host window)
  (:method (host window)
    (error "Host ~s does not implement GET-WINDOW-TITLE." host))
  (:method ((host (eql :sdl2)) window)
    (sdl2:get-window-title window)))

(defgeneric set-window-title (host window title)
  (:method (host window title)
    (error "Host ~s does not implement SET-WINDOW-TITLE." host))
  (:method ((host (eql :sdl2)) window title)
    (sdl2:set-window-title window title)))

(defgeneric get-window-size (host window)
  (:method (host window)
    (error "Host ~s does not implement GET-WINDOW-SIZE." host))
  (:method ((host (eql :sdl2)) window)
    (multiple-value-list (sdl2:get-window-size window))))

(defgeneric set-window-size (host window width height)
  (:method (host window width height)
    (error "Host ~s does not implement SET-WINDOW-SIZE." host))
  (:method (host window width height)
    (sdl2:set-window-size window width height)))

(defgeneric get-window-mode (host window)
  (:method (host window)
    (error "Host ~s does not implement GET-WINDOW-MODE." host))
  (:method ((host (eql :sdl2)) window)
    (if (member :fullscreen-desktop (sdl2:get-window-flags window))
        :fullscreen
        :windowed)))

(defgeneric set-window-mode (host window mode)
  (:method (host window mode)
    (error "Host ~s does not implement SET-WINDOW-MODE." host))
  (:method ((host (eql :sdl2)) window mode)
    (ecase mode
      (:fullscreen (sdl2:set-window-fullscreen window :desktop))
      (:windowed (sdl2:set-window-fullscreen window :windowed)))))

(defgeneric set-window-hidden (host window)
  (:method (host window)
    (error "Host ~s does not implement SET-WINDOW-HIDDEN." host))
  (:method ((host (eql :sdl2)) window)
    (sdl2:hide-window window)))

(defgeneric set-window-visible (host window)
  (:method (host window)
    (error "Host ~s does not implement SET-WINDOW-VISIBLE." host))
  (:method ((host (eql :sdl2)) window)
    (sdl2:show-window window)))

(defgeneric set-cursor-hidden (host)
  (:method (host)
    (error "Host ~s does not implement SET-CURSOR-HIDDEN." host))
  (:method ((host (eql :sdl2)))
    (sdl2:hide-cursor)))

(defgeneric set-cursor-visible (host)
  (:method (host)
    (error "Host ~s does not implement SET-CURSOR-VISIBLE." host))
  (:method ((host (eql :sdl2)))
    (sdl2:show-cursor)))

(defmacro event-case ((event) &body handlers)
  `(case (sdl2:get-event-type ,event)
     ,@(au:collecting
         (dolist (handler handlers)
           (destructuring-bind (type options . body) handler
             (let ((body (list* `(declare (ignorable ,@(au:plist-values options))) body)))
               (dolist (type (au:ensure-list type))
                 (au:when-let ((x (sdl2::expand-handler event type options body)))
                   (collect x)))))))))

(defgeneric dispatch-event (host core-state event)
  (:method (host event core-state)
    (error "Host ~s does not implement DISPATCH-EVENT." host))
  (:method ((host (eql :sdl2)) core-state event)
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
       (:which device-id :timestamp ts :button button :state state :clicks clicks :x x :y y)
       (let ((button (aref +mouse-button-names+ button)))
         (on-mouse-button-up core-state button)))
      (:mousebuttondown
       (:which device-id :timestamp ts :button button :state state :clicks clicks :x x :y y)
       (let ((button (aref +mouse-button-names+ button)))
         (on-mouse-button-down core-state button)))
      (:mousewheel
       (:which device-id :timestamp ts :x x :y y)
       (unless (zerop x)
         (on-mouse-scroll-horizontal core-state x))
       (unless (zerop y)
         (on-mouse-scroll-vertical core-state y)))
      (:mousemotion
       (:which device-id :timestamp ts :state state :x x :y y :xrel xrel :yrel yrel)
       (on-mouse-move core-state x y xrel yrel))
      (:keyup
       (:timestamp ts :state state :repeat repeat :keysym keysym)
       (let ((key (aref +key-names+ (sdl2:scancode-value keysym))))
         (on-key-up core-state key)))
      (:keydown
       (:timestamp ts :state state :repeat repeat :keysym keysym)
       (let ((key (aref +key-names+ (sdl2:scancode-value keysym))))
         (on-key-down core-state key)))
      (:controllerdeviceadded
       (:which device-id :timestamp ts)
       (let ((device-id (aref +gamepad-device-names+ device-id)))
         (on-gamepad-attach core-state device-id)))
      (:controllerdeviceremoved
       (:which device-id :timestamp ts)
       (let ((device-id (aref +gamepad-device-names+ device-id)))
         (on-gamepad-detach core-state device-id)))
      (:controlleraxismotion
       (:which device-id :timestamp ts :axis axis :value value)
       (let* ((device-id (aref +gamepad-device-names+ device-id))
              (axis (aref +gamepad-axis-names+ axis))
              (value (case axis
                       ((:trigger-left :trigger-right)
                        (au:map-domain 0 32767 0 1 value))
                       (t
                        (au:map-domain -32768 32767 -1 1 value)))))
         (on-gamepad-axis-move core-state device-id axis value)))
      (:controllerbuttonup
       (:which device-id :timestamp ts :button button)
       (let ((device-id (aref +gamepad-device-names+ device-id))
             (button (aref +gamepad-button-names+ button)))
         (on-gamepad-button-up core-state device-id button)))
      (:controllerbuttondown
       (:which device-id :timestamp ts :button button)
       (let ((device-id (aref +gamepad-device-names+ device-id))
             (button (aref +gamepad-button-names+ button)))
         (on-gamepad-button-down core-state device-id button))))))

(defgeneric handle-events (host core-state)
  (:method (host core-state)
    (error "Host ~s does not implement HANDLE-EVENTS." host))
  (:method ((host (eql :sdl2)) core-state)
    (loop :with event = (sdl2:new-event)
          :until (zerop (sdl2:next-event event :poll))
          :do (dispatch-event host core-state event)
          :finally (sdl2:free-event event))))
