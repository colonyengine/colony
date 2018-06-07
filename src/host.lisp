(in-package :fl.host)

;;; Constants

(au:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore :mouse-focus-enter
      :mouse-focus-leave :keyboard-focus-enter :keyboard-focus-leave :close nil nil)
  :test #'equalp)

(au:define-constant +key-names+
    #(:unknown nil nil nil
      :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z :1 :2 :3 :4 :5
      :6 :7 :8 :9 :0 :return :escape :backspace :tab :space :minus :equals :leftbracket
      :rightbracket :backslash :nonushash :semicolon :apostrophe :grave :comma :period :slash
      :capslock :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :printscreen :scrolllock :pause
      :insert :home :pageup :delete :end :pagedown :right :left :down :up :numlockclear :kp_divide
      :kp_multiply :kp_minus :kp_plus :kp_enter :kp_1 :kp_2 :kp_3 :kp_4 :kp_5 :kp_6 :kp_7 :kp_8
      :kp_9 :kp_0 :kp_period :nonusbackslash :application :power :kp_equals :f13 :f14 :f15 :f16
      :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :execute :help :menu :select :stop :again :undo :cut
      :copy :paste :find :mute :volumeup :volumedown :lockingcapslock :lockingnumlock
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

(au:define-constant +gamepad-axis-names+
    #(nil :left-x :left-y :right-x :right-y :trigger-x :trigger-y nil)
  :test #'equalp)

(au:define-constant +gamepad-button-names+
    #(nil :a :b :x :y :back :guide :start :left-stick :right-stick :left-shoulder :right-shoulder
      :dpad-up :dpad-down :dpad-left :dpad-right nil)
  :test #'equalp)

;;; Protocol

(defgeneric initialize-host (host)
  (:method (host)
    (error "Host ~s does not implement INITIALIZE-HOST." host))
  (:documentation "Perform any steps needed to initialize the host system."))

(defgeneric shutdown-host (host)
  (:method (host)
    (error "Host ~s does not implement SHUTDOWN-HOST." host))
  (:documentation "Perform any steps needed to shutdown the host system."))

(defgeneric create-window (host title width height)
  (:method (host title width height)
    (error "Host ~s does not implement CREATE-WINDOW." host))
  (:documentation "Create a display window."))

(defgeneric create-opengl-context (host window major-version minor-version)
  (:method (host window major-version minor-version)
    (error "Host ~s does not implement CREATE-OPENGL-CONTEXT." host))
  (:documentation "Create an OpenGL context for a host window."))

(defgeneric close-window (host window)
  (:method (host window)
    (error "Host ~s does not implement CLOSE-WINDOW." host))
  (:documentation "Close a display window."))

(defgeneric get-refresh-rate (host window)
  (:method (host window)
    (error "Host ~s does not implement GET-REFRESH-RATE." host))
  (:documentation "Request the display's refresh rate."))

(defgeneric redraw-window (host window)
  (:method (host window)
    (error "Host ~s does not implement REDRAW-WINDOW." host))
  (:documentation "Request the host to redraw the display window."))

(defgeneric set-draw-mode (host mode)
  (:method (host mode)
    (error "Host ~s does not implement SET-DRAW-MODE." host))
  (:documentation "Specify whether to draw immediately or synced with the refresh rate."))

(defgeneric get-window-title (host window)
  (:method (host window)
    (error "Host ~s does not implement GET-WINDOW-TITLE." host))
  (:documentation "Get the title of a display window."))

(defgeneric set-window-title (host window title)
  (:method (host window title)
    (error "Host ~s does not implement SET-WINDOW-TITLE." host))
  (:documentation "Set the title of a display window."))

(defgeneric get-window-size (host window)
  (:method (host window)
    (error "Host ~s does not implement GET-WINDOW-SIZE." host))
  (:documentation "Get the size of a display window, as a list of 2 elements."))

(defgeneric set-window-size (host window width height)
  (:method (host window width height)
    (error "Host ~s does not implement SET-WINDOW-SIZE." host))
  (:documentation "Request the host to resize a window."))

(defgeneric get-window-mode (host window)
  (:method (host window)
    (error "Host ~s does not implement GET-WINDOW-MODE." host))
  (:documentation "Check if a window is in fullscreen or windowed mode."))

(defgeneric set-window-mode (host window mode)
  (:method (host window mode)
    (error "Host ~s does not implement SET-WINDOW-MODE." host))
  (:documentation "Request the host to put a display window into fullscreen or windowed mode."))

(defgeneric set-window-hidden (host window)
  (:method (host window)
    (error "Host ~s does not implement SET-WINDOW-HIDDEN." host))
  (:documentation "Request the host to hide a display window."))

(defgeneric set-window-visible (host window)
  (:method (host window)
    (error "Host ~s does not implement SET-WINDOW-VISIBLE." host))
  (:documentation "Request the host to unhide a display window."))

(defgeneric set-cursor-hidden (host)
  (:method (host)
    (error "Host ~s does not implement SET-CURSOR-HIDDEN." host))
  (:documentation "Request the host to hide the cursor."))

(defgeneric set-cursor-visible (host)
  (:method (host)
    (error "Host ~s does not implement SET-CURSOR-VISIBLE." host))
  (:documentation "Request the host to unhide the cursor."))

(defgeneric on-event (host event core-state)
  (:method (host event core-state)
    (error "Host ~s does not implement ON-EVENT." host))
  (:documentation "Dispatch events."))

(defgeneric handle-events (host core-state)
  (:method (host core-state)
    (error "Host ~s does not implement HANDLE-EVENTS." host))
  (:documentation "Handle the host's events."))
