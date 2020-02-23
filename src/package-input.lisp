(in-package #:cl-user)

(defpackage #:virality.input
  (:use #:cl)
  (:export
   #:disable-relative-motion
   #:enable-relative-motion
   #:get-gamepad-analog
   #:get-mouse-position
   #:get-window-mode
   #:get-window-size
   #:get-window-title
   #:input-enabled-p
   #:input-enter-p
   #:input-exit-p
   #:mouse-motion-relative-p
   #:on-gamepad-analog-move
   #:on-gamepad-attach
   #:on-gamepad-button-down
   #:on-gamepad-button-up
   #:on-gamepad-detach
   #:on-window-close
   #:on-window-hide
   #:on-window-maximize
   #:on-window-minimize
   #:on-window-keyboard-focus-enter
   #:on-window-keyboard-focus-exit
   #:on-window-mouse-focus-enter
   #:on-window-mouse-focus-exit
   #:on-window-move
   #:on-window-resize
   #:on-window-restore
   #:on-window-show
   #:set-window-hidden
   #:set-window-mode
   #:set-window-title
   #:set-window-size
   #:set-window-visible))
