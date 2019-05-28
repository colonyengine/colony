(in-package #:cl-user)

(defpackage #:first-light.input
  (:nicknames #:fl.input)
  (:local-nicknames (#:v2 #:box.math.vec2))
  (:use #:cl)
  (:export
   #:get-gamepad-analog
   #:get-gamepad-name
   #:get-mouse-position
   #:get-mouse-scroll
   #:handle-events
   #:input-enabled-p
   #:input-enter-p
   #:input-exit-p
   #:make-input-data
   #:prepare-gamepads
   #:shutdown-gamepads))
