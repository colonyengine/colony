(in-package #:cl-user)

(defpackage #:first-light.actions
  (:nicknames #:fl.actions)
  (:local-nicknames (#:v3 #:box.math.vec3))
  (:use #:cl #:%first-light)
  ;; protocol
  (:export
   #:insert-action
   #:make-action-manager
   #:process-actions
   #:remove-action)
  ;; built-in actions
  (:export
   #:fade-in
   #:fade-out
   #:rotate
   #:rotate/reverse
   #:sprite-animate))
